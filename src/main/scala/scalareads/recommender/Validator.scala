package scalareads.recommender

import scalareads.{ReadBook, ToReadBook, User}
import scalareads.values.GEnvironment
import NearestNeighborFunctions._

/**
 * Create a validator for recommender.
 * Get predicted ratings for read books, compare to actual ratings.
 */

object Validator {

  case class ValidatorPredictions(b: BookPrediction, actualUserRating: Option[Int])

  val predicts = (env: GEnvironment, u: User) => {
     for {
      b <- u.readBooks(env)
      booksAndUserRating = b.map(rb => (rb, rb.userRating))
    } yield {
       val (test, train) = booksAndUserRating.splitAt(b.length/2)

       val toReadShelves: List[(ReadBook, List[Tag])] = train.map(trb => (trb._1, trb._1.popularShelves.takeRight(50).map(_._1)))


       //scaling based on training set
       val minMaxes: Map[ReadBook, Map[Tag, MinMax]] = {

        //for each read book, need to compare it to each trb's shelves; measure shelfishness compared to each one
        val shelfishnesses: List[(ReadBook, UnscaledShelfishness)] = train.flatMap(rb => {
          toReadShelves.map(tup => (tup._1, rb._1.measureShelfishness(tup._2)))
        })
        val justTagsAndShelfishnesses = shelfishnesses.map(s => (s._1, s._2.tagsAndShelfishness))

        justTagsAndShelfishnesses.map(tup => (tup._1, NearestNeighborFunctions.shelfishnessScaling(tup._2))).toMap
       }


       val testSet = {
         //for each trb measure its shelfishness, get correct minmaxes

         val mms: List[(Option[Map[Tag, MinMax]], ReadBook)] = test.map(trb => minMaxes.get(trb._1)).zip(test.map(_._1))
         val noNones: List[(ReadBook, Map[Tag, MinMax])] = mms.map(tup => tup._1 match {
           case None => (tup._2, Map.empty[Tag, MinMax])
           case Some(x) => (tup._2, x)
         })

         val unscaledShelfishnesses: List[(UnscaledShelfishness, Map[Tag, MinMax])] = noNones.map(trb => {
           val trbTags = trb._2.toList.map(_._1)

           (trb._1.measureShelfishness(trbTags), trb._2)
         })

         unscaledShelfishnesses.map(tup => scaleShelfishness(tup._1, tup._2))
       }

       println("validator test set: " + testSet)
       val trainSet = {

         val unscaledS: List[(UnscaledShelfishness, Map[Tag, MinMax])] = minMaxes.toList.flatMap(tup => {
           val trbTags = tup._2.toList.map(_._1)

           train.map(rb => (rb._1.measureShelfishness(trbTags), tup._2))
         })

         unscaledS.map(tup => scaleShelfishness(tup._1, tup._2))
       }


       val predicted: List[BookPrediction] = NearestNeighborFunctions.predictRatings(testSet, trainSet)

       for {
         p <- predicted
         t <- test
         if p.b == t._1.simpleBook
       } yield ValidatorPredictions(p, t._2)
    }
  }

  def meanSquaredError(ps: List[ValidatorPredictions]) = {
    val filtered = ps.filter(vp => vp.actualUserRating.nonEmpty).map(vp => (vp.b, vp.actualUserRating.get))
    math.sqrt(filtered.map(vp => math.pow(vp._2.toDouble - vp._1.predictedRating, 2)).fold(0.0)((a, b) => a + b)/filtered.length)
  }
}
