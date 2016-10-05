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

       val toReadShelves2 = {
         val sorted50 = train.flatMap(trb => trb._1.popularShelves).groupBy(_._1).mapValues(l =>
           l.foldRight(0)((tup, i) => tup._2 + i)).toList.sortBy(_._2).takeRight(50)
         sorted50.foreach(println)
         sorted50.map(_._1)
       }

       val unwantedTags = List(Tag("epic-fantasy"), Tag("fantasy-sci-fi"), Tag("default"),
         Tag("zombies"), Tag("tolkien"), Tag("dystopia"), Tag("memoir"), Tag("urban-fantasy"),
          Tag("literature"), Tag("novels"), Tag("contemporary"), Tag("young-adult"),
         Tag("books-i-own"), Tag("fiction"), Tag("adventure"), Tag("audiobook"))

//       toReadShelves2.foreach(t => println(t))
      // lowest mse with unwantedTags filtered out: 1.0480689803493954
       val toReadShelves3 = toReadShelves2.filterNot(unwantedTags.contains)

       //scaling based on training set
       val minMaxes: Map[Tag, MinMax] = {
         //for each read book, need to compare it to each trb's shelves; measure shelfishness compared to each one
         val shelfishnesses: List[UnscaledShelfishness] = train.map(rb => {
           rb._1.measureShelfishness(toReadShelves3)
         })
         val justTagsAndShelfishnesses = shelfishnesses.flatMap(s => s.tagsAndShelfishness)

         NearestNeighborFunctions.shelfishnessScaling(justTagsAndShelfishnesses)
       }


       val testSet =
         test.map(trb => {
           scaleShelfishness(trb._1.measureShelfishness(toReadShelves3), minMaxes)
         })

       val trainSet =
         train.map(rb => {
           scaleShelfishness(rb._1.measureShelfishness(toReadShelves3), minMaxes)
         })

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
