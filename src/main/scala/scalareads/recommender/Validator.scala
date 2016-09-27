package scalareads.recommender

import scalareads.User
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
       val toReadShelves: List[Tag] = train.flatMap(trb => trb._1.popularShelves.takeRight(50)).map(_._1)


       //scaling based on training set
       val minMaxes: Map[Tag, MinMax] = {
         val shelfishnesses: List[UnscaledShelfishness] = b.map(rb => rb.measureShelfishness(toReadShelves))
         val justTagsAndShelfishnesses: Set[(Tag, Double)] = shelfishnesses.flatMap(s => s.tagsAndShelfishness).toSet
         val map = shelfishnessScaling(justTagsAndShelfishnesses)
         println("validator minmaxes: " + map)
         map
       }

       val testSet = scaleShelfishness(test.map(b =>
         b._1.measureShelfishness(toReadShelves)).toSet, minMaxes)

       println("validator test set: " + testSet)
       val trainSet = scaleShelfishness(train.map(b =>
         b._1.measureShelfishness(toReadShelves)).toSet, minMaxes)
       val predicted: Set[BookPrediction] = NearestNeighborFunctions.predictRatings(testSet, trainSet)

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
