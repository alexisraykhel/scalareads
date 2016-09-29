package scalareads.recommender

import scalareads.User
import scalareads.values.{GDisjunction, SimpleBook, GEnvironment}
import NearestNeighborFunctions._

final case class BookPrediction(b: SimpleBook, predictedRating: Double)
final case class Tag(s: String) extends AnyVal
final case class UnscaledShelfishness(s: SimpleBook, tagsAndShelfishness: Set[(Tag, Double)])
final case class ScaledShelfishness(s: SimpleBook, tagsAndScaledShelf: Set[(Tag, Double)])
final case class MinMax(min: Double, max: Double)

object BookPrediction {

  def apply(env: GEnvironment)(u: User): GDisjunction[Option[BookPrediction]] = {
    for {
      r <- u.readBooks(env)
      t <- u.toReadBooks(env)
    } yield {

      val toReadShelves: List[Tag] = t.flatMap(trb => trb.popularShelves.takeRight(50)).map(_._1)

      //scaling based on training set
      val minMaxes: Map[Tag, MinMax] = {
        val shelfishnesses: List[UnscaledShelfishness] = r.map(rb => rb.measureShelfishness(toReadShelves))
        val justTagsAndShelfishnesses: Set[(Tag, Double)] = shelfishnesses.flatMap(s => s.tagsAndShelfishness).toSet
        NearestNeighborFunctions.shelfishnessScaling(justTagsAndShelfishnesses)
      }
      val testSet: Set[ScaledShelfishness] =
        scaleShelfishness(t.map(trb => trb.measureShelfishness(toReadShelves)).toSet, minMaxes)

      val trainingSet: Set[ScaledShelfishness] =
        scaleShelfishness(r.map(trb => trb.measureShelfishness(toReadShelves)).toSet, minMaxes)


      val listOfNearestBooks =
        NearestNeighborFunctions.predictRatings(testSet, trainingSet)
      println("listOfNearest length: " + listOfNearestBooks.size)
      println("listOfNearest 5 random: " + listOfNearestBooks.take(5))

      if (listOfNearestBooks.isEmpty) Option.empty[BookPrediction]
      else Some(listOfNearestBooks.maxBy(nb => nb.predictedRating))
    }
  }
}


