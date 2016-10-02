package scalareads.recommender

import scalareads.{ToReadBook, User}
import scalareads.values.{GDisjunction, SimpleBook, GEnvironment}
import NearestNeighborFunctions._

final case class BookPrediction(b: SimpleBook, predictedRating: Double)
final case class Tag(s: String) extends AnyVal
final case class UnscaledShelfishness(s: SimpleBook, tagsAndShelfishness: List[(Tag, Double)])
final case class ScaledShelfishness(s: SimpleBook, tagsAndScaledShelf: List[(Tag, Double)])
final case class MinMax(min: Double, max: Double)

object BookPrediction {

  def apply(env: GEnvironment)(u: User): GDisjunction[Option[BookPrediction]] = {
    for {
      r <- u.readBooks(env)
      t <- u.toReadBooks(env)
    } yield {

      //for every toreadbook, gets top 50 shelves
      val toReadShelves: List[(ToReadBook, List[Tag])] = t.map(trb => (trb, trb.popularShelves.takeRight(50).map(_._1)))

      //scaling based on training set
      val minMaxes: Map[ToReadBook, Map[Tag, MinMax]] = {
        //for each read book, need to compare it to each trb's shelves; measure shelfishness compared to each one
        val shelfishnesses: List[(ToReadBook, UnscaledShelfishness)] = r.flatMap(rb => {
          toReadShelves.map(tup => (tup._1, rb.measureShelfishness(tup._2)))
        })
        val justTagsAndShelfishnesses = shelfishnesses.map(s => (s._1, s._2.tagsAndShelfishness))

        justTagsAndShelfishnesses.map(tup => (tup._1, NearestNeighborFunctions.shelfishnessScaling(tup._2))).toMap
      }

      val testSet: List[ScaledShelfishness] = {
        //for each trb measure its shelfishness, get correct minmaxes

        val mms: List[(Option[Map[Tag, MinMax]], ToReadBook)] = t.map(trb => minMaxes.get(trb)).zip(t)
        val noNones: List[(ToReadBook, Map[Tag, MinMax])] = mms.map(tup => tup._1 match {
          case None => (tup._2, Map.empty[Tag, MinMax])
          case Some(x) => (tup._2, x)
        })

        val unscaledShelfishnesses: List[(UnscaledShelfishness, Map[Tag, MinMax])] = noNones.map(trb => {
          val trbTags = trb._2.toList.map(_._1)

          (trb._1.measureShelfishness(trbTags), trb._2)
        })

        unscaledShelfishnesses.map(tup => scaleShelfishness(tup._1, tup._2))
      }
      val trainingSet: List[ScaledShelfishness] = {
        //for each rb measure its shelfishness as compared with a trb


        val unscaledS: List[(UnscaledShelfishness, Map[Tag, MinMax])] = minMaxes.toList.flatMap(tup => {
          val trbTags = tup._2.toList.map(_._1)

          r.map(rb => (rb.measureShelfishness(trbTags), tup._2))
        })

        unscaledS.map(tup => scaleShelfishness(tup._1, tup._2))
      }

      val listOfNearestBooks =
        NearestNeighborFunctions.predictRatings(testSet, trainingSet)
      println("listOfNearest length: " + listOfNearestBooks.size)
      println("listOfNearest 5 random: " + listOfNearestBooks.take(3))

      if (listOfNearestBooks.isEmpty) Option.empty[BookPrediction]
      else Some(listOfNearestBooks.maxBy(nb => nb.predictedRating))
    }
  }
}


