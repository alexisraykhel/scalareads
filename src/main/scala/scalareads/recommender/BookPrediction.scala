package scalareads.recommender


import scalareads.general.{ToReadBook, User, UserBook}
import scalareads.values.{GDisjunction, SimpleBook, GEnvironment}
import NearestNeighborFunctions._


final case class BookPrediction(b: SimpleBook,
                                predictedRating: Double)
final case class Tag(s: String) extends AnyVal
final case class UnscaledShelfishness(s: SimpleBook,
                                      tagsAndShelfishness: List[(Tag, Double)])
final case class ScaledShelfishness(s: SimpleBook,
                                    tagsAndScaledShelf: List[(Tag, Double)])
final case class MinMax(min: Double, max: Double)

object BookPrediction {

  def apply(env: GEnvironment)(u: User): GDisjunction[Option[BookPrediction]] = {
    for {
      r <- u.readBooks(env)
      t <- u.toReadBooks(env)
    } yield {

      //for every toreadbook, get top 50 shelves
      val toReadShelves: List[Tag] = {
        val sorted50 = t.flatMap(_.popularShelves).groupBy(_._1)
          .mapValues(_.foldRight(0)((tup, i) => tup._2 + i))
          .toList.sortBy(_._2).takeRight(50)

        sorted50.map(_._1)
      }

      val unwantedTags = List(Tag("epic-fantasy"), Tag("fantasy-sci-fi"),
        Tag("default"), Tag("zombies"), Tag("tolkien"), Tag("dystopia"),
        Tag("memoir"), Tag("urban-fantasy"), Tag("literature"), Tag("novels"),
        Tag("contemporary"), Tag("young-adult"), Tag("books-i-own"),
         Tag("fiction"), Tag("adventure"), Tag("audiobook"))

      val toReadShelves2 = toReadShelves.filterNot(unwantedTags.contains)


      //scaling based on training set
      val minMaxes: Map[Tag, MinMax] = {
        // for each read book, need to compare it to each trb's shelves;
        // measure shelfishness compared to each one
        val shelfishnesses: List[UnscaledShelfishness] = r.map(rb =>
          UserBook.measureShelfishness(rb, toReadShelves2))
        val justTagsAndShelfishnesses = shelfishnesses
          .flatMap(_.tagsAndShelfishness)

        NearestNeighborFunctions.shelfishnessScaling(justTagsAndShelfishnesses)
      }

      val testSet: List[ScaledShelfishness] = t.map(trb =>
        scaleShelfishness(UserBook.measureShelfishness(trb, toReadShelves2), minMaxes))

      val trainingSet: List[ScaledShelfishness] = r.map(rb =>
        scaleShelfishness(UserBook.measureShelfishness(rb, toReadShelves2), minMaxes))

      val predictions =
        NearestNeighborFunctions.predictRatings(testSet, trainingSet)
          .sortBy(bp => bp.predictedRating)

      if (predictions.isEmpty) Option.empty[BookPrediction]
      else Some(predictions.maxBy(_.predictedRating))
    }
  }
}
