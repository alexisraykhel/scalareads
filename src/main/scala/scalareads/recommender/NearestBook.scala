package scalareads.recommender

import scalareads.User
import scalareads.values.{IOError, GDisjunction, SimpleBook, GEnvironment}

final case class NearestBook(b: SimpleBook, predictedRating: Double)
final case class Tag(s: String) extends AnyVal
final case class Shelfishness(s: SimpleBook, tagsAndShelfishness: List[(Tag, Double)])
final case class PredictedRating(r: Double)

object NearestBook {

  def apply(env: GEnvironment)(u: User): GDisjunction[Option[NearestBook]] = {
    for {
      r <- u.readBooks(env)
      t <- u.toReadBooks(env)
    } yield {

      val toReadShelves: List[Tag] = t.flatMap(trb => trb.popularShelves.takeRight(50)).map(_._1)
      val testSet: List[Shelfishness] = t.map(trb => {
        trb.measureShelfishness(toReadShelves)
      })
      val trainingSet = r.map(trb => trb.measureShelfishness(toReadShelves))
      val listOfNearestBooks = predictRatings(testSet, trainingSet).map(ls => NearestBook(ls._1, ls._2.r))
      println("listOfNearest length: " + listOfNearestBooks.length)
      println("listOfNearest 5 random: " + listOfNearestBooks.take(5))

      if (listOfNearestBooks.isEmpty) Option.empty[NearestBook]
      else Some(listOfNearestBooks.maxBy(nb => nb.predictedRating))
    }
  }

  def predictRatings(
                 test: List[Shelfishness],
                 training: List[Shelfishness]): List[(SimpleBook, PredictedRating)] = {

    test.map(testTuple => {

      val testTupleTagAndScore = testTuple.tagsAndShelfishness
      val testTagScoreAndRating = testTupleTagAndScore.map(x => (x._2, testTuple.s.avgRating))

      (testTuple.s, comparePredictorsWithTest(testTuple.s, training, testTagScoreAndRating))
    }).map(thing => bookAndRating(thing))
  }

  def unWeightedEuclideanDistance(predictors: List[(Double, Double)], target: List[(Double, Double)]): Double = {
    val zipped = predictors.zip(target)
    val sub = zipped.map(dd => math.pow(dd._1._1 - dd._2._1, 2)) ++ zipped.map(dd => math.pow(dd._1._2 - dd._2._2, 2))
    math.sqrt(sub.foldRight(0.0)((num, b) => num + b))
  }

  def bookAndRating(testAndPredictors: (SimpleBook, List[(Double, SimpleBook)])): (SimpleBook, PredictedRating) = {
    val averageOf5ClosestRatings = {
      testAndPredictors._2.sortBy(_._1)
        .take(5).map(_._2.avgRating)
        .fold(0.0)((d1, d2) => {d1 + d2}) / 5.0
    }
    (testAndPredictors._1, PredictedRating(averageOf5ClosestRatings))
  }

  def comparePredictorsWithTest(test: SimpleBook,
                                training: List[Shelfishness],
                                testTagScoreAndRating: List[(Double, Double)]): List[(Double, SimpleBook)] =
    training.map(train => {
      val unweightedED = unWeightedEuclideanDistance(
        train.tagsAndShelfishness.map(x => (x._2, train.s.avgRating)), testTagScoreAndRating)
//      println("unweightedED: " + unweightedED + " between train: " + train.s.title + " and test: " + test.title)
      (unweightedED, train.s)
    })
}
