package scalareads.recommender

import scalareads.{User}
import scalareads.values.{GEnvironment, SimpleBook}

final case class NearestBook(b: SimpleBook, score: Double)
final case class Tag(s: String) extends AnyVal

object NearestBook {

  def apply(env: GEnvironment)(u: User): NearestBook = {
    for {
      r <- u.readBooks(env)
      t <- u.toReadBooks(env)
    } yield {
      //user's most common tags
      val topShelves = u.getTopTags(r)
      //produces (toreadbook, List((tag, relation)), option of world average rating)
      val testSet: List[(SimpleBook, List[(Tag, Double)], Option[Double])] = t.map(trb => trb.measureShelfishness(topShelves))
      //produces (readbook, List((tag, relation)), user's rating)
      val trainingSet = r.map(trb => trb.measureShelfishness(topShelves))

      val listOfNearestBooks = something(testSet, trainingSet).map(tup => NearestBook(tup._2, tup._1))
      listOfNearestBooks.foreach(println)
      listOfNearestBooks.maxBy(nb => nb.score)
    }

    def something(
                   test: List[(SimpleBook, List[(Tag, Double)], Option[Double])],
                  training: List[(SimpleBook, List[(Tag, Double)], Option[Int])]): List[List[(Double, SimpleBook)]] = {

//      println("test")
//      test.foreach(println)
//
//      println("train")
//      training.foreach(println)

      val b = for {
        testTuple <- test
        testTupleTagAndScore = testTuple._2.map(tup => (tup._1, tup._2))
        testTupleBook = testTuple._1
      } yield training.map(train =>
          (unWeightedEuclideanDistance(train._2.map(_._2), testTupleTagAndScore.map(_._2)), train._1))

      b
    }

    def unWeightedEuclideanDistance(predictors: List[Double], target: List[Double]): Double = {
      val sub = predictors.zip(target).map(dd => math.pow(dd._1 - dd._2, 2))
      math.sqrt(sub.foldRight(0.0)((num, b) => num + b))
    }

    NearestBook(SimpleBook("nope", "this sucks"), 0.0)
  }
}
