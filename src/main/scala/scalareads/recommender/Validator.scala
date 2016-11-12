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
//       val (test, train) = booksAndUserRating.splitAt(b.length/2)

       val crossValidateTest = booksAndUserRating.sliding(5).toList
       val crossValidateTrainAndTestIterator: List[(List[(ReadBook, Option[Int])], List[(ReadBook, Option[Int])])] =
         crossValidateTest.map(testList => {
           val trainList = booksAndUserRating.diff(testList)
           (testList, trainList)
         }
         )
       //booksAndUserRating.filterNot(x => crossValidateTest.contains(x))

       def getListOfTagsFromToReadShelves(trainList: List[(ReadBook, Option[Int])], unwanted: List[Tag]) = {
         val sorted50 = trainList.flatMap(trb => trb._1.popularShelves).groupBy(_._1).mapValues(l =>
           l.foldRight(0)((tup, i) => tup._2 + i)).toList.sortBy(_._2).takeRight(50)
         sorted50.foreach(println)
         sorted50.map(_._1).filterNot(unwanted.contains)
       }

       val unwantedTags = List(Tag("epic-fantasy"), Tag("fantasy-sci-fi"), Tag("default"),
         Tag("zombies"), Tag("tolkien"), Tag("dystopia"), Tag("memoir"), Tag("urban-fantasy"),
          Tag("literature"), Tag("novels"), Tag("contemporary"), Tag("young-adult"),
         Tag("books-i-own"), Tag("fiction"), Tag("adventure"), Tag("audiobook"))

//       toReadShelves2.foreach(t => println(t))
      // lowest mse with unwantedTags filtered out: 1.0480689803493954
//       val toReadShelves = getListOfTagsFromToReadShelves(train, unwantedTags)

       //scaling based on training set
       def minMaxes(tags: List[Tag], train: List[(ReadBook, Option[Int])]): Map[Tag, MinMax] = {
         //for each read book, need to compare it to each trb's shelves; measure shelfishness compared to each one
         val shelfishnesses: List[UnscaledShelfishness] = train.map(rb => {
           rb._1.measureShelfishness(tags)
         })
         val justTagsAndShelfishnesses = shelfishnesses.flatMap(s => s.tagsAndShelfishness)

         NearestNeighborFunctions.shelfishnessScaling(justTagsAndShelfishnesses)
       }

       def localScaling(list: List[(ReadBook, Option[Int])], tags: List[Tag]) =
         list.map(t => {
           scaleShelfishness(t._1.measureShelfishness(tags), minMaxes(tags, list))
         })

       val crossValidatingTestAndTrain = {
         crossValidateTrainAndTestIterator.flatMap(x => {
           val validatingTest = x._1
           val validatingTrain = x._2
           val tags = getListOfTagsFromToReadShelves(validatingTrain, unwantedTags)
           val validatingTestSet = localScaling(validatingTest, tags)
           val validatingTrainSet = localScaling(validatingTrain, tags)
           val predicted = NearestNeighborFunctions.predictRatings(validatingTestSet, validatingTrainSet)

           for {
             p <- predicted
             t <- validatingTest
           r = println(p.b == t._1.simpleBook)
           _ <- List(r)
             if p.b == t._1.simpleBook
           } yield ValidatorPredictions(p, t._2)
         })
       }
       crossValidatingTestAndTrain.toList
    }
  }

  def meanSquaredError(ps: List[ValidatorPredictions]) = {

    val filtered = ps.filter(vp => vp.actualUserRating.nonEmpty).map(vp => (vp.b, vp.actualUserRating.get))
    math.sqrt(filtered.map(vp => math.pow(vp._2.toDouble - vp._1.predictedRating, 2)).fold(0.0)((a, b) => a + b)/filtered.length)
  }
}
