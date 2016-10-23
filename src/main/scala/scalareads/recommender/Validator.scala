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
           println("validatingTest length: " + validatingTest.size)
           val validatingTrain = x._2
           println("validatingTrain length: " + validatingTrain.size)
           val tags = getListOfTagsFromToReadShelves(validatingTrain, unwantedTags)
           println("tags length: " + tags.size)
           val validatingTestSet = localScaling(validatingTest, tags)
           println("validatingTestSet length: " + validatingTestSet.size)
           val validatingTrainSet = localScaling(validatingTrain, tags)
           println("validatingTrainSet length: " + validatingTrainSet.size)
           val predicted = NearestNeighborFunctions.predictRatings(validatingTestSet, validatingTrainSet)
           println("predicted length: " + predicted.size)

           for {
             p <- {
               println("predicted: " + predicted.size)
               predicted
             }
             t <- {
               println("validatingtest: " + validatingTest.size)
               validatingTest
             }
            q = {
              println("p: " + p)
              println("t._1.simpleBook: " + t._1.simpleBook)
            }
           _ <- List(q)
           r = println(p.b == t._1.simpleBook)
           _ <- List(r)
             if p.b == t._1.simpleBook
           } yield ValidatorPredictions(p, t._2)
         })
       }


//       val testSet: List[ScaledShelfishness] = localScaling(test)
//
//       val trainSet = localScaling(train)

//       val predicted: List[BookPrediction] = NearestNeighborFunctions.predictRatings(testSet, trainSet)
//
//       for {
//         p <- predicted
//         t <- test
//         if p.b == t._1.simpleBook
//       } yield ValidatorPredictions(p, t._2)

       println("FLSDKJLSDKJC!")
       crossValidatingTestAndTrain.map(vp => {
         println("holy crap: " + vp)
         vp
       })
       println("crossValidatingTestAndTrain size: " + crossValidatingTestAndTrain.size)
       val b = crossValidatingTestAndTrain.toList
       println("b: " + b.size)
       b
    }
  }

  def meanSquaredError(ps: List[ValidatorPredictions]) = {
    println("mean squared error")
    println("ps: " + ps)
    val filtered = ps.filter(vp => {
      println("vp: " + vp)
      vp.actualUserRating.nonEmpty
    }).map(vp => {
      println("userrating: " + vp.actualUserRating)
      println("book prediction: " + vp.b)
      (vp.b, vp.actualUserRating.get)
    })
    println("filtered: " + filtered)
    math.sqrt(filtered.map(vp => {
      println("vp inside sqrt: " + vp)
      val pow = math.pow(vp._2.toDouble - vp._1.predictedRating, 2)
      println("pow: " + pow)
      pow
    }).fold(0.0)((a, b) => a + b)/filtered.length)
  }
}
