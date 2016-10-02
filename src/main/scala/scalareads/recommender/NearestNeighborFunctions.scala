package scalareads.recommender

import scalareads.values.SimpleBook


object NearestNeighborFunctions {

  def predictRatings(
                      test: List[ScaledShelfishness],
                      training: List[ScaledShelfishness]): List[(BookPrediction)] = {

    test.map(testTuple => {

      val testTupleTagAndScore = testTuple.tagsAndScaledShelf
      val testTagScoreAndRating = testTupleTagAndScore.map(x => (x._2, testTuple.s.avgRating))

      (testTuple.s, comparePredictorsWithTest(testTuple.s, training, testTagScoreAndRating))
    }).map(thing => bookAndRating(thing))
  }

  def unWeightedEuclideanDistance(predictors: List[(Double, Double)], target: List[(Double, Double)]): Double = {
    val zipped = predictors.zip(target)
    val sub = zipped.map(dd => math.pow(dd._1._1 - dd._2._1, 2)) ++ zipped.map(dd => math.pow(((1/dd._1._2) * dd._1._2) - ((1/dd._2._2) * dd._2._2), 2))
    math.sqrt(sub.foldRight(0.0)((num, b) => num + b))
  }

  def bookAndRating(testAndPredictors: (SimpleBook, List[(Double, SimpleBook)])): BookPrediction = {
    val averageOf5ClosestRatings = {
      testAndPredictors._2.toList.sortBy(_._1)
        .take(3).map(_._2.avgRating)
        .fold(0.0)((d1, d2) => {d1 + d2}) / 3.0
    }

    BookPrediction(testAndPredictors._1, averageOf5ClosestRatings)
  }

  def comparePredictorsWithTest(test: SimpleBook,
                                training: List[ScaledShelfishness],
                                testTagScoreAndRating: List[(Double, Double)]): List[(Double, SimpleBook)] =
    training.map(train => {
      val unweightedED = unWeightedEuclideanDistance(
        train.tagsAndScaledShelf.map(x => (x._2, train.s.avgRating)), testTagScoreAndRating)
      //      println("unweightedED: " + unweightedED + " between train: " + train.s.title + " and test: " + test.title)
      (unweightedED, train.s)
    })

  //given the list of tags and their shelfishnesses, return a map from each tag to its min and max
  def shelfishnessScaling(ls: List[(Tag, Double)]): Map[Tag, MinMax] = {
    //all tags and their scores
    ls.groupBy(td => td._1).mapValues(l => {
      val justDoubles = l.map(td => td._2)
      val min = justDoubles.min
      val max = justDoubles.max
      MinMax(min, max)
    })
  }

  //x_scaled = (x - x.min) / (x.max - x.min)

  def shelfishnessScaled(map: Map[Tag, MinMax], ls: List[(Tag, Double)]): List[(Tag, Double)] = {

    val withMinMax = ls.map(td => (td._1, td._2, map.getOrElse(td._1, MinMax(0.0, 5.0))))

    withMinMax.map{tup =>
      val min = tup._3.min
      val max = tup._3.max
      (tup._1, (tup._2 - min) / (max - min))
    }
  }

  def scaleShelfishness(us: UnscaledShelfishness, map: Map[Tag, MinMax]): ScaledShelfishness =
    ScaledShelfishness(us.s, shelfishnessScaled(map, us.tagsAndShelfishness))
}
