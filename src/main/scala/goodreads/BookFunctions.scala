package goodreads

import goodreads.gstuff.SimpleBook

import scala.collection.immutable.Iterable


object BookFunctions {

//  def recommendFromToRead(readShelf: List[Book], toReadShelf: List[Book], howMany: Int): List[SimpleBook] = ???

  def userRatingDiff(rbs: List[ReadBook]): Double = {

    //map of userrating and averagerating
    val ratingsMap: Map[SimpleBook, (Double, Double)] =
      rbs.foldRight(Map.empty[SimpleBook, (Double, Double)])(
      (rb, b) => b.updated(rb.book, (rb.userRating, rb.averageRating)))

    val c: List[Double] = ratingsMap.map(b => b._2._1 - b._2._2).toList

    c.sum / c.length
  }
}
