package scalareads

import scalareads.values.{GDisjunction, ToIntError, SimpleBook}
import scalaz.{-\/, \/-}


object ScalareadsFunctions {

//  def recommendFromToRead(readShelf: List[Book], toReadShelf: List[Book], howMany: Int): List[SimpleBook] = ???

  def userRatingDiff(rbs: List[ReadBook]): Double = {

    //map of userrating and averagerating
    val ratingsMap: Map[SimpleBook, (Double, Double)] =
      rbs.foldRight(Map.empty[SimpleBook, (Double, Double)])(
      (rb, b) => b.updated(rb.book, (rb.userRating, rb.averageRating)))

    val c: List[Double] = ratingsMap.map(b => b._2._1 - b._2._2).toList

    c.sum / c.length
  }

  def optionToInt(o: Option[String]): Option[Int] =
    o.fold(Option.empty[Int]) { s => try {
      Some(s.toInt)
    }
    catch {
      case e: NumberFormatException => Option.empty[Int]
    }
    }

  def stringToInt(s: String): GDisjunction[Int] = try {
    \/-(s.toInt)
  } catch {
    case e: NumberFormatException => -\/(ToIntError(s + " is not a valid integer. " + e.getMessage))
  }

  def optionToDouble(o: Option[String]): Option[Double] =
    o.fold(Option.empty[Double])(s => try {
      Some(s.toDouble)
    } catch {
      case e: NumberFormatException => Option.empty[Double]
    }
    )

  def toMaybeBoolean(os: Option[String]): Option[Boolean] =
    os.fold(Option.empty[Boolean])(s =>
      try {
        Some(s.toBoolean)
      } catch {
        case i: IllegalArgumentException => None
      }
    )
}
