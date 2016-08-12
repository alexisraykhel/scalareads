package goodreads

import java.io.IOException

import goodreads.gstuff._

import scala.xml.{XML, Elem}
import scalaz.{\/, Reader, -\/, \/-}

case class Book(isbn: String, author: List[SimpleAuthor], length: Int, averageStar: Double)

object Book {

  //by goodreadsid: "https://www.goodreads.com/book/show/" + id + ".xml?key=" + env.devKey
  //by isbn: "https://www.goodreads.com/book/isbn/" + isbn + "?key=" + env.devKey

  def apply(isbn: String)(env: GEnvironment): GDisjunction[Book] = {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/book/isbn/" + isbn + "?key=" + env.devKey))

    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }

    for {
      x <- url
    } yield Book(isbn, List.empty, 50, 3.0)
  }
}