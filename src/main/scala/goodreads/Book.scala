package goodreads

import java.io.IOException

import goodreads.gstuff._

import scala.xml.{XML, Elem}
import scalaz.{Reader, -\/, \/-}

case class Book(isbn: String, author: List[SimpleAuthor], length: Int, averageStar: Double)

object Book {

  def apply(isbn: String): GReader[GDisjunction[Book]] = Reader((env: GEnvironment) => {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/author/show/" + isbn.toString + "?format=xml&key=" + env.devKey))
    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }

    for {
      x <- url
    } yield Book(isbn, List.empty, 50, 3.0)
  }
  )
}