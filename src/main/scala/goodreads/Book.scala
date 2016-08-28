package goodreads

import java.io.IOException

import goodreads.gstuff._

import scala.xml.{NodeSeq, XML, Elem}
import scalaz.{\/, Reader, -\/, \/-}

case class Book(isbn: String, title: Option[String], author: List[SimpleAuthor], length: Option[Int],
                averageRating: Option[Double], popularShelves: Map[Int, String], originalPublicationYear: Option[Int],
                ratingDistribution: RatingDistribution) extends GResult

case class RatingDistribution(fives: Option[Int], fours: Option[Int], threes: Option[Int],
                              twos: Option[Int], ones: Option[Int], total: Option[Int]) {
  override def toString = {
    "Ratings: \n" +
      "5: " + this.fives.getOrElse(0) +
      "\n4: " + this.fours.getOrElse(0) + "\n3: " + this.threes.getOrElse(0) +
      "\n2: " + this.twos.getOrElse(0) + "\n1: " + this.ones.getOrElse(0) + "\ntotal: " + this.total.getOrElse(0)
  }
}

object Book {

  def apply(isbn: String)(env: GEnvironment): GDisjunction[Book] = {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/book/isbn/" + isbn + "?key=" + env.devKey))

    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }

    for {
      x <- url
    } yield {


      def getBookString(s: String): Option[String] = {
        val output: NodeSeq = x.\("book").\(s)
        if (output.isEmpty) None else Some(output.text)
      }

      val title = getBookString("title")
      val authorID = x.\("book").\("authors").\("author").\("id").toList
      val authorName = x.\("book").\("authors").\("author").\("name").toList
      val avgRating = {
        try {
          getBookString("average_rating").map(s => s.toDouble)
        } catch {
          case e: NumberFormatException => None
        }
      }

      val simpleAuthors = authorID.zip(authorName).map(tup => SimpleAuthor(tup._1.text, tup._2.text))
      val length = {
        val a = getBookString("num_pages")
        optionToInt(a)
      }

      val popShelves: Map[Int, String] = {
        val countOutput: List[Int] = x.\("book").\("popular_shelves").\("shelf").toList.map(x => x.\@("count").toInt)
        val nameOutput: List[String] = x.\("book").\("popular_shelves").\("shelf").toList.map(x => x.\@("name"))
        val zipped = countOutput.zip(nameOutput).toMap

        zipped
      }

      val origPubYear = {
        val b = x.\("book").\("work").\("original_publication_year")
        val a = if (b.isEmpty) None else Some(b.text)
        optionToInt(a)
      }

      val ratingDist: RatingDistribution = {
        val a: Option[String] = {
          val output: NodeSeq = x.\("book").\("work").\("rating_dist")
          if (output.isEmpty) None else Some(output.text)
        }
        val split = a.fold(Array.empty[Array[String]])(b => b.split("\\|").map(s => s.split(":")))

        val options = split.map(as => (as.head, as.last))

        val b: Map[String, String] = options.toMap

        RatingDistribution(optionToInt(b.get("5")),optionToInt(b.get("4")), optionToInt(b.get("3")),
          optionToInt(b.get("2")), optionToInt(b.get("1")), optionToInt(b.get("total")))
      }

      Book(isbn, title, simpleAuthors, length, avgRating, popShelves, origPubYear, ratingDist)
    }
  }
}