package scalareads.general


import scalareads.values._
import scalareads.ScalareadsFunctions._

import java.io.{File, FileNotFoundException, IOException, PrintWriter}

import scala.xml.{Elem, NodeSeq, XML}
import scalaz.{-\/, \/-}


case class Book(id: String,
                title: Option[String],
                author: List[SimpleAuthor],
                length: Option[Int],
                averageRating: Option[Double],
                popularShelves: Map[Int, String],
                originalPublicationYear: Option[Int],
                ratingDistribution: RatingDistribution) extends GResult

case class RatingDistribution(fives: Option[Int],
                              fours: Option[Int],
                              threes: Option[Int],
                              twos: Option[Int],
                              ones: Option[Int],
                              total: Option[Int]) {
  override def toString =
    "Ratings: \n" +
      "5: " + this.fives.getOrElse(0) +
      "\n4: " + this.fours.getOrElse(0) +
      "\n3: " + this.threes.getOrElse(0) +
      "\n2: " + this.twos.getOrElse(0) +
      "\n1: " + this.ones.getOrElse(0) +
      "\ntotal: " + this.total.getOrElse(0)
}

object Book {

  def apply(id: String)(env: GEnvironment): GDisjunction[Book] = {
    val url: GDisjunction[Elem] = try {
      \/-(XML.loadFile(getClass.getResource(s"/book_$id.txt").getPath))
    } catch {
      case f: NullPointerException => try {
        val result = XML.load("https://www.goodreads.com/book/show/" +
          id + ".xml?key=" + env.devKey)
        printToFile(new File(s"""${env.resourcesPathWithEndSlash}
          book_$id.txt"""))((p: PrintWriter) => p.println(result))
        \/-(result)
      } catch {
        case i: IOException => -\/(IOError(i.toString))
      }
    }

    url.map(e => {
      print(".")
      makeBook(e, id)
    })
  }

  private def makeBook(e: Elem, i: String): Book = {

    def getBookString(s: String): Option[String] = {
      val output: NodeSeq = e.\("book").\(s)
      if (output.isEmpty) None else Some(output.text)
    }

    val title = getBookString("title")
    val authorID = e.\("book").\("authors").\("author").\("id").toList
    val authorName = e.\("book").\("authors").\("author").\("name").toList
    val avgRating =
      try {
        getBookString("average_rating").map(_.toDouble)
      } catch {
        case e: NumberFormatException => None
      }

    val simpleAuthors = authorID.zip(authorName)
      .map(tup => SimpleAuthor(tup._1.text, tup._2.text))

    val length = optionToInt(getBookString("num_pages"))

    val popShelves = {
      val countOutput = e.\("book").\("popular_shelves")
        .\("shelf").toList.map(x => x.\@("count").toInt)
      val nameOutput: List[String] = e.\("book").\("popular_shelves")
        .\("shelf").toList.map(x => x.\@("name"))

      countOutput.zip(nameOutput).toMap
    }

    val origPubYear = {
      val b = e.\("book").\("work").\("original_publication_year")
      optionToInt(if (b.isEmpty) None else Some(b.text))
    }

    val ratingDist: RatingDistribution = {
      val a: Option[String] = {
        val output: NodeSeq = e.\("book").\("work").\("rating_dist")
        if (output.isEmpty) None else Some(output.text)
      }
      val split = a.fold(Array.empty[Array[String]])(_.split("\\|")
        .map(_.split(":")))
      val b = split.map(as => (as.head, as.last)).toMap

      RatingDistribution(optionToInt(b.get("5")),
                         optionToInt(b.get("4")),
                         optionToInt(b.get("3")),
                         optionToInt(b.get("2")),
                         optionToInt(b.get("1")),
                         optionToInt(b.get("total")))
    }

    Book(i, title, simpleAuthors,
      length, avgRating, popShelves,
      origPubYear, ratingDist)
  }
}
