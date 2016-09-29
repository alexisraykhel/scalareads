package scalareads

import scalareads.values._
import ScalareadsFunctions._
import java.io.{File, IOException}
import scala.xml.{Node, NodeSeq, Elem, XML}
import scalaz._

case class Author(name: Option[AuthorName],
                  id: Option[AuthorID],
                  link: Option[Link],
                  fansCount: Option[Int],
                  authorFollowersCount: Option[Int],
                  influences: Option[String],
                  worksCount: Option[Int],
                  gender: Option[String],
                  hometown: Option[String],
                  bornAt: Option[String],
                  diedAt: Option[String],
                  goodreadsAuthor: Option[Boolean],
                  works: List[SimpleBook]) extends GResult

case class AuthorName(v: String) extends AnyVal
case class AuthorID(v: Int) extends AnyVal
case class Link(v: String) extends AnyVal

object Author {

  def apply(id: Int)(env: GEnvironment): GDisjunction[Author] =   {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/author/show/" + id.toString + "?format=xml&key=" + env.devKey))
    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }

    url.map{ e =>

//      printToFile(new File(s"/Users/araykhel/scala_practice/goodreads/src/main/resources/author_$id.txt"))(p => p.println(e))

      makeAuthor(e)
    }
  }

  private def makeAuthor(e: Elem): Author = {

    def getAuthorString(s: String): Option[String] = {
      val output: NodeSeq = e.\("author").\(s)
      if (output.isEmpty) None else Some(output.text)
    }

    val name = getAuthorString("name").map(s => AuthorName(s))
    val id = optionToInt(getAuthorString("id")).map(i => AuthorID(i))
    val link = getAuthorString("link").map(s => Link(s))
    val fansCount = optionToInt(getAuthorString("fans_count"))
    val authorFollowersCount = optionToInt(getAuthorString("author_followers_count"))
    val influences = getAuthorString("influences")
    val worksCount = optionToInt(getAuthorString("works_count"))
    val gender = getAuthorString("gender")
    val hometown = getAuthorString("hometown")
    val bornAt = getAuthorString("born_at")
    val diedAt = getAuthorString("died_at")
    val goodreadsAuthor = toMaybeBoolean(getAuthorString("goodreads_author"))

    val books: List[SimpleBook] = {
      val bookTitle: List[Node] = e.\("author").\("books").\("book").\("title").toList
      val bookId = e.\("author").\("books").\("book").\("id").toList
      val bookRating: List[Double] = {
        val maybeRating: NodeSeq = e.\("author").\("books").\("book").\("average_rating")
        val maybes = maybeRating.map{n =>
          try {Some(n.text.toDouble)
          } catch
            { case e: NumberFormatException => Option.empty[Double]}
        }.toList
        maybes.filterNot(_.isEmpty).map(op => op.get)
      }

      bookId.zip(bookTitle).zip(bookRating).map(tup => SimpleBook(tup._1._1.text, tup._1._2.text, tup._2))
    }

    Author(name, id, link, fansCount, authorFollowersCount, influences, worksCount, gender,
      hometown, bornAt, diedAt, goodreadsAuthor, books)
    }
}