package scalareads.general


import scalareads.values._
import scalareads.ScalareadsFunctions._

import java.io.{File, FileNotFoundException, IOException, PrintWriter}
import java.net.URL

import scala.xml.{Elem, Node, NodeSeq, XML}
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
      val where = getClass.getResource(s"/author_$id.txt").getPath
      \/-(XML.loadFile(where))
    } catch {
      case f: FileNotFoundException => try {
        val result = XML.load("https://www.goodreads.com/author/show/" +
          id.toString + "?format=xml&key=" + env.devKey)

        printToFile(new File(s"""${env.resourcesPathWithEndSlash}
          author_$id.txt"""))((p: PrintWriter) => p.println(result))

        \/-(result)
      } catch {
        case i: IOException => {
          -\/(IOError(i.toString))
        }
      }
    }

    url.map(makeAuthor)
  }

  private def makeAuthor(e: Elem): Author = {

    def getAuthorString(s: String) = {
      val output: NodeSeq = e.\("author").\(s)
      if (output.isEmpty) None
      else Some(output.text)
    }

    val name = getAuthorString("name").map(AuthorName(_))
    val id = optionToInt(getAuthorString("id")).map(AuthorID(_))
    val link = getAuthorString("link").map(Link(_))
    val fansCount = optionToInt(getAuthorString("fans_count"))
    val authorFollowersCount = optionToInt(getAuthorString("author_followers_count"))
    val influences = getAuthorString("influences")
    val worksCount = optionToInt(getAuthorString("works_count"))
    val gender = getAuthorString("gender")
    val hometown = getAuthorString("hometown")
    val bornAt = getAuthorString("born_at")
    val diedAt = getAuthorString("died_at")
    val goodreadsAuthor = toMaybeBoolean(getAuthorString("goodreads_author"))

    Author(name, id, link, fansCount,
      authorFollowersCount, influences, worksCount, gender,
      hometown, bornAt, diedAt, goodreadsAuthor,
      books(e))
    }


  private def books(e: Elem) = {
    val bookTitle = e.\("author").\("books").\("book").\("title").toList
    val bookId = e.\("author").\("books").\("book").\("id").toList
    val bookRating: List[Double] = {
      val maybeRating = e.\("author").\("books").\("book").\("average_rating")
      val maybes = maybeRating.map{n =>
        try {Some(n.text.toDouble)
        } catch
          { case e: NumberFormatException => Option.empty[Double]}
      }.toList
      maybes.filterNot(_.isEmpty).map(op => op.get)
    }

    bookId.zip(bookTitle).zip(bookRating).map(tup =>
      SimpleBook(tup._1._1.text, tup._1._2.text, tup._2))
  }
}
