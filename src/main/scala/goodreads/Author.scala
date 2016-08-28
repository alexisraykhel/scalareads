package goodreads


import java.io.IOException

import goodreads.gstuff._

import scala.collection.immutable.Seq
import scala.xml.{Node, NodeSeq, Elem, XML}
import scalaz._


case class Author(name: Option[String], id: Option[Int], link: Option[String], fansCount: Option[Int],
                   authorFollowersCount: Option[Int], influences: Option[String], worksCount: Option[Int],
                   gender: Option[String], hometown: Option[String], bornAt: Option[String], diedAt: Option[String],
                   goodreadsAuthor: Option[Boolean], works: List[SimpleBook]) extends GResult {

  def getAuthorsBook(sbs: SimpleBook)(env: GEnvironment): GDisjunction[Book] = Book(sbs.isbn)(env)
}


object Author {

//  def theREALApply(id: Int) = apply(id)(theEnvironmentYoureUsing)

  def apply(id: Int)(env: GEnvironment): GDisjunction[Author] = {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/author/show/" + id.toString + "?format=xml&key=" + env.devKey))
    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }

    for {
      x <- url
    } yield {

      def toMaybeBoolean(os: Option[String]): Option[Boolean] = os.fold(Option.empty[Boolean])(s => try {
        Some(s.toBoolean)
      } catch {
        case i: IllegalArgumentException => None
        }
      )

      def getAuthorString(s: String): Option[String] = {
        val output: NodeSeq = x.\("author").\(s)
        if (output.isEmpty) None else Some(output.text)
      }

      val name = getAuthorString("name")
      val id = optionToInt(getAuthorString("id"))
      val link = getAuthorString("link")
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

        val bookTitle: List[Node] = x.\("author").\("books").\("book").\("title").toList
        val isbn = x.\("author").\("books").\("book").\("isbn").toList

        val zipped: List[(Node, Node)] = isbn.zip(bookTitle)

        zipped.map(tup => SimpleBook(tup._1.text, tup._2.text))
      }

      Author(name, id, link, fansCount, authorFollowersCount, influences, worksCount, gender,
      hometown, bornAt, diedAt, goodreadsAuthor, books)
    }
  }
}