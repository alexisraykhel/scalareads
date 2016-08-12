package goodreads


import java.io.IOException

import goodreads.gstuff.{GDisjunction, IOError}

import scala.xml.{NodeSeq, Elem, XML}
import scalaz.{-\/, \/-, \/}


case class Author(name: Option[String], id: Option[Int], link: Option[String], fansCount: Option[Int],
                   authorFollowersCount: Option[Int], influences: Option[String], worksCount: Option[Int],
                   gender: Option[String], hometown: Option[String], bornAt: Option[String], diedAt: Option[String],
                   goodreadsAuthor: Option[Boolean])


object Author {

  def getAuthorDeets(id: Int, devKey: String): GDisjunction[Author] = {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/author/show/" + id.toString + "?format=xml&key=" + devKey))
    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }

    for {
      x <- url
    } yield {

      def toMaybeInt(os: Option[String]): Option[Int] = os.fold(Option.empty[Int])(s => Some(s.toInt))

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
//      Author(x.\("author").\("name").text, x.\("author").\("id").text)
      val name = getAuthorString("name")
      val id = toMaybeInt(getAuthorString("id"))
      val link = getAuthorString("link")
      val fansCount = toMaybeInt(getAuthorString("fans_count"))
      val authorFollowersCount = toMaybeInt(getAuthorString("author_followers_count"))
      val influences = getAuthorString("influences")
      val worksCount = toMaybeInt(getAuthorString("works_count"))
      val gender = getAuthorString("gender")
      val hometown = getAuthorString("hometown")
      val bornAt = getAuthorString("born_at")
      val diedAt = getAuthorString("died_at")
      val goodreadsAuthor = toMaybeBoolean(getAuthorString("goodreads_author"))
      Author(name, id, link, fansCount, authorFollowersCount, influences, worksCount, gender,
      hometown, bornAt, diedAt, goodreadsAuthor)
    }
  }
}