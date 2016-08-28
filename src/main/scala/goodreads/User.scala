package goodreads

import java.io.IOException

import goodreads.gstuff._

import scala.xml.{Node, NodeSeq, XML, Elem}
import scalaz.{-\/, \/-}

//todo: genre and averagestar, averagestar; these both require a little math

case class User(username: Option[String], userId: Option[Int],
                age: Option[Int], gender: Option[String]) extends GResult {

  def findInShelf(env: GEnvironment)(shelf: String): GDisjunction[List[SimpleBook]] = {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/review/list/" +
        userId.get +".xml?key=" +  env.devKey + "&shelf=" + shelf))
    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }


    url.map(e => {
      val isbns = e.flatMap(n => n.\("books").\("book").\("isbn"))
      val titles = e.flatMap(n => n.\("books").\("book").\("title"))
      val zipped = isbns.zip(titles)
      zipped.map(tup => SimpleBook(tup._1.text, tup._2.text))
    }.toList)
  }

  def toReadBooks(env: GEnvironment) = findInShelf(env)("to-read")
  def readBooks(env: GEnvironment) = findInShelf(env)("read")
}

case class ReadBook(book: SimpleBook, myTags: List[String], userRating: Double,
                      averageRating: Double, length: Option[Int], authors: List[SimpleAuthor]) 

case class ToReadBook(book: SimpleBook, genres: List[String], averageRating: Option[Double], length: Option[Int],
                      authors: List[SimpleAuthor])

object User {

  def apply(id: Int)(env: GEnvironment): GDisjunction[User] = {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/user/show/" + id.toString + ".xml?key=" + env.devKey))
    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }

    for {
      x <- url
    } yield {

      def simpleUserString(s: String) = x.\("user").\(s)

      val userString = {
        val b = simpleUserString("name")
        if (b.isEmpty) None else Some(b.text)
      }

      val userId = {
        val b = simpleUserString("id")
        if (b.isEmpty) None else optionToInt(Some(b.text))
      }

      val age = {
        val b = simpleUserString("age")
        if (b.isEmpty) None else optionToInt(Some(b.text))
      }

      val gender = {
        val b = simpleUserString("gender")
        if (b.isEmpty) None else Some(b.text)
      }

      User(userString, userId, age, gender)

    }
  }
}