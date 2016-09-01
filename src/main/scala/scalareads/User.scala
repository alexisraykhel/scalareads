package scalareads

import scalareads.values._
import ScalareadsFunctions._
import java.io.IOException
import scala.xml.{XML, Elem}
import scalaz.{\/, -\/, \/-}
import scalaz.syntax.either._
import scalaz.std.list._
import scalaz.syntax.traverse._

//todo: genre and averagestar, averagestar; these both require a little math

case class User(username: Option[String], 
                userId: Option[Int],
                age: Option[Int], 
                gender: Option[String]) extends GResult {

  def findInShelf(env: GEnvironment)(shelf: String): GDisjunction[List[SimpleBook]] = {
    def url(page: Int): GDisjunction[Elem] =
      try {
        \/-(XML.load("https://www.goodreads.com/review/list/" +
          userId.get +".xml?key=" +  env.devKey + "&shelf=" + shelf + "&page=" + page))
      } catch {
        case i: IOException => -\/(IOError(i.toString))
      }

    def simpleBookZip(e: Elem): List[SimpleBook] = {
      val isbns = e.flatMap(n => n.\("books").\("book").\("isbn"))
      val titles = e.flatMap(n => n.\("books").\("book").\("title"))
      val zipped = isbns.zip(titles)
      zipped.map(tup => SimpleBook(tup._1.text, tup._2.text))
    }.toList

    def endOfList(g: GDisjunction[Elem]): Boolean =
      g.map(e => e.\("books").\@("end") == e.\("books").\@("total")) match {
        case -\/(error) => false
        case \/-(good) => good
      }

    def something(n: Int, es: List[GDisjunction[Elem]]): List[GDisjunction[Elem]] = {
      val u = url(n)
      if (endOfList(url(n))) u :: es
      else something(n + 1, u :: es)
    }

    val listOfLists = something(1, List.empty).map(gdisj => gdisj.map(simpleBookZip))

    listOfLists.foldLeft(List.empty[SimpleBook].right[GError])(
      (o: \/[GError, List[SimpleBook]], d: \/[GError, List[SimpleBook]]) =>
      {
        for {
          firstL <- o
          secondL <- d
        } yield firstL ++ secondL
      })
  }

  def toReadShelf(env: GEnvironment) = findInShelf(env)("to-read")
  def readShelf(env: GEnvironment) = findInShelf(env)("read")
}

//<books start="1" end="20" total="136" numpages="7" currentpage="1">

case class ReadBook(book: SimpleBook,
                    myTags: List[String],
                    userRating: Double,
                    averageRating: Double,
                    length: Option[Int],
                    authors: List[SimpleAuthor])

case class ToReadBook(book: SimpleBook,
                      genres: List[String],
                      averageRating: Option[Double],
                      length: Option[Int],
                      authors: List[SimpleAuthor])

object User {

  def apply(id: Int)(env: GEnvironment): GDisjunction[User] = {
    val url: GDisjunction[Elem] =
      try {
        \/-(XML.load("https://www.goodreads.com/user/show/" + id.toString + ".xml?key=" + env.devKey))
      } catch {
        case i: IOException => -\/(IOError(i.toString))
      }

    url.map(makeUser)
  }

  private def makeUser(e: Elem): User = {
    def simpleUserString(s: String) = e.\("user").\(s)

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