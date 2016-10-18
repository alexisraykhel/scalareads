package scalareads

import scalareads.recommender.{Tag, UnscaledShelfishness}
import scalareads.values._
import ScalareadsFunctions._
import java.io.{File, FileNotFoundException, IOException, PrintWriter}

import scala.xml.{Elem, Node, XML}
import scalaz.{-\/, \/, \/-}
import scalaz.syntax.either._
import scalaz.std.list._
import scalaz.syntax.traverse._
import java.io

case class User(username: Option[String], 
                userId: Int,
                age: Option[Int], 
                gender: Option[String],
                 tags: Option[List[(Option[Int], Tag)]]) extends GResult {

  def findInShelf(env: GEnvironment)(shelf: String): GDisjunction[List[((SimpleBook, List[Tag]), Option[Int])]] = {
    def url(page: Int): GDisjunction[Elem] =
      try {
        \/-{
          val where = getClass.getResource(s"/user_${userId.toString}_${shelf}_$page.txt").getPath
          XML.loadFile(where)
        }
      } catch {
        case i: FileNotFoundException => try {
          val result = XML.load(s"https://www.goodreads.com/review/list/${userId.toString}.xml?key=${env.devKey}&shelf=$shelf&page=$page&v=2")
          printToFile(new File(s"/Users/araykhel/scala_practice/goodreads/src/main/resources/user_${userId.toString}_${shelf}_$page.txt"))((p: PrintWriter) => p.println(result))

          \/-(result)
        } catch {
          case i: IOException => -\/(IOError(i.toString))
        }
      }

    def simpleBookZip(e: Elem): List[((SimpleBook, List[Tag]), Option[Int])] = {
      val bookIds: List[Node] = e.flatMap(n => n.\("reviews").\("review").\("book").\("id")).toList
      val titles = e.flatMap(n => n.\("reviews").\("review").\("book").\("title")).toList
      val userTags = e.flatMap(n => n.\("reviews").\("review").\("shelves"))
        .toList.map(n => n.\("shelf").toList)
        .map(lnds => lnds.map(n => Tag(n.\@("name"))))
      val worldRatings = e.flatMap(n => n.\("reviews").\("review").\("book").\("average_rating"))
        .map(x => optionToDouble(Some(x.text))).toList.map(x => x.fold(0.0)(identity))
      val userRating = e.flatMap(n => n.\("reviews").\("review").\("rating")).map(n => {
        if (n.text.isEmpty) Option.empty[Int]
        else optionToInt(Some(n.text))
      }).toList

      bookIds.zip(titles).zip(worldRatings).map(tup =>
        SimpleBook(tup._1._1.text, tup._1._2.text, tup._2)
      ).zip(userTags).zip(userRating)
    }

    def endOfList(g: GDisjunction[Elem]): Boolean =
      g.map(e => e.\("reviews").\@("end") == e.\("reviews").\@("total")) match {
        case -\/(error) => false
        case \/-(good) => good
      }

    def getAllPages(n: Int, es: List[GDisjunction[Elem]]): List[GDisjunction[Elem]] = {
      val u = url(n)
      if (endOfList(u)) u :: es
      else getAllPages(n + 1, u :: es)
    }

    val listOfLists = getAllPages(1, List.empty).map(gdisj => {
      gdisj.map(s => {
        val simp = simpleBookZip(s)
        simp
      })
    })

    val b = listOfLists.foldLeft(List.empty[((SimpleBook, List[Tag]), Option[Int])].right[GError])(
      (o: \/[GError, List[((SimpleBook, List[Tag]), Option[Int])]], d: \/[GError, List[((SimpleBook, List[Tag]), Option[Int])]]) =>
      {
        for {
          firstL <- o
          secondL <- d
        } yield firstL ++ secondL
      })
    b
  }

  def toReadShelf(env: GEnvironment) = findInShelf(env)("to-read")
  def readShelf(env: GEnvironment) = findInShelf(env)("read")

  def readBooks(env: GEnvironment): GDisjunction[List[ReadBook]] = {
    val found = readShelf(env)

    val somethingElse: GDisjunction[List[Book]] = for {
      l <- found
      mappedL = l.map(tup => tup._1._1.getBook(env))
      sequenced = mappedL.sequenceU
      tup <- sequenced //List[Book]
    } yield tup

    val zipped: GDisjunction[List[(((SimpleBook, List[Tag]), Option[Int]), Book)]] = for {
      f <- found
      b <- somethingElse
    } yield f.zip(b)

    zipped.map(list => list.map(trip => {
      ReadBook(
        trip._1._1._1,
        trip._2,
        trip._1._1._2,
        trip._2.popularShelves.toList.map(tup => (Tag(tup._2), tup._1)),
        trip._1._2,
        trip._2.averageRating)
    }))
  }
  def toReadBooks(env: GEnvironment): GDisjunction[List[ToReadBook]] = {

    val found = toReadShelf(env)

    val somethingElse: GDisjunction[List[Book]] = for {
      l <- found
      mappedL = l.map(tup => tup._1._1.getBook(env))
      sequenced = mappedL.sequenceU
      tup <- sequenced //List[Book]
    } yield tup

    val zipped = for {
      f <- found
      b <- somethingElse
    } yield f.zip(b)

    zipped.map(list => list.map(trip => {
      ToReadBook(
      trip._1._1._1,
      trip._2.popularShelves.toList.map(tup => (Tag(tup._2), tup._1)).sortBy(tup => tup._2),
      trip._2.averageRating
      )
    }))
  }

  //most popular tags in the world from user's readbooks list
  def getUsersTopTags(rbs: List[ReadBook]): List[Tag] = {
    val b: List[Tag] = rbs.foldRight(List.empty[Tag])((rb, b) => rb.usersTags ++ b)
    val c = b.groupBy(identity).toList.map(tup => (tup._1, tup._2.length)).sortBy(_._2)

    c.takeRight(10).map(tup => tup._1)
  }
}

object User {

  def apply(id: Int)(env: GEnvironment): GDisjunction[User] = {
    val url: GDisjunction[Elem] =
      try {
        val where = getClass.getResource(s"/user_${id.toString}.txt").getPath
        \/-(XML.loadFile(where))
      } catch {
        case i: FileNotFoundException => try {
          val result = XML.load(s"https://www.goodreads.com/user/show/${id.toString}.xml?key=${env.devKey}")
          printToFile(new File(s"/Users/araykhel/scala_practice/goodreads/src/main/resources/user_${id.toString}.txt"))((p: PrintWriter) => p.println(result))

          \/-(result)
        } catch {
          case i: IOException => -\/(IOError(i.toString))
        }
      }

    url.map(makeUser(id))
  }

  private def makeUser(id: Int)(e: Elem): User = {

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

    val userShelves = {
      val b = simpleUserString("user_shelves").\("user_shelf").\("name")
        .toList.map(n => Tag(n.text))
      val c = simpleUserString("user_shelves").\("user_shelf").\("book_count")
        .toList.map(n => optionToInt(Some(n.text)))
      if (b.isEmpty || c.isEmpty) Option.empty[List[(Option[Int], Tag)]]
      else Some(c.zip(b))
    }

    User(userString, id, age, gender, userShelves)
  }
}