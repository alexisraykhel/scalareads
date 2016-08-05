package goodreads

import scala.util.parsing.json.JSON


case class Author(name: String, nationality: String, gender: String, numberOfBooks: Int, averageStar: Double) {
  def apply(id: Int, devKey: String) = {
    val url = "https://www.goodreads.com/author/show/" + id.toString + "?format=xml&key=" + devKey
    val result = scala.io.Source.fromURL(url).mkString

    Author(???, ???, ???, ???, ???)
  }
}

object Author {}

case class Book(title: String, author: List[String], length: Int, averageStar: Double)

object Book {}

case class User(username: String, booksRead: Int, averageStar: Double, genreAndAverageStar: Map[String, Int])

object User {}