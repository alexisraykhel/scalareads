package goodreads

case class Book(title: String, author: List[String], length: Int, averageStar: Double)

object Book {}

case class User(username: String, booksRead: Int, averageStar: Double, genreAndAverageStar: Map[String, Int])

object User {}