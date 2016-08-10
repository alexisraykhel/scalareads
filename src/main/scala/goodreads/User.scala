package goodreads

case class User(username: String, booksRead: Int, averageStar: Double, genreAndAverageStar: Map[String, Int])

object User {}
