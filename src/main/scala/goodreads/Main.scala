package goodreads

object Main extends App{

  val a = Author.getAuthorDeets(18541, "k").getOrElse("There was an error. :(")
  println(a)

}
