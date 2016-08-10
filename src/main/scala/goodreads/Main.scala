package goodreads

object Main extends App{

  val a = Author.getAuthorDeets(18541, "xQXvrwOTLq7xonOLcjt2A").getOrElse("There was an error. :(")
  println(a)

}
