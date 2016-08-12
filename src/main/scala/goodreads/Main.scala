package goodreads

import goodreads.gstuff.GEnvironment

object Main extends App{

  val a = Author(1077326)(GEnvironment("xQXvrwOTLq7xonOLcjt2A"))
  println(a)
  val b = a.map(auth => auth.works.map(sb => auth.getAuthorsBook(sb)(GEnvironment("xQXvrwOTLq7xonOLcjt2A"))))
  println(b)

}
