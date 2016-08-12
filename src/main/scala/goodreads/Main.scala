package goodreads

import goodreads.gstuff.GEnvironment

object Main extends App{

  val a = Author(1077326)(GEnvironment(""))
  println(a)
  println(a.map(auth => auth.works.map(sb => auth.getAuthorsBook(sb))))

}
