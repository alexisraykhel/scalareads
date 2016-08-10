package goodreads


import java.io.IOException

import goodreads.gstuff.{GDisjunction, IOError}

import scala.xml.{Elem, XML}
import scalaz.{-\/, \/-, \/}


case class Author(name: String, id: String)
//in the future wouldn't it be nice to get more info on an author! For now, let's do something easy
{
}

object Author {

  def getAuthorDeets(id: Int, devKey: String): GDisjunction[Author] = {
    val url: GDisjunction[Elem] = try {
      \/-(XML.load("https://www.goodreads.com/author/show/" + id.toString + "?format=xml&key=" + devKey))
    } catch {
      case i: IOException => -\/(IOError(i.toString))
    }

    for {
      x <- url
    } yield Author(x.\\("name").text, x.\\("id").text)
  }

}