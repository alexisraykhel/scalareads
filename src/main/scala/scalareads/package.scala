package scalareads

import scalaz.{\/, Reader}


package object values {

  type GDisjunction[A] = \/[GError, A]
  type GReader[A] = Reader[GEnvironment, A]
  type KeyAndResult = (GEnvironment, List[GDisjunction[GResult]])

  final case class GEnvironment(devKey: String) extends AnyVal

  //Author, Book, User all extend GResult
  trait GResult

  sealed trait Simplified
  final case class SimpleBook(id: String, title: String) extends Simplified {
    def getBook(env: GEnvironment): GDisjunction[Book] = Book(this.id)(env)
  }
  final case class SimpleAuthor(id: String, name: String) extends Simplified
  final case class SimpleUser(id: String, name: String) extends Simplified

  sealed trait GError
  final case class IOError(m: String) extends GError
  final case class ToIntError(m: String) extends GError
  final case class CommandLineError(m: String) extends GError

}