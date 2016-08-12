package goodreads

import scalaz._

package object gstuff {

  type GDisjunction[A] = \/[GError, A]
  type GReader[A] = Reader[GEnvironment, A]

  final case class GEnvironment(devKey: String) extends AnyVal

  sealed trait Simplified
  final case class SimpleBook(isbn: String, title: String) extends Simplified
  final case class SimpleAuthor(id: Int, name: String) extends Simplified

  sealed trait GError
  final case class IOError(m: String) extends GError

}