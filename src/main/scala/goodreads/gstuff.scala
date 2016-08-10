package goodreads

import scalaz.\/

package object gstuff {

  type GDisjunction[A] = \/[GError, A]

  sealed trait GError
  final case class IOError(m: String) extends GError

}