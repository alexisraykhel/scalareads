package goodreads

import scalaz._

package object gstuff {

  type GDisjunction[A] = \/[GError, A]
  type GReader[A] = Reader[GEnvironment, A]

  final case class GEnvironment(devKey: String) extends AnyVal

  trait GResult

  sealed trait Simplified
  final case class SimpleBook(isbn: String, title: String) extends Simplified
  final case class SimpleAuthor(id: String, name: String) extends Simplified
  final case class SimpleUser(id: String, name: String) extends Simplified

  sealed trait GError
  final case class IOError(m: String) extends GError
  final case class ToIntError(m: String) extends GError
  final case class CommandLineError(m: String) extends GError

  def optionToInt(o: Option[String]): Option[Int] =
    o.fold(Option.empty[Int])(s => try {
      Some(s.toInt)
      } catch {
        case e: NumberFormatException => Option.empty[Int]
        }
    )

  def stringToInt(s: String): GDisjunction[Int] = try {
    \/-(s.toInt)
  } catch {
    case e: NumberFormatException => -\/(ToIntError(s + " is not a valid integer. " + e.getMessage))
  }

  def optionToDouble(o: Option[String]): Option[Double] =
    o.fold(Option.empty[Double])(s => try {
      Some(s.toDouble)
    } catch {
      case e: NumberFormatException => Option.empty[Double]
    }
    )

}