package scalareads

import scalareads.recommender.Validator._
import scalareads.values._
import ScalareadsFunctions._
import scalaz.{\/, \/-, ImmutableArray}
import scalaz.concurrent.{Task, TaskApp}
import scalaz.syntax.traverse._
import scalaz.syntax.either._

object Main extends TaskApp {

  override def run(args: ImmutableArray[String]): Task[Unit] = {

    if (args.length == 0) Task(println(usage))
    else Task(println(commandLineParse(args.toList).map(tup => tup._2)))
    //use main for recommendation, use classes straight up as library
  }

  val usage =
    """
      |Expected usage: scalareads [--user id] [--book id] [--author id] --devkey string
    """.stripMargin

  private def commandLineParse(l: List[String]): GDisjunction[KeyAndResult] = {

    val b: List[\/[GError, (GEnvironment) => \/[GError, GResult]]] = l.sliding(2,1).toList.collect {
      case List("--user", userId: String) => {

        val c: \/[GError, (GEnvironment) => GDisjunction[User]] = stringToInt(userId).map(i => User(i)_)
        c
      }
      case List("--book", bookId: String) => \/-(Book(bookId)_)
      case List("--author", authorId: String) => stringToInt(authorId).map(i => Author(i)_)
//      case List("--recommendToRead", userId: String) => stringToInt(userId).map(i => Recommendation(i)_)
    }

    val d: Option[GEnvironment] = l.sliding(2, 1).toList.collect {
      case List("--devkey", string: String) => GEnvironment(string)
    }.headOption


    d.fold(CommandLineError("No developer key found.").left[KeyAndResult])(ge => {
      (ge, b.map(disjs => disjs.flatMap(x => {
        val gresult = x(ge)
        gresult.map( {
          case u@User(_,_,_,_, _) => {
            val validator = predicts(ge, u)
//            val nearest = BookPrediction(ge)(u)

            println("Validator: " + validator)

            val x = for {
              v <- validator
            } yield meanSquaredError(v.toList)

            println("Mean squared error: " + x)
//            nearest.fold(er => er, op =>
//              op.fold(println("You need to get some more books on your read/to-read shelves!"))(nb =>
//                println("Your book with the highest predicted rating is: " + nb)))
//            println(u.readShelf(ge).map(l => l.length))
          }
          case _ => println("That's embarrassing.")
        })
        gresult
      }))).right[CommandLineError]
    })
  }
}
