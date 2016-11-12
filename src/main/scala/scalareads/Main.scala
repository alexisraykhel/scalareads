package scalareads

import scalareads.recommender.BookPrediction
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
      |Expected usage: scalareads [--user id] [--book id] [--author id] --devkey string --pathToResources string
    """.stripMargin

  private def commandLineParse(l: List[String]): GDisjunction[KeyAndResult] = {

    val b: List[\/[GError, (GEnvironment) => \/[GError, GResult]]] = l.sliding(2,1).toList.collect {
      case List("--user", userId: String) => stringToInt(userId).map(User(_)_)
      case List("--book", bookId: String) => \/-(Book(bookId)_)
      case List("--author", authorId: String) => stringToInt(authorId).map(i => Author(i)_)
    }

    val d: Option[GEnvironment] = l.sliding(4, 1).toList.collect {
      case List("--devkey", devKey: String, "--pathToResources", path: String) => GEnvironment(devKey, path)
    }.headOption


    d.fold(CommandLineError("No developer key found.").left[KeyAndResult])(ge => {
      (ge, b.map(disjs => disjs.flatMap(x => {
        val gresult = x(ge)
        gresult.map( {
          case u@User(_,_,_,_, _) => {
            val validator = predicts(ge, u)
            println("validator: " + validator)
            val nearest = BookPrediction(ge)(u)

            val x = for {
              v <- validator
            } yield meanSquaredError(v)

            println("\nMean squared error: " + x)
            nearest.fold(er => println("Error! Error!" + er.toString), op =>
              op.fold(println("You need to get some more books on your read/to-read shelves!"))(nb =>
                println("Your book with the highest predicted rating is: " + nb)))
          }
          case _ => println("That's embarrassing.")
        })
        gresult
      }))).right[CommandLineError]
    })
  }
}
