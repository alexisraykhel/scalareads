package goodreads

import goodreads.gstuff._

import scalaz.{-\/, \/, \/-, ImmutableArray}
import scalaz.concurrent.{Task, TaskApp}
import scalaz.syntax.traverse._
import scalaz.syntax.either._

object Main extends TaskApp {

  val usage =
    """
      |Expected usage: scalareads [--user id] [--book isbn] [--author id] --devkey string
    """.stripMargin

  override def run(args: ImmutableArray[String]): Task[Unit] = {

    if (args.length == 0) Task(println(usage))
    else {

      type KeyAndResult = (GEnvironment, List[GDisjunction[GResult]])
      val argList = args.toList
      def listMatch(l: List[String]): GDisjunction[KeyAndResult] = {

        val b: List[\/[GError, (GEnvironment) => \/[GError, GResult]]] = l.sliding(2,1).toList.collect {
          case List("--user", userId: String) => {

            val c: \/[GError, (GEnvironment) => GDisjunction[User]] = gstuff.stringToInt(userId).map(i => User(i)_)
            c
          }
          case List("--book", isbn: String) => \/-(Book(isbn)_)
          case List("--author", authorId: String) => gstuff.stringToInt(authorId).map(i => Author(i)_)
        }

        val d: Option[GEnvironment] = l.sliding(2, 1).toList.collect {
          case List("--devkey", string: String) => GEnvironment(string)
        }.headOption


        d.fold(CommandLineError("No developer key found.").left[KeyAndResult])(ge => {
          (ge, b.map(disjs => disjs.flatMap(_(ge)))).right[CommandLineError]
        })
      }

      Task(println(listMatch(argList)))
    }

    //printlog instead of printing stuff
    //use main for recommendation, use classes straight up as library
  }
}
