package scalareads

import scalareads.values.SimpleBook
import scalareads.recommender.{UnscaledShelfishness, Tag}

trait UserBook {
  def measureShelfishness(l: List[Tag]): UnscaledShelfishness = {

    val popularShelves = this match {
      case ToReadBook(_, ps, _) => ps
      case ReadBook(_, _, _, ps, _, _) => ps
    }

    val book = this match {
      case ToReadBook(sb, _, _) => sb
      case ReadBook(sb, _, _, _, _, _) => sb
    }

    val totalShelves = popularShelves.foldRight(0.0)((tup, b) => tup._2 + b)

    val matchingShelves: List[(Tag, Int)] = for {
      tag <- popularShelves
      giventag <- l
      if tag._1 == giventag
    } yield tag

    val allShelves = l.filter(t => matchingShelves.exists(tup => t == tup._1)).map(t => (t, 0)) ++ matchingShelves

    UnscaledShelfishness(book, allShelves.map(tup => (tup._1, tup._2.toDouble / totalShelves)).sortBy(_._1.s))
  }
}

final case class ReadBook(simpleBook: SimpleBook,
                          book: Book,
                          usersTags: List[Tag],
                          popularShelves: List[(Tag, Int)],
                          userRating: Option[Int],
                          averageRating: Option[Double]) extends UserBook

final case class ToReadBook(book: SimpleBook,
                            popularShelves: List[(Tag, Int)],
                            averageRating: Option[Double]) extends UserBook