package scalareads.general


import scalareads.values.SimpleBook
import scalareads.recommender.{UnscaledShelfishness, Tag}


trait UserBook {
  val popularShelves: List[(Tag,Int)]
  val sb: SimpleBook
}

object UserBook {

  def measureShelfishness(ub: UserBook,
                          ts: List[Tag]): UnscaledShelfishness = {


    val matchingShelves: List[(Tag, Int)] = for {
      tag <- ub.popularShelves
      giventag <- ts
      if tag._1 == giventag
    } yield tag

    UnscaledShelfishness(ub.sb, sortShelves(ub, ts, matchingShelves))
  }
 private def totalShelves(ub: UserBook) =
   ub.popularShelves.foldRight(0.0)((tup, b) => tup._2 + b)

  private def sortShelves(ub: UserBook,
                          ts: List[Tag],
                          matches: List[(Tag, Int)]) = {
    val allShelves = ts.filter(t =>
      matches.exists(tup => t == tup._1))
        .map((_, 0)) ++ matches

    allShelves.map(tup =>
      (tup._1, tup._2.toDouble / totalShelves(ub))).sortBy(_._1.s)
  }
}

final case class ReadBook(sb: SimpleBook,
                          book: Book,
                          usersTags: List[Tag],
                          popularShelves: List[(Tag, Int)],
                          userRating: Option[Int],
                          averageRating: Option[Double]) extends UserBook

final case class ToReadBook(sb: SimpleBook,
                            popularShelves: List[(Tag, Int)],
                            averageRating: Option[Double]) extends UserBook
