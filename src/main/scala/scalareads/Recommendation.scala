package scalareads

import scalareads.values.{GDisjunction, GEnvironment, SimpleBook, GResult}

case class Recommendation(recommended: List[SimpleBook]) extends GResult

object Recommendation {
  def apply(userId: Int)(env: GEnvironment): GDisjunction[Recommendation] = {
    User(userId)(env).map(u => makeRecommendation(u))
  }

  private def makeRecommendation(u: User): Recommendation = ???
}
