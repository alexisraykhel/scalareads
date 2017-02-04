package scalareads

import org.http4s.dsl._
import org.http4s.HttpService

import scalareads.values.{AuthorRequest, GEnvironment}

final case class ScalareadsService() {

  def httpService: HttpService = HttpService {
    //info page
    case req@GET -> Root / "scalareads" => Ok(???)
      //actual author
    case req@POST -> Root / "scalareads" / "author" =>
      Ok(AuthorRequest(req)((authorId: String, devKey: String) => (authorId, GEnvironment(devKey, ???))))
      //actual user
    case req@POST -> Root / "scalareads" / "user" => Ok(???)
      //actual book
    case req@POST -> Root / "scalareads" / "book" => Ok(???)
  }
}
