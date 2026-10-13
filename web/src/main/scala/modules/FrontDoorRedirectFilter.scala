package modules

import controllers.PageMeta
import models.Country
import org.apache.pekko.stream.Materializer
import play.api.mvc.{Filter, RequestHeader, Result, Results}

import scala.concurrent.Future

/**
 * On the brand FRONT DOOR — `showtimes.cc/` itself, which the deployment mounted
 * at the root answers ([[Country.servesApex]]) — a bare `/{slug}/` naming a place
 * of a country mounted under it 301s to that country's page: `showtimes.cc/york/`
 * → `/uk/york/`. Which country owns a slug several of them answer to is
 * [[Country.frontDoorTarget]]'s rule (the most populous town of that name).
 *
 * Only a single bare segment, and only one the root country itself does not
 * answer: that country's own pages stay its own. Off the apex — `kinowo.net`, the
 * root country's own domain — nothing is touched.
 */
class FrontDoorRedirectFilter(country: Country)(using val mat: Materializer) extends Filter {

  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] =
    target(request) match {
      case Some(path) => Future.successful(Results.MovedPermanently(path))
      case None       => next(request)
    }

  private def target(request: RequestHeader): Option[String] =
    Option.when(country.servesApex(PageMeta.host(request))) {
      request.path.stripPrefix("/").stripSuffix("/") match {
        case slug if slug.nonEmpty && !slug.contains('/') && country.resolve(slug).isEmpty =>
          Country.frontDoorTarget(slug).map { case (owner, city) =>
            val path = s"${owner.pathPrefix}/${city.slug}/"
            if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}"
          }
        case _ => None
      }
    }.flatten
}
