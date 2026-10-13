package modules

import controllers.PageMeta
import models.Country
import org.apache.pekko.stream.Materializer
import play.api.mvc.{Filter, RequestHeader, Result, Results}

import scala.concurrent.Future

/**
 * On the brand FRONT DOOR — `showtimes.cc/` itself, which the deployment mounted
 * at the root answers ([[Country.servesApex]]) — a bare `/{slug}/` naming a place
 * of another country 301s to that country's page: `showtimes.cc/kent/` →
 * `/uk/kent/`. Which country a bare slug means is [[Country.bareSlugTarget]]'s
 * rule — the country that owned it before slugs were per country, else the one
 * that has it, else the first in order — the same one the apps read.
 *
 * Only a single bare segment; one that means the root country's own place is left
 * to it. Off the apex — `kinowo.net`, the root country's own domain — nothing is
 * touched.
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
        case slug if slug.nonEmpty && !slug.contains('/') =>
          Country.bareSlugTarget(slug).filter(_.country != country).map { target =>
            val path = s"${target.country.pathPrefix}/${target.city.slug}/"
            if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}"
          }
        case _ => None
      }
    }.flatten
}
