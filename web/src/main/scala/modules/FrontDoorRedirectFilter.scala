package modules

import controllers.PageMeta
import models.Country
import services.metrics.{LegacyAddressKind, LegacyAddressMetrics}
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
 * touched. Every bare slug answered is counted in [[LegacyAddressMetrics]] by
 * whether the old-owner table decided it.
 */
class FrontDoorRedirectFilter(country: Country, legacy: LegacyAddressMetrics)(using val mat: Materializer)
    extends Filter {

  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] =
    target(request) match {
      case Some(path) => Future.successful(Results.MovedPermanently(path))
      case None       => next(request)
    }

  private def target(request: RequestHeader): Option[String] =
    Option.when(country.servesApex(PageMeta.host(request))) {
      request.path.stripPrefix("/").stripSuffix("/") match {
        case slug if slug.nonEmpty && !slug.contains('/') =>
          val answered = Country.bareSlugTarget(slug)
          answered.foreach { target =>
            legacy.record(if (target.viaLegacyOwner) LegacyAddressKind.FrontDoorOldOwner else LegacyAddressKind.FrontDoorNew,
                          request)
          }
          answered.filter(_.country != country).map { target =>
            val path = s"${target.country.pathPrefix}/${target.city.slug}/"
            if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}"
          }
        case _ => None
      }
    }.flatten
}
