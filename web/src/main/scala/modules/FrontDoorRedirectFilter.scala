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
 * A path under one of the root country's OWN places — `showtimes.cc/poznan/`, its
 * sub-pages, with or without the slash — 301s to the same address on that country's
 * own domain (`kinowo.net/poznan/`): Caddy sends every unmounted path on the apex to
 * the root deployment, which would otherwise serve its pages a second time under
 * showtimes.cc, canonical and all. Off the apex — `kinowo.net` itself — nothing is
 * touched. Every redirect is counted in [[LegacyAddressMetrics]] — as
 * `front_door_old_owner` only where the old-owner table changed the answer, the
 * one kind the table's drop rule reads; a Polish page served on the apex is no
 * redirect and is not counted.
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
      request.path.stripPrefix("/").split('/').toList match {
        case slug :: Nil if slug.nonEmpty =>
          Country.bareSlugTarget(slug).flatMap { target =>
            if (target.country == country) ownDomain(request)
            else {
              legacy.record(if (target.tableChangedAnswer) LegacyAddressKind.FrontDoorOldOwner else LegacyAddressKind.FrontDoorNew,
                            request)
              val path = s"${target.country.pathPrefix}/${target.city.slug}/"
              Some(if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}")
            }
          }
        case place :: _ if country.resolve(place).isDefined => ownDomain(request)
        case _ => None
      }
    }.flatten

  /** The same address on the root country's own domain. */
  private def ownDomain(request: RequestHeader): Option[String] = country.webOrigin.map(_ + request.uri)
}
