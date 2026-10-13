package modules

import controllers.{PageMeta, ServedCity}
import models.Country
import services.metrics.{LegacyAddressKind, LegacyAddressMetrics}
import org.apache.pekko.stream.Materializer
import play.api.i18n.Messages
import play.api.mvc.{Filter, RequestHeader, Result, Results}

import scala.concurrent.Future

/**
 * On the brand FRONT DOOR — `showtimes.cc/` itself, which the deployment mounted
 * at the root answers ([[Country.servesApex]]) — a bare `/{slug}/` naming a place
 * of another country 301s to that country's page: `showtimes.cc/harrogate/` →
 * `/uk/harrogate/`, and a renamed or aliased slug in one hop to the page it now
 * resolves to (`/kent/` → `/uk/chatham/`, the page Kent's towns are on). Which
 * country a bare slug means is [[Country.bareSlugTarget]]'s rule — the country
 * that owned it before slugs were per country, else the one that has it, else
 * the first in order — the same one the apps read.
 *
 * A path under one of the root country's OWN places — `showtimes.cc/poznan/`, its
 * sub-pages, with or without the slash, a renamed or aliased Polish slug, a bare
 * slug the rule above gives to Poland — is a 404: Polish places live on
 * `kinowo.net` and nowhere else, and Caddy sends every unmounted path on the apex
 * to the root deployment, which would otherwise serve its pages a second time
 * under showtimes.cc. This filter sits AHEAD of [[RenamedCityRedirectFilter]]
 * (`AppLoader.filterChain`) so a renamed Polish slug is a 404 too, rather than a
 * 301 onto one. The 404 is exactly the one an unknown city gets on the apex
 * ([[ServedCity.unknown]], in the front door's English, `controllers.FrontDoorMessages`) — `showtimes.cc/poznan/`
 * reads like `showtimes.cc/xyzzy/`, not a bare status with an empty body. The
 * apex's own root, files and endpoints (`/`, `/robots.txt`, `/.well-known/…`,
 * `/assets/…`, `/api/…`) name no place and pass through. Off the apex —
 * `kinowo.net` itself — nothing is touched.
 *
 * Every redirect is counted in [[LegacyAddressMetrics]] — as
 * `front_door_old_owner` only where the old-owner table changed the answer, the
 * one kind the table's drop rule reads. A Polish place's 404 is no legacy address
 * and is not counted.
 */
class FrontDoorRedirectFilter(country: Country, legacy: LegacyAddressMetrics, frontDoorMessages: Messages)(
    using val mat: Materializer)
    extends Filter {

  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] =
    answer(request).getOrElse(next(request))

  private def answer(request: RequestHeader): Option[Future[Result]] =
    Option.when(country.servesApex(PageMeta.host(request))) {
      request.path.stripPrefix("/").split('/').toList match {
        case slug :: Nil if slug.nonEmpty =>
          Country.bareSlugTarget(slug).map { target =>
            if (target.country == country) notHere(slug)
            else {
              legacy.record(if (target.tableChangedAnswer) LegacyAddressKind.FrontDoorOldOwner else LegacyAddressKind.FrontDoorNew,
                            request)
              val path = s"${target.country.pathPrefix}/${target.city.slug}/"
              Future.successful(
                Results.MovedPermanently(if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}"))
            }
          }
        case place :: _ if country.resolve(place).isDefined => Some(notHere(place))
        case _ => None
      }
    }.flatten

  /** A place of the root country, which lives on its own domain only. */
  private def notHere(place: String): Future[Result] = Future.successful(ServedCity.unknown(place)(using frontDoorMessages))
}
