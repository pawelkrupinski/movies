package modules

import models.Country
import services.metrics.{LegacyAddressKind, LegacyAddressMetrics}
import org.apache.pekko.stream.Materializer
import play.api.mvc.{Filter, RequestHeader, Result, Results}

import scala.concurrent.Future

/**
 * 301s every path under a slug that is not a page of THIS deployment's country
 * but names one — `/us/san-francisco-bay-area/movie/dune` → `/us/san-francisco/movie/dune`.
 *
 * Two kinds of slug, both the country's own ([[Country.redirectFor]]):
 *  - a slug a city USED to answer at ([[Country.renamedSlugs]]): a retired page's,
 *    or one a page was qualified to while another country held its name. Those
 *    are PUBLISHED — the sitemap, whatever a search engine indexed, the `city`
 *    cookie of everyone who visited, the share cards named after them;
 *  - another name of a place a page covers ([[Country.slugAliases]]): a town on a
 *    cluster's page (`/uk/ilkley/`), a page's qualified slug. Nobody should hit a
 *    404 for a place the site lists.
 *
 * Only the deployment's own country is consulted: every country owns its slugs,
 * so `/uk/york/` is a UK page whatever the US calls York, and one country's
 * retired slug never moves another's live page.
 *
 * A FILTER rather than a branch in `MovieController.withCity` because the slug
 * appears in fifteen routes — the index, the film pages, three `/api/…`
 * endpoints the mobile apps call, both OG-image endpoints — and `withCity` is
 * handed only the slug, never the path it has to rewrite.
 *
 * The rewrite is confined to the CITY segment: the path is split after the
 * deployment's mount point, and only that first segment is looked up. A film
 * whose own slug collides with a renamed city's is therefore untouched. A bare
 * city segment with no trailing slash lands on the listing's canonical slash
 * form in the one hop, rather than in two.
 *
 * Each redirect is counted by its kind ([[Country.redirect]]) in
 * [[LegacyAddressMetrics]], the signal for when these addresses can go.
 */
class RenamedCityRedirectFilter(country: Country, legacy: LegacyAddressMetrics)(using val mat: Materializer)
    extends Filter {

  /** The mount point without its trailing slash — `/us`, or `""` for a
   *  deployment served at the root. */
  private val mount: String = country.mountPath.stripSuffix("/")

  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] =
    renamed(request.path) match {
      case Some((path, kind)) =>
        legacy.record(LegacyAddressKind.of(kind), request)
        val target = if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}"
        Future.successful(Results.MovedPermanently(target))
      case None => next(request)
    }

  /** The path this one redirects to and the kind of old address it is, or `None`
   *  when its city segment is not a slug this country redirects (the overwhelmingly
   *  common case — a map lookup per request). */
  private def renamed(path: String): Option[(String, models.SlugRedirectKind)] =
    Option.when(path.startsWith(s"$mount/")) {
      val rest              = path.substring(mount.length + 1)
      val (city, remainder) = rest.span(_ != '/')
      country.redirect(city).map(r => s"$mount/${r.to}${if (remainder.isEmpty) "/" else remainder}" -> r.kind)
    }.flatten
}
