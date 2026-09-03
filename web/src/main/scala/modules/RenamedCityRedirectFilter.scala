package modules

import org.apache.pekko.stream.Materializer
import play.api.mvc.{Filter, RequestHeader, Result, Results}

import scala.concurrent.Future

/**
 * 301s every path under a city that changed slug to the same path under its
 * current one — `/us/san-francisco/movie/dune` → `/us/san-francisco-bay-area/movie/dune`.
 *
 * A city slug is a PUBLISHED URL: it is in the sitemap, in whatever Google has
 * indexed, in the `city` cookie of everyone who has visited, and in the share
 * cards named after it. Renaming one without this leaves every one of those
 * pointing at `withCity`'s 404, which is why the film-path rename
 * (`/film` → `/movie`) shipped with its own permanent redirects rather than a
 * flag day.
 *
 * A FILTER rather than a branch in `MovieController.withCity` because the slug
 * appears in fifteen routes — the index, the film pages, three `/api/…`
 * endpoints the mobile apps call, both OG-image endpoints — and `withCity` is
 * handed only the slug, never the path it has to rewrite. One filter covers
 * every route that exists now and every one added later; the alternative is
 * remembering this at fifteen call sites.
 *
 * The rewrite is confined to the CITY segment: the path is split after the
 * deployment's mount point, and only that first segment is looked up. A film
 * whose own slug collides with a renamed city's is therefore untouched.
 */
class RenamedCityRedirectFilter(mountPath: String)(using val mat: Materializer) extends Filter {

  /** The mount point without its trailing slash — `/us`, or `""` for a
   *  deployment served at the root. */
  private val mount: String = mountPath.stripSuffix("/")

  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] =
    renamed(request.path) match {
      case Some(path) =>
        val target = if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}"
        Future.successful(Results.MovedPermanently(target))
      case None => next(request)
    }

  /** The path this one redirects to, or `None` when its city segment is not a
   *  renamed slug (the overwhelmingly common case — one map lookup per
   *  request). */
  private def renamed(path: String): Option[String] =
    Option.when(path.startsWith(s"$mount/")) {
      val rest              = path.substring(mount.length + 1)
      val (city, remainder) = rest.span(_ != '/')
      models.City.renamedSlugs.get(city).map(current => s"$mount/$current$remainder")
    }.flatten
}
