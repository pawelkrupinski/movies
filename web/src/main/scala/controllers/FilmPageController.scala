package controllers

import models.*
import play.api.mvc.*

/** The film pages: `/{city}/movie/:slug`, and the pre-slug `/{city}/movie?title=…` address. Rendered for nobody, as
 *  the listing is ([[MovieController]]): no `UserRepository`, so no response can vary by session. */
class FilmPageController(cc: ControllerComponents,
                         cityRequests: CityRequests,
                         // A film page's address resolved to its schedule, and its other cities' pages.
                         filmLinks: FilmDeepLinks,
                         oauthProviders: Set[String],
                         // Rewrites the page's inline `<script>`/`<style>` blocks (`Minifier.forMode`).
                         minifier: tools.Minifier,
                         // The third-party page tags, read per render so a live config change reaches the page.
                         pageTags: () => PageTags,
                         // Each film page served — its client's user agent and the page's URL
                         // (`CityPath.filmUrl`) — for the record of the pages Facebook's crawler
                         // fetched (`FacebookFetchRecorder`); nowhere unless the composition root says.
                         filmPageServed: (Option[String], String) => Unit = (_, _) => (),
                        ) extends AbstractController(cc) {

  import cityRequests.{cityCookie, deploymentMessages, withCity}

  import ResponseBody.okHtml

  /** The canonical film page, addressed by slug. */
  def filmBySlug(city: String, slug: String): Action[AnyContent] = Action { request =>
    withCity(city, request) { implicit c =>
      filmLinks.filmBySlug(c, slug) match {
        // A slug that reached the film but is not its address — a former billing-title
        // address, or a stale fold the fallback re-slugged — 301s onto the address, so a
        // shared link keeps working and crawlers consolidate on one URL per film.
        case Some(schedule) if schedule.slug.exists(_ != slug) =>
          val path = FilmHref.forSlug(schedule.slug, schedule.movie.title)
          MovedPermanently(if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}")
        case Some(schedule) => renderFilm(schedule, request)
        case None           => NotFound(s"Film not found: $slug")
      }
    }
  }

  /** The pre-slug `?title=…` address. Kept routable indefinitely — it is what
   *  every link minted before the switch carries, including the ~10k URLs the
   *  old sitemap put in search indexes and the share links installed app builds
   *  still generate — but answered with a 301 so crawlers consolidate on the
   *  slug and users land on the canonical address. */
  def film(city: String, title: String): Action[AnyContent] = Action { request =>
    withCity(city, request) { implicit c =>
      filmLinks.film(c, title) match {
        // A title with no usable slug has no other address to offer, so it
        // renders here rather than 301-ing to itself.
        case Some(schedule) if schedule.slug.isDefined =>
          MovedPermanently(FilmHref.forSlug(schedule.slug, schedule.movie.title))
        case Some(schedule) => renderFilm(schedule, request)
        case None           => NotFound(s"Film not found: $title")
      }
    }
  }

  private def renderFilm(schedule: FilmSchedule, request: Request[AnyContent])(implicit c: City): Result = {
    implicit val messages: play.api.i18n.Messages = deploymentMessages
    val meta = FilmPageMeta.of(schedule, request, c)
    // Sibling cities currently showing this same film — the cross-links a
    // near-duplicate per-city page needs so it isn't only reachable through
    // the sitemap.
    val otherCities = filmLinks.otherCities(schedule, c)
    // Nobody is rendered into this page either, so `no-cache` (revalidate, keep
    // the browser copy, bfcache works) replaces the `no-store` a signed-in render
    // used to need. It stops short of offering itself to a shared cache only
    // because a per-film edge entry wants its own validator analysis, not because
    // the bytes are anyone's.
    meta.servedUrl.foreach(filmPageServed(request.headers.get(USER_AGENT), _))
    okHtml(views.html.film(schedule, meta.canonicalUrl, meta.preview, meta.ogImageUrl, minifier, oauthProviders, otherCities, pageTags()))
      .withHeaders("Cache-Control" -> "private, no-cache")
      .withCookies(cityCookie(c))
  }
}
