package controllers

import models.City
import play.api.mvc.RequestHeader

import java.time.LocalDateTime

/** The city listing's HTML (`views.html.repertoire`): the schedules [[ListingResponse]] built, with the OG meta derived
 *  from the request's `?…` filter parameters, the canonical folded back onto the bare listing and the listing's
 *  JSON-LD. Renders for nobody: nothing here can ask who made the request ([[MovieController]]). */
final class ListingHtml(
  oauthProviders: Set[String],
  // The navbar's Debug tab: on outside production (`DevMode.enabled`).
  devMode: Boolean,
  // Rewrites every page's inline `<script>`/`<style>`
  // blocks — the composition root's choice for this mode
  // (`Minifier.forMode`), so its memo caches live and die
  // with the wiring rather than the JVM.
  minifier: tools.Minifier,
  // The third-party page tags (`fb:app_id`, GA, Sentry). A
  // function read per render rather than a value fixed at
  // boot, so a live config change reaches the page.
  pageTags: () => PageTags,
  // Rendered film cards kept across renders of a listing (see
  // `FilmCardFragments`); the templates find it as their implicit.
  filmCards: FilmCardFragments,
) {
  private implicit val fragments: FilmCardFragments = filmCards

  private val cityStructuredData = new CityStructuredData

  /** How many listings' JSON-LD this is holding — for the spec. */
  private[controllers] def structuredDataHeld: Long = cityStructuredData.size

  /** `city`'s listing of `schedules`, pruned at `now` — also the page's own expiry countdown's start
   *  (`_repertoireView` counts forward from `renderedAt`, so it has to be the instant the schedules were
   *  actually pruned at). */
  def render(city: City, request: RequestHeader, now: LocalDateTime, schedules: Seq[FilmSchedule])(
      implicit messages: play.api.i18n.Messages): play.twirl.api.Html = {
    implicit val c: City = city
    val meta        = FilterDescription.forIndex(city, request.queryString, schedules)
    val isLargeCity = MovieControllerService.totalShowtimes(schedules) > MovieControllerService.LargeCityShowtimeThreshold
    val pageUrl     = PageMeta.canonicalUrl(request)
    views.html.repertoire(
      schedules,
      city.cinemaDisplayNames,
      city.cinemaPillMap,
      devMode, minifier, oauthProviders, renderedAt = now,
      isLargeCity     = isLargeCity,
      pageTitle       = meta.title,
      pageDescription = meta.description,
      pageUrl         = pageUrl,
      pageTags        = pageTags(),
      // og:url keeps the filtered request URL (so a shared filtered link
      // previews the filter), but the canonical folds `/{city}/movies` and every
      // `?filter` variation back to the bare listing.
      canonicalUrl    = PageMeta.listingUrl(request, city),
      structuredData  = Some(cityStructuredData.of(pageUrl, city, schedules)),
    )
  }
}
