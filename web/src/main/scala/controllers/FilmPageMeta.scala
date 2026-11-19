package controllers

import models.{City, CityPath}
import play.api.mvc.RequestHeader

/** What a film page says about itself: its canonical address, its preview text, its share card and the address it is
 *  served at (`CityPath.filmUrl`, none for a film without a slug). */
final case class FilmPageMeta(canonicalUrl: String, preview: String, ogImageUrl: String, servedUrl: Option[String])

object FilmPageMeta {

  /** `schedule`'s page in `city`, asked for by `request`. `request.uri` would carry the raw inbound encoding; the
   *  canonical FilmHref form is used instead, so the og:url matches the link the page exposes elsewhere. Scheme/host
   *  come from PageMeta so the X-Forwarded-* workaround (Play 3.0's `request.secure` ignores the `trustedProxies` knob
   *  on this Fly setup) is in one place. The share card is the film's, or the city's while it has none
   *  ([[ShareCardUrl]]). */
  def of(schedule: FilmSchedule, request: RequestHeader, city: City): FilmPageMeta =
    FilmPageMeta(PageMeta.origin(request) + FilmHref.forSlug(schedule.slug, schedule.movie.title)(using city),
      FilmPreviewText.previewDescription(schedule), ShareCardUrl.forFilm(schedule, city),
      schedule.slug.flatMap(CityPath.filmUrl(city, _)))
}
