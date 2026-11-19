package controllers

import models.Country
import play.api.Mode
import play.api.mvc._
import services.review.TmdbPosterLookup

/** A review candidate's TMDB poster (`/debug/review/tmdb-poster/:tmdbId`), looked up live ([[TmdbPosterLookup]]; none
 *  without a TMDB key) in its country's language: a redirect to it, or not found -- the card's "no TMDB poster"
 *  placeholder then takes the image's place. Production 404s it. */
class TmdbPosterController(cc: ControllerComponents, environment: Mode, tmdbPosters: Option[TmdbPosterLookup])
  extends AbstractController(cc) {

  def tmdbPoster(tmdbId: Int, country: Option[String]): Action[AnyContent] = Action {
    DevMode.gate(environment) {
      val language = country.flatMap(Country.byCode).fold(java.util.Locale.UK)(_.language)
      tmdbPosters.flatMap(_.poster(tmdbId, language)).fold(NotFound("no TMDB poster"))(Redirect(_))
    }
  }
}
