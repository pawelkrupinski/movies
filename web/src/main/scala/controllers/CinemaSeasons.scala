package controllers

import models.{Cinema, Opening}
import play.api.libs.json.{JsObject, Json, Writes}

import java.time.LocalDate

/** A seasonal venue's typical window, months 1..12 both inclusive, and whether `today`
 *  falls outside it: `{"cinema": "Freiluftkino Kreuzberg", "opens": 5, "closes": 9, "offSeason": true}`.
 *
 *  `offSeason` is decided HERE, on the server's injected clock, and every client reads it
 *  rather than re-deriving it from the months — so the web, iOS and Android can never
 *  disagree with each other or with the worker about which venues are out of season. */
case class ApiCinemaSeason(cinema: String, opens: Int, closes: Int, offSeason: Boolean)

object ApiCinemaSeason {
  implicit val writes: Writes[ApiCinemaSeason] = Json.writes[ApiCinemaSeason]
}

/** The seasonal venues among a page's cinemas, as both the `/api/cinemas` payload and the
 *  listing page's `window.CINEMA_SEASONS` carry them. */
object CinemaSeasons {
  /** `today` is the city's own day. Year-round venues are left out. */
  def of(cinemas: Seq[Cinema], today: LocalDate): Seq[ApiCinemaSeason] =
    cinemas.flatMap(cinema => cinema.opening match {
      case seasonal @ Opening.Seasonal(window) =>
        Some(ApiCinemaSeason(cinema.displayName, window.opens.getValue, window.closes.getValue, seasonal.offSeasonOn(today)))
      case Opening.YearRound => None
    })

  /** Keyed by display name, for the listing page's script: `{}` when there are none. */
  def byName(cinemas: Seq[Cinema], today: LocalDate): JsObject =
    JsObject(of(cinemas, today).map(season =>
      season.cinema -> Json.obj("opens" -> season.opens, "closes" -> season.closes, "offSeason" -> season.offSeason)))
}
