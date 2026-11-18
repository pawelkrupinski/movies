package services.metrics

import models.Country
import services.movies.{ScreeningsRepository, SlotKeyed, VenueRoster}

import java.time.{Clock, ZoneId}

/** The upcoming showtimes on screenings rows filed under venues the roster no longer lists — what
 *  [[RetiredVenueCensus]] reports as `kinowo_worker_retired_venue_future_showtimes`. */
final class RetiredVenueShowtimes(screenings: ScreeningsRepository, country: Country, clock: Clock) {

  private val venueClock = new models.VenueClock(clock)

  // "Future" is judged on each venue's OWN clock: a retired Los Angeles venue's 21:30 is still ahead
  // when New York is past midnight. `City.all` never lists a venue once it is retired from the
  // roster, so a city's clock is found only for a venue still on it; one no city lists falls back
  // to the country's first city.
  private val fallbackZone: ZoneId = country.cities.headOption.map(_.zoneId).getOrElse(java.time.ZoneOffset.UTC)
  private def zoneOf(venue: String): ZoneId =
    models.City.all.find(_.cinemas.exists(_.displayName == venue)).fold(fallbackZone)(_.zoneId)

  /** Upcoming showtimes on `retiredRows`, or None when their showtimes could not be read. */
  def on(retiredRows: Set[String]): Option[Int] =
    if (retiredRows.isEmpty) Some(0)
    else {
      val slotKeysByFilm = retiredRows.groupMap(SlotKeyed.filmIdOf)(SlotKeyed.slotKeyOf)
      screenings.findForFilmsChecked(slotKeysByFilm.keySet).answered.map { byFilm =>
        val nowAt = scala.collection.mutable.Map.empty[String, java.time.LocalDateTime]
        def now(venue: String) = nowAt.getOrElseUpdate(venue, venueClock.now(zoneOf(venue)))
        slotKeysByFilm.iterator.map { case (filmId, keys) =>
          val showtimes = byFilm.getOrElse(filmId, Map.empty)
          keys.iterator.map { key =>
            val venueNow = now(VenueRoster.venueOf(key))
            showtimes.getOrElse(key, Nil).count(_.isUpcoming(venueNow))
          }.sum
        }.sum
      }
    }
}
