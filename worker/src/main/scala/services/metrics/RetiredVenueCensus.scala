package services.metrics

import io.prometheus.metrics.core.metrics.Gauge
import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Country
import services.movies.{RetiredVenueRows, ScreeningsRepository, SlotKeyed, SlotsRepository}

import java.time.{Clock, LocalDateTime}
import scala.concurrent.duration._

/**
 * Counts the side-collection rows (`screenings`, `movie_slots`) filed under a venue this
 * country's roster no longer lists, and the FUTURE showtimes among them.
 *
 * Dropping a cinema from the roster stops its scrape, and the read path ignores its rows, but
 * until [[RetiredVenueRows]] (2026-09-23) nothing removed them: after "Kino Etiuda OBK" left the
 * PL roster, 3 films and 8 future showtimes stayed under it for days, counted by every census
 * that reads the side collections raw, and nothing said so. This is the watchdog for that sweep:
 * it runs daily and spares rows written in the last 24h, so a count above zero for over two days
 * means it refused, failed or stopped.
 *
 * Reads only row ids (never documents) from both collections, then the showtimes of the few
 * films with retired rows. A read that fails publishes nothing for that store — "could not
 * read" is not "none" — and an EMPTY roster publishes nothing at all: that is a roster that
 * failed to load, and every row would read as retired. Venue and roster come from
 * [[VenueRoster.venuesOf]] and [[RetiredVenueRows.venueOf]], the definitions its cleanup sweep uses.
 */
class RetiredVenueCensus(
  screenings:      ScreeningsRepository,
  slots:           SlotsRepository,
  roster:          Set[String],
  rows:            Gauge,
  futureShowtimes: Gauge,
  country:         Country,
  clock:           Clock = Clock.systemUTC(),
  override protected val sampleInterval: FiniteDuration = RetiredVenueCensus.DefaultSampleInterval
) extends SampledCensus {
  import RetiredVenueCensus._

  private val countryCode = country.code

  // A retired venue sits in no city, so "future" is judged on the country's first city's clock —
  // off by at most a zone's width in a multi-zone country, which a watchdog on a count can bear.
  private val zone = country.cities.headOption.map(_.zoneId).getOrElse(java.time.ZoneOffset.UTC)

  Collections.foreach(coll => rows.labelValues(countryCode, coll).set(0.0))
  futureShowtimes.labelValues(countryCode).set(0.0)

  def sample(): Unit =
    if (roster.isEmpty) logger.warn(s"$censusName: the $countryCode roster is EMPTY — not counting any venue as retired.")
    else {
      val (slotIds, slotsRead) = slots.rowIdsChecked()
      if (slotsRead) rows.labelValues(countryCode, SlotsRepository.Collection).set(retiredOf(slotIds).size.toDouble)
      val (screeningIds, screeningsRead) = screenings.rowIdsChecked()
      if (screeningsRead) {
        val retiredRows = retiredOf(screeningIds)
        rows.labelValues(countryCode, ScreeningsRepository.Collection).set(retiredRows.size.toDouble)
        futureOf(retiredRows).foreach(n => futureShowtimes.labelValues(countryCode).set(n.toDouble))
      }
    }

  private def retiredOf(ids: Set[String]): Set[String] = ids.filterNot(id => roster(RetiredVenueRows.venueOf(id)))

  /** Upcoming showtimes on `retiredRows`, or None when their showtimes could not be read. */
  private def futureOf(retiredRows: Set[String]): Option[Int] =
    if (retiredRows.isEmpty) Some(0)
    else {
      val slotKeysByFilm = retiredRows.groupMap(SlotKeyed.filmIdOf)(id => id.drop(SlotKeyed.filmIdOf(id).length + 1))
      val (byFilm, read) = screenings.findForFilmsChecked(slotKeysByFilm.keySet)
      Option.when(read) {
        val now = LocalDateTime.now(clock.withZone(zone))
        slotKeysByFilm.iterator.map { case (filmId, keys) =>
          val showtimes = byFilm.getOrElse(filmId, Map.empty)
          keys.iterator.map(key => showtimes.getOrElse(key, Nil).count(_.isUpcoming(now))).sum
        }.sum
      }
    }

  override protected val censusName: String = "retired-venue-census"
}

object RetiredVenueCensus {
  val RowsName            = "kinowo_worker_retired_venue_side_rows"
  val FutureShowtimesName = "kinowo_worker_retired_venue_future_showtimes"

  private val Collections = Seq(ScreeningsRepository.Collection, SlotsRepository.Collection)

  /** Hourly: the rows appear the moment a venue leaves the roster and should go with the next
   *  cleanup, so this moves on the scale of deploys, not minutes. Two id-only reads per tick. */
  val DefaultSampleInterval: FiniteDuration = 1.hour

  /** The two shared gauges every country's census writes into, registered once. */
  def gauges(registry: PrometheusRegistry): (Gauge, Gauge) = {
    val rows = Gauge.builder()
      .name(RowsName)
      .help("Side-collection rows (collection=screenings|movie_slots) filed under a venue this country's roster no longer lists, by country. Nothing serves them, and only the daily retired-venue sweep (RetiredVenueRows) deletes them: before it, Kino Etiuda OBK's rows stayed for days after it left the PL roster, invisible. Zero is healthy; above zero for over two days (the cleanup's 24h grace plus its daily tick) means a removed venue's rows outlived it. Hourly, id-only reads; a failed read keeps the last value. Alerted by RetiredVenueRowsLingering.")
      .labelNames("country", "collection")
      .register(registry)
    val future = Gauge.builder()
      .name(FutureShowtimesName)
      .help("Upcoming showtimes on screenings rows filed under a venue this country's roster no longer lists, by country — how much of kinowo_worker_retired_venue_side_rows every census reading the side collections raw still counts as live. Zero is healthy.")
      .labelNames("country")
      .register(registry)
    (rows, future)
  }
}
