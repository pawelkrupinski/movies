package controllers

import models.{City, Showtime, UsRoster}

import java.time.LocalDateTime

/**
 * When a city's showtimes count as started — `Showtime.isUpcoming`'s cutoff — judged at each
 * venue on that venue's own clock. A city keeps one zone, but a US metro across a zone line
 * holds venues an hour either side of it (`UsRoster.venueZones`): on the city's clock Russell
 * Springs' 21:00 show dropped at 20:30 its time, before it began. Built once per render from
 * the few venues off their city's clock; every other venue costs one map lookup.
 */
final class StartedShowtimeCut private (cityCut: LocalDateTime, venueCuts: Map[models.Cinema, LocalDateTime]) {

  /** The cutoff for `cinema`'s showtimes: a showtime is upcoming when it is after this. */
  def at(cinema: models.Cinema): LocalDateTime = venueCuts.getOrElse(cinema, cityCut)

  /** The latest cutoff any of the city's venues has — what a schedule's earliest showtime must
   *  still be after for nothing in it to have lapsed. */
  val latest: LocalDateTime = venueCuts.valuesIterator.foldLeft(cityCut)((a, b) => if (b.isAfter(a)) b else a)
}

object StartedShowtimeCut {
  /** The cut at `now`, the wall-clock time in `city`'s zone. */
  def apply(city: City, now: LocalDateTime): StartedShowtimeCut = {
    val instant = now.atZone(city.zoneId)
    // The same venues, and the same precedence, `VenueClock.zoneOf` reads.
    val venueCuts = UsRoster.venueZones.iterator
      .collect { case (cinema, zone) if zone != city.zoneId && City.forCinema(cinema).contains(city) =>
        cinema -> instant.withZoneSameInstant(zone).toLocalDateTime.minus(Showtime.Grace) }
      .toMap
    new StartedShowtimeCut(now.minus(Showtime.Grace), venueCuts)
  }
}
