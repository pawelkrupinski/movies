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

  /** The latest cutoff any of the city's venues has — the safe cut for a showtime whose venue is
   *  unknown. */
  val latest: LocalDateTime = venueCuts.valuesIterator.foldLeft(cityCut)((a, b) => if (b.isAfter(a)) b else a)

  /** The earliest showtime of each venue in `showtimes`, kept as [[noneLapsed]] reads them: one
   *  instant for every venue on the city's clock, one per venue off it. Which venues are off the
   *  city's clock depends only on the city, so a later cut of the same city reads it right. */
  def earliest(showtimes: Iterator[(models.Cinema, LocalDateTime)]): StartedShowtimeCut.Earliest = {
    var onCityClock = LocalDateTime.MAX
    val offClock    = scala.collection.mutable.Map.empty[models.Cinema, LocalDateTime]
    showtimes.foreach { case (cinema, at) =>
      if (venueCuts.contains(cinema)) offClock.updateWith(cinema)(held => Some(held.filter(_.isBefore(at)).getOrElse(at)))
      else if (at.isBefore(onCityClock)) onCityClock = at
    }
    StartedShowtimeCut.Earliest(onCityClock, offClock.toMap)
  }

  /** Whether no showtime summarised by `earliest` has started at this cut — each judged on its
   *  venue's own clock. Against the LATEST cut alone, a venue behind its city's clock kept a
   *  showtime the city's cut had passed, and every render rebuilt its film's schedule for up to an
   *  hour. */
  def noneLapsed(earliest: StartedShowtimeCut.Earliest): Boolean =
    cityCut.isBefore(earliest.onCityClock) && earliest.offClock.forall { case (cinema, at) => this.at(cinema).isBefore(at) }
}

object StartedShowtimeCut {
  /** A schedule's earliest showtimes as [[StartedShowtimeCut.noneLapsed]] judges them. */
  final case class Earliest(onCityClock: LocalDateTime, offClock: Map[models.Cinema, LocalDateTime])

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
