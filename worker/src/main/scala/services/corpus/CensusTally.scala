package services.corpus

import models.{City, Showtime, ShowtimesDigest, VenueClock}

import java.time.{Clock, Instant, LocalDateTime, ZoneId, ZoneOffset}

/** One census pass's running counts, film by film: the cards served per city and scope, the showtimes ahead per
 *  city, the unresolved films still screening and the widest film ([[CensusTally.at]]: against one instant). */
private[corpus] final class CensusTally private (windows: Map[String, CensusTally.Window], venueClock: VenueClock) {
  private val zoneCutoffs = scala.collection.mutable.HashMap.empty[ZoneId, Int]
  val served   = scala.collection.mutable.HashMap.empty[(String, String), Int].withDefaultValue(0)
  val upcoming = scala.collection.mutable.HashMap.empty[String, Int].withDefaultValue(0)
  var unresolvedScreening = 0
  var widest = 0

  private def zoneCutoff(zone: ZoneId) = zoneCutoffs.getOrElseUpdate(zone, CensusTally.upcomingAfter(venueClock.now(zone)))

  def add(film: FilmCensus): Unit = {
    if (film.cinemaSlots > widest) widest = film.cinemaSlots
    if (film.ready) film.cards.foreach(addCard)
    else if (film.screenable.exists(slot => slot.startsAfter(zoneCutoff(slot.zone)) > 0)) unresolvedScreening += 1
  }

  private def addCard(card: Seq[SlotCensus]): Unit = {
    val cardCities = scala.collection.mutable.HashMap.empty[String, (Boolean, Boolean)]
    card.foreach { slot =>
      slot.city.flatMap(slug => windows.get(slug).map(slug -> _)).foreach { case (slug, window) =>
        // Started on the venue's own clock, as the web cuts it (`StartedShowtimeCut`): a US city's venues across a
        // zone line keep their own. Tomorrow stays the city's day, as the web's tomorrow page reads it.
        val ahead = slot.startsAfter(zoneCutoff(slot.zone))
        upcoming(slug) += ahead
        val (anyAhead, anyTomorrow) = cardCities.getOrElse(slug, (false, false))
        cardCities(slug) = (anyAhead || ahead > 0, anyTomorrow || slot.startsWithin(window.tomorrowFrom, window.tomorrowUntil))
      }
    }
    cardCities.foreach { case (slug, (anyAhead, anyTomorrow)) =>
      if (anyAhead) served((slug, CensusSubsets.ServedAll)) += 1
      if (anyTomorrow) served((slug, CensusSubsets.ServedTomorrow)) += 1
    }
  }
}

private[corpus] object CensusTally {

  /** A tally of `cities` against `instant`: each city's local tomorrow cut on its own clock. */
  def at(cities: Seq[City], instant: Instant): CensusTally = {
    val venueClock = new VenueClock(Clock.fixed(instant, ZoneOffset.UTC))
    new CensusTally(cities.map { c =>
      val tomorrow = venueClock.nowIn(c).toLocalDate.plusDays(1)
      c.slug -> Window(ShowtimesDigest.startMinute(tomorrow.atStartOfDay), ShowtimesDigest.startMinute(tomorrow.plusDays(1).atStartOfDay))
    }.toMap, venueClock)
  }

  /** One city's local tomorrow on a tick: a start in `[tomorrowFrom, tomorrowUntil)`, in [[ShowtimesDigest.startMinute]]s. */
  final case class Window(tomorrowFrom: Int, tomorrowUntil: Int)

  /** The minute after which a start at `now` (venue-local) is still upcoming: a showtime is until [[Showtime.Grace]]
   *  past its start. Exact for starts on the minute, which every showtime a venue lists is. */
  private def upcomingAfter(now: LocalDateTime): Int = ShowtimesDigest.startMinute(now.minus(Showtime.Grace))
}
