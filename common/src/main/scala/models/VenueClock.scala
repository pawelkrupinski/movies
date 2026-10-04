package models

import java.time.{Clock, Instant, LocalDate, LocalDateTime, LocalTime, ZoneId, ZoneOffset}

/**
 * The ONE source of local dates and times in production code: what day and hour it is at a
 * venue, in a city, or in a named zone — read from the injected [[Clock]] at each ask.
 *
 * Two bugs kept recurring wherever code read the time by itself. A date taken once froze: scrapers
 * are built at the worker's boot and live as long as it does, so a `LocalDate` handed to each froze
 * every venue on its boot day (Helios baked it into its REST URLs; a day walk read seven blank past
 * days). And a date taken in the wrong zone moved venues a day: a US venue given POLAND's day (or
 * New York's, for Los Angeles; Madrid's, for the Canaries) dropped its still-running evening from
 * a listing marked complete, and a metric judged "upcoming" in the pod's UTC. Every ask goes
 * through here, per call, in the zone the venue keeps its own calendar in (`NoDefaultZoneSpec`
 * fails the build on any other way of reading one).
 */
final class VenueClock(clock: Clock) {
  def instant: Instant = clock.instant()

  /** Today in `zone`, now. */
  def today(zone: ZoneId): LocalDate = LocalDate.ofInstant(clock.instant(), zone)

  /** The wall-clock time in `zone`, now. */
  def now(zone: ZoneId): LocalDateTime = LocalDateTime.ofInstant(clock.instant(), zone)

  /** Today in `city`'s zone. */
  def todayIn(city: City): LocalDate = today(city.zoneId)

  /** The wall-clock time in `city`'s zone — what its city-local `Showtime.dateTime`s compare with. */
  def nowIn(city: City): LocalDateTime = now(city.zoneId)

  /** Today where `cinema` is: its own zone where it keeps one apart from its city
   *  ([[UsRoster.venueZones]]), else its city's, else `fallback` for a venue no city lists.
   *  A country-wide zone is wrong for a country that spans several — the US spans six. */
  def todayAt(cinema: Cinema, fallback: ZoneId): LocalDate = today(VenueClock.zoneOf(cinema, fallback))

  /** The wall-clock time where `cinema` is (its own zone, else its city's, else `fallback`). */
  def nowAt(cinema: Cinema, fallback: ZoneId): LocalDateTime = now(VenueClock.zoneOf(cinema, fallback))

  /** Today in Poland — the zone every Polish venue (and Helios's REST date) keeps. */
  def todayInPoland: LocalDate = today(TimeZones.Poland)
}

object VenueClock {
  /** The live clock a deployed process reads — built once, at a composition root. */
  def system: VenueClock = new VenueClock(Clock.systemUTC())

  /** A clock stopped on `day` everywhere — a fixture replay's capture date. Noon UTC is `day` in
   *  every zone from UTC-11 to UTC+11, so a pinned replay reads the same date for a Polish venue
   *  and a Los Angeles one. */
  def fixedOn(day: LocalDate): VenueClock =
    new VenueClock(Clock.fixed(day.atTime(LocalTime.NOON).toInstant(ZoneOffset.UTC), ZoneOffset.UTC))

  /** The zone `cinema` keeps its calendar in: its own ([[UsRoster.venueZones]]), else its
   *  city's, else `fallback`. */
  def zoneOf(cinema: Cinema, fallback: ZoneId): ZoneId =
    UsRoster.venueZones.getOrElse(cinema, City.forCinema(cinema).fold(fallback)(_.zoneId))
}
