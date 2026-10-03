package services.cinemas.common

import java.time.{Clock, LocalDate, LocalTime, ZoneId, ZoneOffset}

/**
 * Which calendar day a venue is on — asked at each scrape, in the venue's own zone.
 *
 * Scrapers are built once, at the worker's boot, and live as long as it does. Handing each
 * one a `LocalDate` froze every venue on its boot day: a worker up a week planned from a
 * week ago (Helios bakes that date into its REST URLs; a day walk read seven blank past
 * days first), and a US venue was given POLAND's day, so a boot between midnight and
 * ~09:00 Warsaw dropped that US evening's still-running screenings from a listing marked
 * complete — and the prune deleted them. Clients take `today` by name, and the catalogue
 * answers it from here per read, in the zone the venue keeps its own calendar in.
 */
final class ScrapeCalendar(clock: Clock) {
  /** Today in `zone`, now. */
  def today(zone: ZoneId): LocalDate = LocalDate.ofInstant(clock.instant(), zone)

  /** Today where `cinema` is: its city's zone, or `fallback` for a venue no city lists. A
   *  country-wide zone is wrong for a country that spans several — the US spans six, and a
   *  New York "today" between midnight and 03:00 Eastern is a day Los Angeles has not
   *  reached, so its still-running evening went unplanned. */
  def todayAt(cinema: models.Cinema, fallback: ZoneId): LocalDate =
    today(models.City.forCinema(cinema).fold(fallback)(_.zoneId))

  /** Today in Poland — the zone every Polish venue (and Helios's REST date) keeps. */
  def todayInPoland: LocalDate = today(ScrapeCalendar.Poland)
}

object ScrapeCalendar {
  val Poland: ZoneId = ZoneId.of("Europe/Warsaw")

  /** The live calendar a worker scrapes by. */
  def system: ScrapeCalendar = new ScrapeCalendar(Clock.systemUTC())

  /** A calendar stopped on `day` everywhere — a fixture replay's capture date. Noon UTC is
   *  `day` in every zone from UTC-11 to UTC+11, so a pinned replay reads the same date for a
   *  Polish venue and a Los Angeles one. */
  def fixedOn(day: LocalDate): ScrapeCalendar =
    new ScrapeCalendar(Clock.fixed(day.atTime(LocalTime.NOON).toInstant(ZoneOffset.UTC), ZoneOffset.UTC))
}
