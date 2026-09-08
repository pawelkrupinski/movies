package services.tasks

import models.{Cinema, CinemaMovie, City}

import java.time.{Clock, Duration => JDuration, LocalDateTime}
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._

/**
 * Shortens a THIN venue's own next scrape interval when its just-landed listing
 * won't last until the country's normal cadence would otherwise return.
 *
 * `kinowo_web_movies_served` counts films with a showing strictly in the future
 * right now ([[models.Showtime.isUpcoming]]); a venue whose freshest listing
 * covers only a day or two of screenings mechanically drains toward zero as
 * those screenings pass, then jumps back up whenever the NEXT scrape lands a
 * new listing — with the venue's real catalogue never having changed. Measured
 * 2026-09-08 against every country's own `screenings`: 113 US, 15 ES, 14 DE and
 * 3 UK cinemas currently hold LESS remaining showtime runway than that
 * country's own `KINOWO_SCRAPE_FRESHNESS_MINUTES` cadence (pl=60min, uk=420,
 * es=420, de=600, us=840) — a few of them (Cinema One Antlers, McCurtain Cinema
 * Idabel, Slickrock Cinema Moab) confirmed as durant/moab's nightly drain, with
 * no `[scrape-depth]`/`[scrape-prune]` guard log at the trough: neither guard is
 * involved, this is a distinct third mechanism from Hood River's breadth-guard
 * gap or Zamora's depth-guard hold (see [[services.movies.ScrapeHealth]]).
 *
 * `DueWindow.periodFor` already supports a per-key period ([[services.cadence.RatingCadence]]
 * uses it for the adaptive rating interval); this feeds it a period derived
 * fresh from each landed scrape rather than learned from history, because
 * "how much runway is left" is fully known the instant the scrape lands — no
 * backoff/streak state to track. Deliberately NOT persisted: a restart just
 * re-learns a venue's override on its very next scrape, which happens within
 * one country cadence regardless, so a Mongo round-trip per scrape would buy
 * nothing durable.
 */
object VenueScrapeCadence {

  /** Floor under the shortened interval — far above the reaper's own 1-minute
   *  tick (so a razor-thin venue doesn't come due on literally every tick) and
   *  far below any country's normal cadence (60min-840min), so it still buys a
   *  real improvement over the flat default for the venues that need it. */
  val MinInterval: FiniteDuration = 30.minutes

  /** The interval before this venue's next scrape, given how much showtime
   *  runway its freshest listing has left. Plenty of runway (>= the country's
   *  own default) keeps that default unchanged — this only ever SHORTENS,
   *  never lengthens, a venue's cadence. A thin venue is re-scraped at HALF its
   *  remaining runway, not all of it: scraping exactly when the last showtime
   *  ends leaves no margin if that attempt is itself late or fails, which is
   *  exactly the shape that emptied Zamora even before the depth-guard bug
   *  compounded it. Clamped at [[MinInterval]]. */
  def periodFor(remainingHorizon: FiniteDuration, countryDefault: FiniteDuration): FiniteDuration =
    if (remainingHorizon >= countryDefault) countryDefault
    else (remainingHorizon / 2).max(MinInterval).min(countryDefault)

  /** How much showtime runway a venue's freshest listing has left, measured on
   *  the venue's OWN city clock — `Showtime.dateTime` is city-local wall-clock
   *  time (the same assumption `WebMovieMetrics.countsFor` makes), so comparing
   *  it against a UTC "now" would misjudge every non-Polish venue by its zone
   *  offset. Falls back to the clock's own zone for a cinema `City.forCinema`
   *  can't place (defensive; every scraped cinema is in some city's roster in
   *  practice). Zero for an empty listing or one whose latest
   *  showtime has already passed — the most urgent case, not a "no data" one. */
  def remainingHorizonOf(cinema: Cinema, movies: Seq[CinemaMovie], clock: Clock): FiniteDuration = {
    val zone = City.forCinema(cinema).map(_.zoneId).getOrElse(clock.getZone)
    val now  = LocalDateTime.now(clock.withZone(zone))
    movies.iterator.flatMap(_.showtimes).map(_.dateTime).maxOption match {
      case None     => Duration.Zero
      case Some(dt) =>
        val millis = JDuration.between(now, dt).toMillis
        if (millis <= 0) Duration.Zero else millis.millis
    }
  }
}

/**
 * Per-venue [[VenueScrapeCadence]] override, shared between the scrape reaper
 * (which builds the `DueWindow` this feeds) and [[ScrapeFreshnessPolicy]]
 * (which records it after every successful scrape) — the same "one shared
 * instance" `DueWindow` itself requires of the reaper/handler pair, extended to
 * this third party. `key` is the scrape dedup key ([[ScrapeCinemaHandler.dedupKey]]),
 * so it lines up with what `DueWindow.isDue` is already called with.
 */
class VenueCadenceStore(countryDefault: FiniteDuration) {
  private val overrides = TrieMap.empty[String, FiniteDuration]

  /** Record this venue's freshly observed runway, deriving and storing the
   *  period it implies. */
  def record(key: String, remainingHorizon: FiniteDuration): Unit =
    overrides.put(key, VenueScrapeCadence.periodFor(remainingHorizon, countryDefault))

  /** The period `DueWindow` should use for `key` — the country default until a
   *  scrape has recorded a shorter one. */
  def periodFor(key: String): FiniteDuration = overrides.getOrElse(key, countryDefault)
}
