package services.fallback

import java.time.Instant

/**
 * One transition in a cinema's Filmweb-fallback history, newest-first in
 * `FilmwebFallbackState.history`. Persisted so the /uptime/fallback page can show
 * not just the current state but how it got there.
 */
case class FallbackEvent(at: Instant, event: String, reason: String)

object FallbackEvent {
  /** Primary first failed/emptied and Filmweb took over. */
  val Enter = "ENTER"
  /** Periodic re-probe of the primary while on fallback — still down. */
  val ProbeFailed = "PROBE_FAILED"
  /** Primary came back; fallback released. */
  val Recovered = "RECOVERED"
}

/**
 * Per-cinema Filmweb-fallback state. The worker owns it (it runs the scrape and
 * decides transitions); the web process reads it for the /uptime/fallback status
 * page. Keyed by `cinema` (the `Cinema.displayName`, same key UptimeMonitor uses)
 * so the two views line up.
 *
 * A cinema gets a document only once it has entered fallback at least once — a
 * healthy cinema has no row. `active=false` with history is a cinema that
 * recovered.
 */
case class FilmwebFallbackState(
  cinema:              String,
  active:              Boolean,
  filmwebCinemaId:     Option[Int],
  since:               Option[Instant],   // when the CURRENT active spell began
  lastReason:          Option[String],
  consecutiveFailures: Int,               // primary failures since entering; drives backoff
  lastPrimaryProbeAt:  Option[Instant],
  nextPrimaryProbeAt:  Option[Instant],   // before this, serve Filmweb without re-probing the primary
  updatedAt:           Instant,
  history:             List[FallbackEvent]
)

object FilmwebFallbackState {
  /** Cap on retained history entries per cinema, oldest dropped first. */
  val MaxHistory: Int = 50
}
