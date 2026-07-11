package services.fallback

import java.time.Instant

/**
 * One transition in a cinema's Filmweb-fallback history, newest-first in
 * `FilmwebFallbackState.history`. Persisted so the /uptime page's Filmweb-fallback
 * section can show not just the current state but how it got there.
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
 * decides transitions); the web process reads it for the /uptime page's
 * Filmweb-fallback section. Keyed by `cinema` (the `Cinema.displayName`, same key UptimeMonitor uses)
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
  failingSince:        Option[Instant] = None, // when the CURRENT run of continuous primary failures began — the grace clock; we only enter fallback once it has lasted `fallbackAfter`. Persisted so a worker restart doesn't reset it. Cleared on any primary success.
  since:               Option[Instant],   // when the CURRENT active (serving-via-Filmweb) spell began
  lastReason:          Option[String],
  consecutiveFailures: Int,               // primary failures since entering; drives backoff
  lastPrimaryProbeAt:  Option[Instant],
  nextPrimaryProbeAt:  Option[Instant],   // before this, serve Filmweb without re-probing the primary
  updatedAt:           Instant,
  history:             List[FallbackEvent],
  alerted:             Boolean = false    // ENTER page sent for the CURRENT spell — gates the recovery page so a grace-window recovery (never entered fallback) stays silent
)

object FilmwebFallbackState {
  /** Cap on retained history entries per cinema, oldest dropped first. */
  val MaxHistory: Int = 50
}
