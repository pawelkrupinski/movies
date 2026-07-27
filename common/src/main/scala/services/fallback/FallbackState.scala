package services.fallback

import java.time.Instant

/**
 * One transition in a cinema's fallback history, newest-first in
 * `FallbackState.history`. Persisted so the /uptime page's fallback section can
 * show not just the current state but how it got there.
 */
case class FallbackEvent(at: Instant, event: String, reason: String)

object FallbackEvent {
  /** Primary first failed/emptied and the fallback source took over. */
  val Enter = "ENTER"
  /** Periodic re-probe of the primary while on fallback — still down. */
  val ProbeFailed = "PROBE_FAILED"
  /** Primary came back; fallback released. */
  val Recovered = "RECOVERED"
}

/**
 * Per-cinema fallback state. The worker owns it (it runs the scrape and decides
 * transitions); the web process reads it for the /uptime page's fallback section.
 * Keyed by `cinema` (the `Cinema.displayName`, same key UptimeMonitor uses) so the
 * two views line up.
 *
 * Source-neutral: a Polish own-site venue falls back to Filmweb, a UK chain venue
 * (Cineworld/Vue/Odeon/…) falls back to the flicks.co.uk aggregator. `fallbackSource`
 * names which — it drives the /uptime "served via <X>" label and the Telegram alert
 * text — and `fallbackRef` carries that source's opaque per-venue handle (a Filmweb
 * numeric id, a flicks slug) for the status page's outbound link.
 *
 * A cinema gets a document only once it has entered fallback at least once — a
 * healthy cinema has no row. `active=false` with history is a cinema that
 * recovered.
 */
case class FallbackState(
  cinema:              String,
  active:              Boolean,
  fallbackSource:      String,             // human name of the fallback feed — "Filmweb", "Flicks", … — shown on /uptime + in alerts
  fallbackRef:         Option[String],     // that feed's opaque per-venue handle (Filmweb id, flicks slug); for the status-page link
  failingSince:        Option[Instant] = None, // when the CURRENT run of continuous primary failures began — the grace clock; we only enter fallback once it has lasted `fallbackAfter`. Persisted so a worker restart doesn't reset it. Cleared on any primary success.
  since:               Option[Instant],   // when the CURRENT active (serving-via-fallback) spell began
  lastReason:          Option[String],
  consecutiveFailures: Int,               // primary failures since entering; drives backoff
  lastPrimaryProbeAt:  Option[Instant],
  nextPrimaryProbeAt:  Option[Instant],   // before this, serve the fallback without re-probing the primary
  updatedAt:           Instant,
  history:             List[FallbackEvent],
  alerted:             Boolean = false    // ENTER page sent for the CURRENT spell — gates the recovery page so a grace-window recovery (never entered fallback) stays silent
)

object FallbackState {
  /** Cap on retained history entries per cinema, oldest dropped first. */
  val MaxHistory: Int = 50

  /** The default fallback feed for venues whose fallback source isn't recorded —
   *  every pre-existing row was a Polish own-site→Filmweb fallback, so old Mongo
   *  documents (written before `fallbackSource` existed) read back as Filmweb. */
  val DefaultSource: String = "Filmweb"
}
