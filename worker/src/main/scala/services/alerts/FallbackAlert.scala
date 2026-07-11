package services.alerts

import services.fallback.{FallbackEvent, FilmwebFallbackState}

/**
 * Turns a Filmweb-fallback transition into the Telegram alert text — but only for
 * the events worth paging: ENTER (a cinema started being served by Filmweb) and
 * RECOVERED (its own scraper came back). PROBE_FAILED is routine backoff noise
 * while a cinema stays down, so it yields no alert.
 *
 * Both pages are gated on `state.alerted`, which the scraper sets the moment a
 * cinema enters fallback — i.e. only after its own scraper has failed continuously
 * for the grace window (default 6h). A cinema that recovers while still in that
 * grace window never entered fallback, so it never set `alerted` and stays silent;
 * RECOVERED therefore only fires for an entry we actually paged.
 */
object FallbackAlert {
  def messageFor(state: FilmwebFallbackState, event: FallbackEvent): Option[String] = event.event match {
    case FallbackEvent.Enter if state.alerted =>
      Some(s"⚠️ ${state.cinema} — serving via Filmweb fallback\nReason: ${event.reason}")
    case FallbackEvent.Recovered if state.alerted =>
      Some(s"✅ ${state.cinema} — recovered, own scraper is back")
    case _ =>
      None
  }
}
