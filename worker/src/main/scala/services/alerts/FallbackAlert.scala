package services.alerts

import services.fallback.{FallbackEvent, FilmwebFallbackState}

/**
 * Turns a Filmweb-fallback transition into the Telegram alert text — but only for
 * the events worth paging: ENTER (a cinema started being served by Filmweb) and
 * RECOVERED (its own scraper came back). PROBE_FAILED is routine backoff noise
 * while a cinema stays down, so it yields no alert.
 */
object FallbackAlert {
  def messageFor(state: FilmwebFallbackState, event: FallbackEvent): Option[String] = event.event match {
    case FallbackEvent.Enter =>
      Some(s"⚠️ ${state.cinema} — serving via Filmweb fallback\nReason: ${event.reason}")
    case FallbackEvent.Recovered =>
      Some(s"✅ ${state.cinema} — recovered, own scraper is back")
    case _ =>
      None
  }
}
