package services.alerts

import services.fallback.{FallbackEvent, FilmwebFallbackState}

/**
 * Turns a Filmweb-fallback transition into the Telegram alert text — but only for
 * the events worth paging: ENTER (a cinema started being served by Filmweb) and
 * RECOVERED (its own scraper came back). PROBE_FAILED is routine backoff noise
 * while a cinema stays down, so it yields no alert.
 *
 * Both pages are gated on `state.alerted`, which the scraper only sets once a
 * cinema has been on fallback for its alert threshold (default 3h): a brief blip
 * never pages its ENTER, and a quick recovery never pages a RECOVERED for an entry
 * we stayed silent about. So RECOVERED only fires when it recovered more than the
 * threshold after falling back.
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
