package services.alerts

import services.fallback.{FallbackEvent, FallbackState}

import java.time.Duration

/**
 * Turns a fallback transition into the Telegram alert text — but only for the
 * events worth paging: ENTER (a cinema started being served by the fallback),
 * RECOVERED (its own scraper came back), and UNCOVERED (nothing is serving it at
 * all). PROBE_FAILED is routine backoff noise while a cinema stays down, so it
 * yields no alert.
 *
 * ENTER and RECOVERED are gated on `state.alerted`, which the scraper sets the
 * moment a cinema enters fallback — i.e. only after its own scraper has failed
 * continuously for the grace window (default 6h). A cinema that recovers while
 * still in that grace window never entered fallback, so it never set `alerted` and
 * stays silent; RECOVERED therefore only fires for an entry we actually paged.
 *
 * UNCOVERED is NOT gated on it: it is the first page this venue gets, because by
 * definition it never entered fallback. `SourceFallbackScraper` fires it once per
 * failing spell, so there is no flood to suppress here.
 */
object FallbackAlert {
  def messageFor(state: FallbackState, event: FallbackEvent): Option[String] = event.event match {
    case FallbackEvent.Enter if state.alerted =>
      Some(s"⚠️ ${state.cinema} — serving via ${state.fallbackSource} fallback\nReason: ${event.reason}")
    case FallbackEvent.Recovered if state.alerted =>
      Some(s"✅ ${state.cinema} — recovered, own scraper is back")
    // Worth a louder marker than ENTER: on ENTER the listing is still being served,
    // just from a sparser feed. Here it is not being served at all, and the usual
    // cause is that the venue stopped existing upstream — which only a person can
    // confirm and act on, by retiring it from the roster.
    case FallbackEvent.Uncovered =>
      Some(s"🚨 ${state.cinema} — down ${downFor(state, event)} and ${state.fallbackSource} has nothing " +
           s"to serve either, so nothing is covering it.\nReason: ${event.reason}\n" +
           "That is what a closed venue (or a retired upstream id) looks like — check whether it still exists.")
    case _ =>
      None
  }

  /** How long the primary has been down, for the page. Whole hours: the grace window
   *  is 6h, so minutes carry no information a reader would act on. */
  private def downFor(state: FallbackState, event: FallbackEvent): String =
    state.failingSince
      .map(since => s"${Duration.between(since, event.at).toHours}h")
      .getOrElse("the grace window")
}
