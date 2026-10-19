package services.alerts

import models.Opening
import services.fallback.{FallbackEvent, FallbackState}

import java.time.{Duration, Instant}

/**
 * Turns a fallback transition into the Telegram alert text — but only for the
 * events worth paging: a venue that has been served by the fallback for
 * [[PageAfter]] (its own scraper is down for good, not for a morning), its
 * RECOVERED once such a spell ends, and UNCOVERED (nothing is serving it at all).
 *
 * ENTER IS NOT A PAGE. The fallback is serving the venue's showtimes, so entering it
 * asks nothing of anyone: in the week to 2026-10-19 PL and UK paged ENTER and then
 * RECOVERED four to seven hours later with nobody acting in between. A spell is paged
 * by the re-probe that first finds it [[PageAfter]] old — the scraper re-probes a
 * venue on fallback at least hourly, so one always crosses — and RECOVERED pages
 * only a spell that long, so every page that goes out has its pair. Both are read off
 * the spell's persisted history (its ENTER, and the transition before this one), so a
 * worker restart neither repeats nor loses one. PROBE_FAILED is otherwise routine
 * backoff noise.
 *
 * UNCOVERED is the first page this venue gets, because by definition it never
 * entered fallback. `SourceFallbackScraper` fires it once per failing spell, so there
 * is no flood to suppress here — except for a seasonal venue ([[Opening.Seasonal]]) off
 * its season, which nothing serving it is exactly what its winter looks like. A spell
 * suppressed that way is fired once more on its first in-season tick, and pages then.
 */
object FallbackAlert {
  /** How long a venue rides its fallback before that is worth a person's look. */
  val PageAfter: Duration = Duration.ofHours(24)

  def messageFor(state: FallbackState, event: FallbackEvent, opening: Opening): Option[String] = {
    // The spell's history before this transition (which the scraper prepends), and when it entered
    // fallback: `since` while it is on it; once RECOVERED clears that, its ENTER -- or, when the 50
    // kept transitions have pushed that out, the oldest one kept, which is no earlier than it.
    val earlier = if (state.history.headOption.contains(event)) state.history.tail else state.history
    val entered = state.since.orElse(earlier.find(_.event == FallbackEvent.Enter).orElse(earlier.lastOption).map(_.at))
    def onFallback = entered.map(Duration.between(_, event.at)).getOrElse(Duration.ZERO)
    event.event match {
      case FallbackEvent.ProbeFailed if crossesPageAfter(entered, earlier.headOption.map(_.at), event.at) =>
        Some(s"⚠️ ${state.cinema} — serving via ${state.fallbackSource} fallback for ${onFallback.toHours}h\nReason: ${event.reason}")
      case FallbackEvent.Recovered if onFallback.compareTo(PageAfter) >= 0 =>
        Some(s"✅ ${state.cinema} — recovered, own scraper is back")
      // Worth a louder marker than a long fallback: there the listing is still being served,
      // just from a sparser feed. Here it is not being served at all, and the usual
      // cause is that the venue stopped existing upstream — which only a person can
      // confirm and act on, by retiring it from the roster.
      case FallbackEvent.Uncovered if !opening.offSeasonAt(event.at) =>
        Some(s"🚨 ${state.cinema} — down ${downFor(state, event)} and ${state.fallbackSource} has nothing " +
             s"to serve either, so nothing is covering it.\nReason: ${event.reason}\n" +
             "That is what a closed venue (or a retired upstream id) looks like — check whether it still exists.")
      case _ =>
        None
    }
  }

  /** The `kind` a page counts under: the day-long spell's page is the spell's ENTER page, sent late. */
  def kindOf(event: FallbackEvent): TelegramAlertKind =
    TelegramAlertKind.fallback(if (event.event == FallbackEvent.ProbeFailed) FallbackEvent.Enter else event.event)

  /** Is `at` the first transition of the spell at or past [[PageAfter]] since its ENTER? */
  private def crossesPageAfter(entered: Option[Instant], previous: Option[Instant], at: Instant): Boolean =
    entered.exists { since =>
      val due = since.plus(PageAfter)
      !at.isBefore(due) && previous.forall(_.isBefore(due))
    }

  /** How long the primary has been down, for the page. Whole hours: the grace window
   *  is 6h, so minutes carry no information a reader would act on. */
  private def downFor(state: FallbackState, event: FallbackEvent): String =
    state.failingSince
      .map(since => s"${Duration.between(since, event.at).toHours}h")
      .getOrElse("the grace window")
}
