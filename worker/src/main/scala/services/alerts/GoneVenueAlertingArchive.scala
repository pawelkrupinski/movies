package services.alerts

import models.Cinema
import services.scrapes.{BarrenAttempt, ForwardingScrapeArchive, GoneUpstream, ScrapeArchiveRepository, ScrapeOutcome}

/**
 * The scrape archive, plus one page when a venue with no fallback has answered
 * "page gone" (404/410, [[GoneUpstream.saysPageIsGone]]) on [[GoneVenueAlertingArchive.FailedRuns]]
 * separate scrape runs in a row. Every read and every archiving rule is the wrapped
 * archive's; the run count is its `BarrenAttempt.failedRuns`, so it survives the
 * worker restarts that a 10-hourly venue sees between runs.
 *
 * A venue WITH a fallback is left to `SourceFallbackScraper`, which already pages
 * UNCOVERED when neither side can serve it, and a Filmweb-only venue to
 * `FilmwebDropAlerter`. Only a page-gone answer counts: an
 * aggregator outage (5xx, 403, timeouts) fails every venue it lists at once, and
 * one page per venue would bury the one that means something.
 *
 * Pages on the attempt whose run brings the count ONTO the threshold — not on the
 * retries of that run, which leave it there — so a spell pages once and a later
 * spell (after any run that fetched) can page again, with no state of its own.
 */
final class GoneVenueAlertingArchive(
  underlying:         ScrapeArchiveRepository,
  // Venues another alert already pages for (a fallback's UNCOVERED, the Filmweb-drop alert).
  pagedElsewhere:     Set[String],
  notify:             String => Unit
) extends ForwardingScrapeArchive(underlying) {
  import GoneVenueAlertingArchive._

  override protected def storeBarren(cinema: Cinema, city: Option[String], attempt: BarrenAttempt): Unit = {
    val watched = attempt.outcome == ScrapeOutcome.Failed && !pagedElsewhere(cinema.displayName)
    // The count before this attempt, so only the attempt that MOVES it onto the
    // threshold pages: the retries of that run leave it there, and would page again.
    val before  = if (watched) underlying.find(cinema).flatMap(_.lastBarren).flatMap(_.failedRuns) else None
    // Filed as every forwarding archive files it, so the source's word that it lists no schedule reaches the store.
    super.storeBarren(cinema, city, attempt)
    if (watched && !before.contains(FailedRuns))
      // Read back rather than re-derive, so the count paged on is the one the archive
      // holds. Neither step can fail the scrape: the Mongo archive's reads and the
      // Telegram send each handle their own failures.
      underlying.find(cinema).flatMap(_.lastBarren).flatMap(messageFor(cinema, _)).foreach(notify)
  }
}

object GoneVenueAlertingArchive {
  /** Several runs, not one: a single 404 can be an aggregator redeploying badly. */
  val FailedRuns: Int = 3

  private[alerts] def messageFor(cinema: Cinema, barren: BarrenAttempt): Option[String] =
    Option.when(barren.failedRuns.contains(FailedRuns) && GoneUpstream.saysPageIsGone(barren.error))(
      s"🚨 ${cinema.displayName} — its page has answered gone on $FailedRuns scrape runs in a row " +
      s"(failing since ${barren.runStartedAt}), and no fallback covers it.\n" +
      s"Reason: ${barren.error.getOrElse("")}\n" +
      "That is what a closed venue (or a retired upstream id) looks like — check whether it still exists.")
}
