package services.cinemas

import models.Cinema

/** What a single cinema scrape tick resolved to, as classified by
 *  [[UptimeRecordingScraper]]: a fetch with screenings (`Success`), one that
 *  parsed cleanly but yielded zero (`Empty`), or one that threw (`Failure`).
 *  The same three outcomes the `UptimeMonitor` records. */
sealed trait ScrapeOutcome
object ScrapeOutcome {
  case object Success extends ScrapeOutcome
  case object Empty   extends ScrapeOutcome
  case object Failure extends ScrapeOutcome
}

/** Observes each cinema's per-tick scrape outcome — a second sink alongside
 *  `UptimeMonitor` so a cross-cutting watcher (e.g. the Filmweb-drop alerter)
 *  can react without `UptimeRecordingScraper` knowing who's listening (OCP). The
 *  default is a no-op, so the production wrap and every test that doesn't care
 *  stay untouched. */
trait ScrapeOutcomeListener {
  def onOutcome(cinema: Cinema, outcome: ScrapeOutcome): Unit
}

object ScrapeOutcomeListener {
  val NoOp: ScrapeOutcomeListener = (_, _) => ()
}
