package services.movies

import models.Cinema

/** What the scrape-health guards remember about one venue between ticks.
 *
 *  `sourceKey` is the upstream listing (`CinemaScraper.sourceKey`) whose scrape last
 *  LANDED — the source the stored corpus came from, so a tick from a different source
 *  is a rewire rather than a degraded fetch (see [[ScrapeHealth.isRewire]]). `None`
 *  for a venue no scrape has recorded one for yet.
 *
 *  The two counters are how many ticks running each guard has rejected the venue
 *  (see [[ScrapeHealth.depth]] / [[ScrapeHealth.breadth]]). */
final case class ScrapeGuardState(
  sourceKey:         Option[String] = None,
  depthRejections:   Int            = 0,
  breadthRejections: Int            = 0
)

object ScrapeGuardState {
  val Fresh: ScrapeGuardState = ScrapeGuardState()
}

/**
 * Where the scrape-health guards keep [[ScrapeGuardState]] between ticks.
 *
 * It has to OUTLIVE the process. The guards' grace — "reject a thin tick N times,
 * then accept it" — only ever ends if the rejection count survives from one tick to
 * the next, and ticks are an hour (PL) to fourteen hours (US) apart: longer than a
 * worker pod reliably lives on a busy deploy day. Held in memory, the count reset on
 * every rollout — Braniewo's Baszta, 2026-09-23, counted 1, 2, 1, 1, 1 across the
 * day's pod changes and never reached its give-up threshold, so the site kept
 * serving a wrong source's showtimes. The same goes for the recorded source key: a
 * rewire must still be recognised on the first tick after a restart.
 *
 * [[InMemoryScrapeGuardLedger]] for tests and Mongo-less wiring;
 * `services.scrapes.MongoScrapeGuardLedger` in production. Neither holds rules — the
 * decisions live in [[ScrapeHealth]] and `ScrapeLanding`.
 */
trait ScrapeGuardLedger {
  /** The venue's remembered state, [[ScrapeGuardState.Fresh]] when there is none — and None
   *  when it could not be READ. The two are not the same answer: a venue judged as Fresh
   *  whose state is then written back resets the rejection count it was carrying. */
  def get(cinema: Cinema): Option[ScrapeGuardState]
  def put(cinema: Cinema, state: ScrapeGuardState): Unit
}

final class InMemoryScrapeGuardLedger extends ScrapeGuardLedger {
  private val byCinema = scala.collection.concurrent.TrieMap.empty[String, ScrapeGuardState]
  def get(cinema: Cinema): Option[ScrapeGuardState] = Some(byCinema.getOrElse(cinema.displayName, ScrapeGuardState.Fresh))
  def put(cinema: Cinema, state: ScrapeGuardState): Unit = byCinema.update(cinema.displayName, state)
}
