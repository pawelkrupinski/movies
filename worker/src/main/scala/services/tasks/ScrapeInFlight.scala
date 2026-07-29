package services.tasks

/**
 * Whether a cinema's scrape is ALREADY running, so [[ScrapeReaper]] can leave it
 * out of the due set instead of re-admitting it every tick.
 *
 * Exists because a chunked venue's freshness is stamped by whichever step
 * TERMINATES its scrape — the reduce on success, the planner on an empty/failed
 * plan — and NOT by the `ScrapeCinema` task that merely starts the run. That is
 * deliberate: stamping at the start would make `lastFetchedAt` claim data we have
 * not fetched yet, and the staleness census reads exactly that field, so the panel
 * would go green while the listing aged.
 *
 * The cost of being honest is that a venue stays "most overdue" for the whole
 * duration of its own run, and `ScrapeReaper` orders most-overdue-first — so it
 * kept re-admitting the same venues, whose `ScrapeCinema` tasks then no-opped
 * against the run mutex (`ChunkScrapePlanner`'s `startRun` returning None). Prod,
 * 2026-07-29: ~139 `ScrapeCinema`/h completed but only ~30-45/h actually stamped,
 * against the ~120/h the 843-venue roster needs at a 7h window — so the oldest
 * cinema aged in a straight diagonal to 14.6h. It became visible once the
 * admission budget was tightened to ~4 venues/min, because then most of the budget
 * went on those no-ops.
 *
 * Skipping in-flight venues is the honest fix: it neither lies about freshness nor
 * spends the budget re-admitting work already underway.
 */
trait ScrapeInFlight {

  /** True while `cinemaName`'s scrape is already running and not yet abandoned.
   *  Keyed by `Cinema.displayName` — what `ScrapeCinemaHandler.scraperKey` and the
   *  chunk-run store both key by. */
  def isRunning(cinemaName: String): Boolean
}

object ScrapeInFlight {

  /** No cinema is ever in flight — the default, so callers that wire no chunked
   *  scrapers (and every test that doesn't care) keep the pre-existing behaviour. */
  val Never: ScrapeInFlight = _ => false
}
