package tools.costs

import tools.costs.PerformanceBudget.{bytes, ceiling, operations}

/**
 * Every hot path's cost budget, in one place: what it measured when its budget was set, and the
 * ceiling its spec holds it to. Each names the spec that reads it, and `PerformanceBudgetsSpec`
 * fails on one no spec reads.
 *
 * HOW TO UPDATE ONE — deliberately, never to make a red build green:
 *  1. Find out WHY the path costs more. A regression is fixed, not budgeted: a cache that stopped
 *     hitting, a copy per element, a read of rows nobody keeps.
 *  2. When the cost is a deliberate trade (a page that now shows more), re-measure on the parent
 *     commit and on yours with the spec's own `info` lines (each prints `name: actual of limit`),
 *     on an idle machine, and set `measured` to yours.
 *  3. Say in the commit message what moved and both numbers. A path made cheaper lowers its budget
 *     in the same commit, so the gain is held from then on.
 *
 * Allocation is per thread (`AllocationMeter`), warmed and the median of several runs, so it holds on
 * a loaded machine; operation counts are exact.
 */
object PerformanceBudgets {

  private def kb(n: Double): Long = (n * 1024).toLong
  private def mb(n: Double): Long = (n * 1024 * 1024).toLong

  // ── Web renders over the fixture corpus (Poznań, `RenderBudgetSpec`) ──────────────────────────
  // Measured 2026-10-04 on e3a5b22ef, two runs within 1%, through the production actions in `Mode.Prod`
  // (its memoising minifier renders the shared stylesheets once; a test-mode controller re-renders them
  // per page, which put the warm film page at 3.64 MB) — the gzip-accepting request whole, body bytes
  // included. NOT the same scope as the commit-message figures for these pages: 0e2630b9b's "film page
  // 3,718 -> 1,731 KB" is one Warsaw film rendered and written; the budget here is Poznań's HEAVIEST film
  // (196 showtimes), and its cost grows with the showtimes (Warsaw's 524-showtime film writes ~2x).
  // "Cold" includes filling the minifier's memo, which is why the film page's cold is above its warm.

  /** The city listing with every controller cache empty: schedules, cards, JSON-LD all built. */
  val ListingCold = bytes("listing render, cold caches", measured = mb(20.0))
  /** The city listing with the controller's caches full and the response blob missed (`?diag=`). */
  val ListingWarm = bytes("listing render, warm caches", measured = mb(1.82))
  /** The city listing served from the gzipped blob. */
  val ListingBlobHit = bytes("listing render, response blob hit", measured = kb(7.0))
  /** The heaviest film page, cold and warm. */
  val FilmPageCold = bytes("film page render, cold caches", measured = mb(4.74))
  val FilmPageWarm = bytes("film page render, warm caches", measured = mb(2.93))
  /** The facet page of the country the most films list. */
  val BrowseWarm = bytes("facet page render, warm caches", measured = mb(1.93))
  /** `/api/repertoire` and `/api/details`, every film's JSON built (cold) or kept (warm). */
  val ApiRepertoireCold = bytes("/api/repertoire render, cold caches", measured = mb(14.3))
  val ApiRepertoireWarm = bytes("/api/repertoire render, warm caches", measured = mb(1.35))
  val ApiDetailsCold    = bytes("/api/details render, cold caches", measured = mb(4.15))
  val ApiDetailsWarm    = bytes("/api/details render, warm caches", measured = kb(651))

  /** Schedules a second render over an unchanged read model builds again: none, in every fixture city
   *  (`RenderBudgetSpec`) and in a city with a venue on an earlier clock (`ScheduleCacheBudgetSpec`). */
  val ScheduleRebuildsOnRepeatRender = operations("schedules rebuilt by a repeated render", limit = 0)

  /** `FilterDescriptionSpec`: an unfiltered listing's description over 600 films, which must not build the
   *  filters' universes (~1 MB a New York render when it did). */
  val UnfilteredListingDescription = bytes("unfiltered listing description, 600 films", measured = kb(4.0))

  // ── The browser (`PageJsBehaviourSpec`, real Chrome) ──────────────────────────────────────────

  /** DOM queries one listing filter pass makes — the same on the fixture listing and with
   *  10,500 more pills, so a query per pill or per cinema group cannot come back. */
  val FilterPassDomQueries = operations("DOM queries in one listing filter pass", limit = 20)

  // ── The identity projection over the settled fixture corpus (`IdentityCutoverEndToEndSpec`) ───

  /** A scoped tick with nothing moved, and a whole one, on the projecting thread. */
  val QuietProjectionTick = bytes("quiet identity projection tick", measured = mb(5.62))
  val WholeProjectionTick = bytes("whole identity projection tick, nothing moved", measured = mb(34.1))
  /** Venue slots built by projections over a corpus where nothing moved. */
  val QuietProjectionSlotsBuilt = operations("venue slots built with nothing moved", limit = 0)

  // ── The projection's listing read, on the wire (`ListingReadBudgetIntegrationSpec`, real Mongo) ──

  /** Whole listing rows fetched beyond the venues the read keeps: a venue no longer live, or the archive's
   *  copy of a venue with an accepted listing, is never fetched. */
  val ListingReadRowsBeyondKept = operations("listing rows fetched beyond the venues kept", limit = 0)
  /** A read over archives nothing was written to since (60 archived venues, 15 accepted, 40 live): no whole
   *  row, and the reads and documents of the two archives' stamp and id scans alone. */
  val QuietListingReadRows      = operations("listing rows fetched by a quiet read", limit = 0)
  val QuietListingReadCommands  = operations("Mongo reads sent by a quiet listing read", limit = 6)
  val QuietListingReadDocuments = operations("documents returned to a quiet listing read", limit = 150)

  // ── Fixed ceilings: a bound the path must stay under, not a measured cost ────────────────────

  /** `ShareCardPostersSpec`: refusing an 8000×12000 progressive JPEG before decoding it, which would
   *  take hundreds of megabytes of raster. */
  val GiantProgressivePosterRefusal = ceiling("refusing a giant progressive JPEG", limit = mb(4))
}
