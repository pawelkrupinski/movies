package tools.costs

import tools.costs.PerformanceBudget.{bytes, ceiling, operations}

/**
 * Every hot path's cost budget, in one place: what it measured when its budget was set, and what its
 * spec holds it to. Each names the spec that reads it, and `PerformanceBudgetsSpec` fails on one no
 * spec reads.
 *
 * An allocation budget (`bytes`) is a two-sided RATCHET around `measured` (`PerformanceBudget.DefaultBand`,
 * 0.85x to 1.15x unless it names a wider band): its spec fails when the path costs MORE (a regression) and
 * when it costs LESS (a stale figure). So `measured` is always today's cost, never a number from months
 * ago that a regression can hide under. A `ceiling` only bounds from above; an `operations` count is exact.
 *
 * HOW TO UPDATE ONE — deliberately, never to make a red build green:
 *  - It went red ABOVE its band (a regression):
 *    1. Find out WHY the path costs more. A regression is fixed, not budgeted: a cache that stopped
 *       hitting, a copy per element, a read of rows nobody keeps.
 *    2. When the cost is a deliberate trade (a page that now shows more), re-measure on the parent
 *       commit and on yours with the spec's own `info` lines (each prints `name: actual, held to
 *       floor..limit`; every check also appends to `target/performance-budgets.tsv`), two runs on an idle
 *       machine, to see what moved and by how much.
 *  - It went red BELOW its band (the path got cheaper): the same, in the commit that made it cheaper, so
 *    the gain is held from then on.
 *  - THE FIGURE WRITTEN DOWN IS CI'S, NOT A LAPTOP'S. A Mac's JVM lands ~3-5% off CI's Linux one, and
 *    figures re-measured locally turned Main red twice on 2026-10-24. Every row of ci.yml that runs a budget
 *    spec uploads its `performance-budgets.tsv` as a `performance-budgets-<row>` artifact, and
 *    `scripts/adopt-ci-budgets <run id>` sets each `measured` here to the median of that run's figures (a
 *    figure outside its band only with `--out-of-band`, for a deliberate trade). After a red Main run on a
 *    budget you moved on purpose, adopt that run's figures in the fixing commit (`Fixes-Red:`); after a green
 *    one, they recalibrate a figure that drifted.
 *  - Either way, say in the commit message what moved and both numbers, and add a dated note here.
 *  - A path whose runs genuinely spread wider than the default band names its own `band =` with a comment
 *    saying what spread was measured — never to quiet a regression.
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
  // Lowered 2026-10-11 when the listing, film and facet pages stopped embedding every country's city
  // catalog (`KINOWO_CATALOG`, then the `/` landing's alone), measured against parent ee9c28631:
  // listing warm 2.03 -> 1.30 MB, film page warm 4.00 -> 2.81, cold 5.83 -> 4.63, facet warm 2.14 -> 1.30.
  // Re-measured 2026-10-24 on f4d30dacc when budgets became two-sided ratchets (four runs, one in a fresh
  // JVM, every figure within 2.2% across them; Main's `chrome / stable` rows, Linux, printed within 5% of
  // these — film page warm 2.89, /api/repertoire cold 12.84 — well inside the 15% band): listing cold 20.0 -> 18.3 MB, warm 1.30 -> 1.26, film page
  // cold 4.63 -> 4.50, warm 2.81 -> 2.75, facet warm 1.30 -> 1.26, landing 990 -> 1,040 KB, /api/repertoire
  // cold 14.3 -> 13.4, warm 1.35 -> 1.28, /api/details cold 4.15 -> 4.13, warm 651 -> 663 KB; blob hit 7.0 KB.

  /** The city listing with every controller cache empty: schedules, cards, JSON-LD all built. 18.3 -> 20.4 MB on
   *  2026-10-24 for the cards' curated search spellings (846a6b572, then dbee52416's leaner fold): Main measured 19.49
   *  before them, 20.43 / 20.44 after. */
  val ListingCold = bytes("listing render, cold caches", measured = mb(20.4))
  /** The city listing with the controller's caches full and the response blob missed (`?diag=`). */
  val ListingWarm = bytes("listing render, warm caches", measured = mb(1.33))
  /** The city listing served from the gzipped blob. */
  val ListingBlobHit = bytes("listing render, response blob hit", measured = kb(7.2))
  /** The heaviest film page, cold and warm. */
  val FilmPageCold = bytes("film page render, cold caches", measured = mb(4.61))
  val FilmPageWarm = bytes("film page render, warm caches", measured = mb(2.87))
  /** The facet page of the country the most films list. */
  val BrowseWarm = bytes("facet page render, warm caches", measured = mb(1.27))
  /** The `/` city-selection landing. It embedded every country's city catalog, so it grew with the roster
   *  (2.95 -> 5.21 MB when the US went from 468 metro pages to 2,003 town pages); it now carries only its
   *  own country's pane, rendered once per process (`PickerFragments`), and fetches the rest after load:
   *  5.21 MB -> 990 KB, measured 2026-10-11. */
  val Landing = bytes("landing render", measured = kb(1043))
  /** `/api/repertoire` and `/api/details`, every film's JSON built (cold) or kept (warm). */
  /** 13.4 -> 15.3 MB on 2026-10-24 for the films' curated search spellings, which the apps' payload carries: Main
   *  measured 14.67 before them, 15.55 after (15.00 locally). */
  val ApiRepertoireCold = bytes("/api/repertoire render, cold caches", measured = mb(15.6))
  val ApiRepertoireWarm = bytes("/api/repertoire render, warm caches", measured = mb(1.29))
  val ApiDetailsCold    = bytes("/api/details render, cold caches", measured = mb(4.18))
  val ApiDetailsWarm    = bytes("/api/details render, warm caches", measured = kb(657))

  /** Schedules a second render over an unchanged read model builds again: none, in every fixture city
   *  (`RenderBudgetSpec`) and in a city with a venue on an earlier clock (`ScheduleCacheBudgetSpec`). */
  val ScheduleRebuildsOnRepeatRender = operations("schedules rebuilt by a repeated render", limit = 0)

  /** `FilterDescriptionSpec`: an unfiltered listing's description over 600 films, which must not build the
   *  filters' universes (~1 MB a New York render when it did). 4.0 -> 2.9 KB re-measured 2026-10-24 on
   *  f4d30dacc (three runs, identical). */
  val UnfilteredListingDescription = bytes("unfiltered listing description, 600 films", measured = kb(2.9))

  // ── The browser (`PageJsBehaviourSpec`, real Chrome) ──────────────────────────────────────────

  /** DOM queries one listing filter pass makes — the same on the fixture listing and with
   *  10,500 more pills, so a query per pill or per cinema group cannot come back. */
  val FilterPassDomQueries = operations("DOM queries in one listing filter pass", limit = 20)

  // ── The identity projection over the settled fixture corpus (`IdentityCutoverEndToEndSpec`) ───

  /** A scoped tick with nothing moved, and a whole one, on the projecting thread. The quiet tick measured
   *  6.52 MB on 7f10382ca, the commit that first set it (at 5.62, which no run reproduces): six of its seven
   *  runs within 0.2%, the seventh the hourly whole read; 7.50 MB with C2 off (`-XX:TieredStopAtLevel=1`).
   *  5.02 MB (5.00 and 5.02, two runs) once the agreement stage stopped reading each of the 3,459 questions it
   *  waits on every quiet tick (9.35 MB: 4.4 MB of `QuestionMemory` reads, read only once the memory marks). */
  val QuietProjectionTick = bytes("quiet identity projection tick", measured = mb(5.33))
  /** The whole tick: 34.1 -> 36.1 MB, re-measured 2026-10-24 (35.92 / 36.71 / 36.07 locally on quiet-tick-alloc's
   *  3ef51dd60; Main's e2e row printed 35.01 on ff779d962 — CI's Linux JVM runs ~3% under a Mac's here). */
  val WholeProjectionTick = bytes("whole identity projection tick, nothing moved", measured = mb(36.1))
  /** Venue slots built by projections over a corpus where nothing moved. */
  val QuietProjectionSlotsBuilt = operations("venue slots built with nothing moved", limit = 0)

  // ── The projection's listing read, on the wire (`ListingReadBudgetIntegrationSpec`, real Mongo) ──

  /** Whole listing rows fetched beyond the venues the read keeps: a venue no longer live, or the archive's
   *  copy of a venue with an accepted listing, is never fetched. */
  val ListingReadRowsBeyondKept = operations("listing rows fetched beyond the venues kept", limit = 0)
  /** A read over archives nothing was written to since (60 archived venues, 15 accepted, 40 live): no whole
   *  row and no id scan: the two archives' stamp reads alone. */
  val QuietListingReadRows      = operations("listing rows fetched by a quiet read", limit = 0)
  val QuietListingReadCommands  = operations("Mongo reads sent by a quiet listing read", limit = 4)
  val QuietListingReadDocuments = operations("documents returned to a quiet listing read", limit = 75)

  // ── Fixed ceilings: a bound the path must stay under, not a measured cost ────────────────────

  /** `ShareCardPostersSpec`: refusing an 8000×12000 progressive JPEG before decoding it, which would
   *  take hundreds of megabytes of raster. */
  val GiantProgressivePosterRefusal = ceiling("refusing a giant progressive JPEG", limit = mb(4))
}
