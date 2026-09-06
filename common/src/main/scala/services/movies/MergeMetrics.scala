package services.movies

/**
 * Why a movie row was folded into another row (and its `(title, year)` key
 * abandoned). The merge counter is split on this so the worker's merge rate can
 * be read per cause: each fold has a different trigger and a different
 * re-enrichment cost, and during the 2026-06-19 worker-throttle incident it was
 * impossible to tell which kind of fold was driving the re-key wave.
 *
 * `sealed trait` + `case object` with a `label` mirrors `StagingStep`; the label
 * is the Prometheus `reason` value.
 */
sealed trait MergeReason { def label: String }
object MergeReason {
  /** `MovieCache.collapseCluster` — the periodic settle / rehydrate pass folds a
   *  same-film cluster onto its canonical: cross-language same-tmdbId rows, or a
   *  case/separator/±year drift of one title. The re-key wave that orphaned
   *  `title|year` detail freshness on 2026-06-19. */
  case object Canonicalize   extends MergeReason { val label = "canonicalize"    }
  /** `MovieCache.settleResolved` — TMDB concludes a yearless row's year and the
   *  yearless+idless same-title strays (plus any prior occupant of the resolved
   *  year) fold onto it in one write. */
  case object ResolvedSettle extends MergeReason { val label = "resolved-settle" }
  /** `MovieCache.foldDeterministically` — a freshly-written row shares a tmdbId
   *  with an existing one; the runtime identity gate folds the lower-rank loser
   *  into the canonical at `put` time, before any settle pass. */
  case object TmdbIdentity   extends MergeReason { val label = "tmdb-identity"   }
  /** Retroactive re-key: rows that previously had distinct merge keys now share one
   *  (e.g. on cache rehydration after a key collision). Pairs with the splits counter. */
  case object NormalizeRebuild extends MergeReason { val label = "normalize-rebuild" }

  val all: Seq[MergeReason] = Seq(Canonicalize, ResolvedSettle, TmdbIdentity, NormalizeRebuild)
}

/**
 * Sink for movie-row merge counts — one increment per VICTIM row absorbed (a
 * cluster of N rows collapsing to one counts N−1). A victim's `(title, year)`
 * key is abandoned by the fold, orphaning its detail freshness, so
 * `rate(kinowo_worker_merges_total{reason=…})` is a direct proxy for the re-key
 * re-enrichment load a merge wave drives (the `EnrichDetails → ResolveTmdb`
 * cascade). The worker wires the Prometheus-backed
 * [[services.metrics.WorkerTaskMetrics]]; web and unit tests use [[noop]].
 */
trait MergeMetrics {
  def recordMerge(reason: MergeReason, victims: Int): Unit
  /** A row that kept its identity but changed its `(title, year)` key — the
   *  degenerate merge of one row. Counted separately from merges because it is the
   *  cost a stable film id would remove and nothing else, so its rate is the
   *  measurement that decides whether that change is worth making. Defaulted so
   *  the SAM `noop` stays a lambda. */
  def recordRekey(reason: RekeyReason): Unit = ()
}

/** Why a row's `(title, year)` key moved while the row stayed the same film. */
sealed trait RekeyReason { def label: String }
object RekeyReason {
  /** `MovieCache.settleResolved` — TMDB concluded a year for a yearless row. */
  case object ResolvedYear extends RekeyReason { val label = "resolved-year" }
  /** `MovieCache.collapseCluster` — the settle re-spelled or re-yeared a lone row
   *  onto its canonical form. */
  case object Canonicalize extends RekeyReason { val label = "canonicalize"  }
  /** `MovieCache.backfillEmbeddedYears` — a year a venue wrote into its title
   *  promoted a yearless key. */
  case object EmbeddedYear extends RekeyReason { val label = "embedded-year" }
  /** `MovieService.resetToScrapedData` — the operator's forced re-enrich re-keyed
   *  the row onto its scraped year. */
  case object ForcedReset  extends RekeyReason { val label = "forced-reset"  }
  /** `MovieCache.recordCinemaScrape` — a scrape's spelling or year variant
   *  promoted an unresolved row's key at landing time. */
  case object ScrapeVariant extends RekeyReason { val label = "scrape-variant" }

  val all: Seq[RekeyReason] = Seq(ResolvedYear, Canonicalize, EmbeddedYear, ForcedReset, ScrapeVariant)
}

object MergeMetrics {
  val noop: MergeMetrics = (_, _) => ()
}

/**
 * Sink for movie-row SPLIT counts — the inverse of a merge. A row keyed by title
 * can end up holding TWO films ("Joanna d'Arc": Besson 1999 and Pálmason 2025);
 * `MixedFilmSplitter`, run by every `MovieService.settle`, sends the stray
 * cinemas' slots back to staging so each film gets a row of its own. This counts
 * the slots re-diverted. Each one re-enters resolution on its own hints, so
 * `rate(kinowo_worker_splits_total)` is the un-merge re-enrichment load — the
 * counterpart to merges — and, since a healthy corpus holds only a handful of
 * genuine title collisions, any sustained rate means the detector has started
 * reading ordinary rows as two films. The only splitting path is the settle
 * pass, so this is unlabelled. The worker wires the Prometheus-backed
 * [[services.metrics.WorkerTaskMetrics]]; web and unit tests use [[noop]].
 */
trait SplitMetrics {
  def recordSplit(fragments: Int): Unit
}

object SplitMetrics {
  val noop: SplitMetrics = _ => ()
}
