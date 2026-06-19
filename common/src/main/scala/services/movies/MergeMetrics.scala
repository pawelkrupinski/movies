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

  val all: Seq[MergeReason] = Seq(Canonicalize, ResolvedSettle, TmdbIdentity)
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
}

object MergeMetrics {
  val noop: MergeMetrics = (_, _) => ()
}
