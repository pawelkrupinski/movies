package services.identity

import play.api.Logging
import services.movies.{ListingConstraints, StoredMovieRecord, TitleNormalizer}

import java.time.Clock
import scala.util.control.NonFatal

/** One shadow resolve's outcome: the run it persisted (none when the resolve refused), what it
 *  cost, and how much of its evidence the observations could not supply. */
final case class ShadowTick(run: Option[ShadowRun], listings: Int, crossings: Int, gaps: Long, resolveSeconds: Double) {
  def films: Map[ShadowRelation, Int] = run.fold(Map.empty[ShadowRelation, Int])(r => ShadowDiff.counts(r.clusters))
}

/** Where the shadow run reports: the per-relation cluster counts, the family crossings of the
 *  last resolve, and how long it took. */
trait ShadowIdentityMetrics {
  def resolved(films: Map[ShadowRelation, Int], resolveSeconds: Double): Unit
  def crossings(count: Int): Unit
}

object ShadowIdentityMetrics {
  val noop: ShadowIdentityMetrics = new ShadowIdentityMetrics {
    def resolved(films: Map[ShadowRelation, Int], resolveSeconds: Double): Unit = ()
    def crossings(count: Int): Unit                                              = ()
  }
}

/**
 * The identity resolver's SHADOW RUN in production (docs/design/identity-resolver.md §8,
 * "Phase 1: shadow mode"): after each settle tick it resolves the country's live listing set —
 * every listing of the scrape archive's latest scrapes — with the lookups the observation store
 * holds, diffs the clusters against the films today's pipeline made of the same listings, and
 * persists the decisions and the diff ([[ShadowRunStore]]) and exports the gauges.
 *
 * It SERVES NOTHING: it writes only the shadow collections, reads the pipeline's films without
 * touching them, and issues no external lookup (`lookups` is built over the store alone,
 * `ObservedIdentityLookups`; a gap is an `Unknown` node). Every family is resolved in each tick —
 * a resolve is a pure function of the listing set, and resolving every family is resolving each
 * touched one, at the measured cost of seconds per corpus (the design doc, §15).
 *
 * A resolve that finds a constraint edge crossing a family (`IdentityResolver.FamilyCrossing`) is
 * refused — the closure's own guard — and reported, and the previous run stays the latest. No
 * failure here reaches the caller: the settle it rides must never fail on the shadow's account.
 */
final class ShadowIdentityReaper(
  listings:      () => Seq[Listing],
  pipelineFilms: () => Seq[StoredMovieRecord],
  lookups:       () => (IdentityLookups, ObservationGaps),
  pins:          PinStore,
  normalizer:    TitleNormalizer,
  calibration:   IdentityCalibration,
  runs:          ShadowRunStore,
  retention:     ShadowRetention,
  metrics:       ShadowIdentityMetrics,
  clock:         Clock
) extends Logging {

  /** One shadow resolve. Throws only what reading its inputs or writing the run throws. */
  def tick(): ShadowTick = {
    val at             = clock.instant()
    val corpus         = listings()
    val (source, gaps) = lookups()
    val started        = System.nanoTime()
    try {
      val resolution = IdentityResolver.resolve(corpus, source, normalizer, calibration, ListingConstraints.pinned(pins.all()))
      val seconds    = (System.nanoTime() - started) / 1e9
      val (clusters, families) = ShadowDiff.of(resolution, PipelineFilms.of(corpus, pipelineFilms(), normalizer))
      val run = ShadowRun(at, clusters, families)
      runs.record(run, retention)
      metrics.crossings(0)
      metrics.resolved(ShadowDiff.counts(clusters), seconds)
      val tick = ShadowTick(Some(run), corpus.size, 0, gaps.total, seconds)
      logger.info(f"identity shadow: ${corpus.size} listings → ${clusters.size} clusters " +
        f"(${tick.films.toSeq.sortBy(_._1.ordinal).map { case (r, n) => s"${r.label} $n" }.mkString(", ")}) in $seconds%.1fs; " +
        s"${gaps.total} unobserved lookups; ${families.size} families differ from the pipeline")
      tick
    } catch {
      case crossing: IdentityResolver.FamilyCrossing =>
        metrics.crossings(crossing.count)
        logger.error(s"identity shadow: resolve refused — ${crossing.getMessage}")
        ShadowTick(None, corpus.size, crossing.count, gaps.total, (System.nanoTime() - started) / 1e9)
    }
  }

  /** [[tick]], for a caller that must not fail on the shadow's account (the settle it rides). */
  def tickQuietly(): Unit =
    try { tick(); () }
    catch { case NonFatal(e) => logger.warn(s"identity shadow: tick failed; the previous run stays the latest: $e") }
}
