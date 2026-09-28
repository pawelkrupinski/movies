package services.identity

import play.api.Logging
import services.movies.{ListingConstraints, StoredMovieRecord, TitleNormalizer}

import java.time.Clock
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

/** One shadow resolve's outcome: the run it persisted (none when the resolve refused), what it
 *  cost, and how much of its evidence the observations could not supply. */
final case class ShadowTick(run: Option[ShadowRun], listings: Int, crossings: Int, gaps: Long, resolveSeconds: Double,
                            gapsByKind: Map[String, Long] = Map.empty, diffSeconds: Double = 0.0) {
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
 * touched one. That cost grows with how many answers the store holds, not with the corpus alone:
 * UK's ~28k listings took 774–835s a tick on 2026-09-28 with the fill two thirds done, pinning the
 * worker's heap — whole-corpus resolving does not scale with a full store.
 *
 * A resolve that finds a constraint edge crossing a family (`IdentityResolver.FamilyCrossing`) is
 * refused — the closure's own guard — and reported, and the previous run stays the latest. No
 * failure here reaches the caller: the settle it rides must never fail on the shadow's account.
 */
final class ShadowIdentityReaper(
  source:        () => Option[ShadowInput],
  pipelineFilms: () => Seq[StoredMovieRecord],
  normalizer:    TitleNormalizer,
  runs:          ShadowRunStore,
  retention:     ShadowRetention,
  metrics:       ShadowIdentityMetrics,
  clock:         Clock
) extends Logging {

  /** One shadow tick: the source's resolution, diffed against the pipeline's films and persisted.
   *  None while the source has nothing yet (a model not taken up). Throws only what reading its
   *  inputs or writing the run throws. */
  def tick(): ShadowTick = {
    val at = clock.instant()
    try source() match {
      case None =>
        logger.info("identity shadow: the model is not taken up yet; nothing to diff")
        ShadowTick(None, 0, 0, 0, 0)
      case Some(input) =>
        val diffing = System.nanoTime()
        val (clusters, families) = ShadowDiff.of(input.resolution, PipelineFilms.of(input.listings, pipelineFilms(), normalizer))
        val diffSeconds = (System.nanoTime() - diffing) / 1e9
        val run = ShadowRun(at, clusters, families)
        runs.record(run, retention)
        metrics.crossings(0)
        metrics.resolved(ShadowDiff.counts(clusters), input.seconds)
        val tick = ShadowTick(Some(run), input.listings.size, 0, input.gaps, input.seconds, input.gapsByKind, diffSeconds)
        logger.info(f"identity shadow: ${input.listings.size} listings → ${clusters.size} clusters " +
          f"(${tick.films.toSeq.sortBy(_._1.ordinal).map { case (r, n) => s"${r.label} $n" }.mkString(", ")}) resolved in ${input.seconds}%.1fs, diffed against the pipeline in $diffSeconds%.1fs; " +
          s"${input.gaps} unobserved lookups (${input.gapsByKind.toSeq.sortBy(-_._2).map { case (k, n) => s"$k $n" }.mkString(", ")}); " +
          s"${families.size} families differ from the pipeline")
        tick
    } catch {
      case crossing: IdentityResolver.FamilyCrossing =>
        metrics.crossings(crossing.count)
        logger.error(s"identity shadow: resolve refused — ${crossing.getMessage}")
        ShadowTick(None, 0, crossing.count, 0, 0)
    }
  }

  /** [[tick]], for a caller that must not fail on the shadow's account (the settle it rides). */
  def tickQuietly(): Unit =
    try { tick(); () }
    catch { case NonFatal(e) => logger.warn(s"identity shadow: tick failed; the previous run stays the latest: $e") }
}

/** What a shadow tick diffs: a resolution, the listings it covers, how many of its lookups the
 *  observations could not answer (by kind), and how long it took to resolve. */
final case class ShadowInput(resolution: Resolution, listings: Seq[Listing], gaps: Long, gapsByKind: Map[String, Long], seconds: Double)

object ShadowIdentityReaper {
  /** A WHOLE resolve of `listings` per tick — the shadow before the incremental model, and the
   *  reference the specs hold the model to. */
  def resolving(listings: () => Seq[Listing], lookups: () => (IdentityLookups, ObservationGaps), pins: PinStore,
                normalizer: TitleNormalizer, calibration: IdentityCalibration): () => Option[ShadowInput] = () => {
    val corpus         = listings()
    val (source, gaps) = lookups()
    val started        = System.nanoTime()
    val resolution     = IdentityResolver.resolve(corpus, source, normalizer, calibration, ListingConstraints.pinned(pins.all()))
    Some(ShadowInput(resolution, corpus, gaps.total, gaps.byKind, (System.nanoTime() - started) / 1e9))
  }

  /** The incremental model's current state — no resolve at all. */
  def modelled(model: IdentityModelService, timeout: FiniteDuration): () => Option[ShadowInput] = () =>
    model.peek(timeout).map { snapshot =>
      val gaps = snapshot.gaps
      ShadowInput(snapshot.resolution, snapshot.listings, (gaps.queries.size + gaps.films.size).toLong,
        Map("QUERY" -> gaps.queries.size.toLong, "RECORD" -> gaps.films.size.toLong).filter(_._2 > 0), 0.0)
    }
}
