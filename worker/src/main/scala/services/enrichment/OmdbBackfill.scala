package services.enrichment

import services.movies.{CacheKey, MovieCache}
import services.tasks.BulkRefreshResult
import tools.BoundedParallel

import java.time.Clock
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

/**
 * OMDb IMDb-id backfill: recovers a missing `imdbId` by title+year search. It
 * writes only that IDENTIFIER — never a rating value, and never a rating site's
 * link: [[RottenTomatoesRatings]] alone finds, verifies and writes
 * `rottenTomatoesUrl`, as [[MetascoreRatings]] does `metacriticUrl`. A second
 * writer of those links raced them and bypassed their page checks. [[ImdbRatings]]
 * then fills `imdbRating` from the recovered id on its next tick.
 *
 * FALLBACK SEMANTICS:
 *   - Acts only on a row MISSING `imdbId` (nothing to gain otherwise — skip the
 *     HTTP call).
 *   - Writes via `orElse` against the live cached row, so a canonical writer
 *     that filled the id in between keeps its value — OMDb never overrides.
 *   - The imdb-id search is title-match guarded (see [[OMDbClient]]) so a fuzzy
 *     OMDb hit can't bind an unrelated film.
 *
 * Feature gate lives one level down in [[OMDbClient]]: with `OMDB_API_KEY`
 * unset, every method returns `None` without any HTTP call, so each
 * `refreshOne` is an immediate no-op. The whole refresher is only wired in
 * `WorkerWiring` when the key is present (see that file).
 *
 * Shares the per-row / full-corpus skeleton with the canonical refreshers via
 * [[CacheRefresher]].
 */
class OmdbBackfill(
  cache: MovieCache,
  omdb:  OMDbClient,
  // Durable per-film backoff so a film OMDb can't resolve isn't re-probed on
  // every daily sweep (which would burn the free 1000/day quota). Default no-op
  // = no backoff (tests / Mongo-less wiring).
  attempts: OmdbAttemptStore = OmdbAttemptStore.noop,
  clock:    Clock = Clock.systemUTC(),
  cadenceRecorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
) extends CacheRefresher(cache, cadenceRecorder) {

  private val normalizer: services.movies.TitleNormalizer = cache.normalizer

  override protected def sourceName: String = "OMDb"

  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      if (e.imdbId.isDefined) None               // already identified
      else if (inBackoff(key)) None              // recently probed + missed → still backing off (no HTTP)
      else {
        // Original (production/English) title first — OMDb is an English DB; the
        // cinema display title is the fallback spelling.
        val foundImdbId =
          omdb.findImdbId((e.originalTitle.toSeq :+ e.displayTitle(key.cleanTitle, normalizer)).distinct, key.year, e.director.toSet)
        foundImdbId match {
          // A miss backs off: the next sweep skips the film until the (doubling) window elapses.
          case None => recordMiss(key); None
          case Some(imdbId) =>
            // `orElse` against the LIVE row: a canonical writer that won the race keeps its id.
            cache.putIfPresent(key, cur => cur.copy(imdbId = cur.imdbId.orElse(foundImdbId)))
            logger.info(s"OMDb: '${e.displayTitle(key.cleanTitle, normalizer)}' (${key.year.getOrElse("?")}) recovered imdbId $imdbId")
            Some(s"imdbId $imdbId")
        }
      }
    }

  private def filmKey(key: CacheKey): String =
    s"${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  // Pre-loaded once per `refreshAll` sweep so the per-film backoff read is an
  // in-memory lookup instead of a blocking Mongo `get` per candidate row. `None`
  // on the single-film path (`refreshOneSync`), which reads the store directly.
  @volatile private var sweepBackoff: Option[Map[String, OmdbAttempt]] = None

  private def attemptFor(key: CacheKey): Option[OmdbAttempt] = sweepBackoff match {
    case Some(snapshot) => snapshot.get(filmKey(key))
    case None           => attempts.get(filmKey(key))
  }

  private def inBackoff(key: CacheKey): Boolean =
    attemptFor(key).exists(a =>
      clock.instant().isBefore(a.at.plusMillis(OmdbBackfill.backoffWindow(a.level).toMillis)))

  private def recordMiss(key: CacheKey): Unit = {
    val nextLevel = math.min(attemptFor(key).map(_.level).getOrElse(0) + 1, OmdbBackfill.MaxBackoffLevel)
    attempts.record(filmKey(key), nextLevel, clock.instant())
  }

  private[services] def refreshAll(): BulkRefreshResult = {
    val snapshot = cache.entries
    // ONE batched read of the backoff stamps for the whole sweep. Previously each
    // candidate row triggered a blocking Mongo `get` inside `inBackoff`; run
    // corpus-wide every sweep, those per-row reads drained the worker's shared-CPU
    // credit to the floor (see OmdbAttemptStore.all).
    sweepBackoff = Some(attempts.all())
    val changed = new AtomicInteger(0)
    try {
      logger.info(s"OMDb backfill: starting tick over ${snapshot.size} cached row(s).")
      BoundedParallel.foreach("OMDb-backfill", snapshot, refreshConcurrency) { case (key, e) =>
        refreshOne(key).foreach { v => recordCadenceChange(key, e.tmdbId, Some(v)); changed.incrementAndGet() }
      }
    } finally sweepBackoff = None
    BulkRefreshResult.counts(walked = snapshot.size, changed = changed.get, discovered = 0, failed = 0,
      message = s"backfill done over ${snapshot.size} row(s) — ${changed.get} identifier(s) recovered.")
  }
}

object OmdbBackfill {
  /** Cap on consecutive-miss backoff levels — level 5 already reaches the 30-day
   *  ceiling. */
  val MaxBackoffLevel = 5
  private val BaseBackoff = 2.days
  private val MaxBackoff  = 30.days

  /** Exponential backoff after `level` consecutive misses: 2d, 4d, 8d, 16d, then
   *  the 30-day cap. Level 0 (never missed) is immediately eligible. */
  def backoffWindow(level: Int): FiniteDuration =
    if (level <= 0) Duration.Zero
    else {
      val scaled = BaseBackoff * math.pow(2, (level - 1).toDouble).toLong
      if (scaled > MaxBackoff) MaxBackoff else scaled
    }
}
