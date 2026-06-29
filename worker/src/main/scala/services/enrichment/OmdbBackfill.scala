package services.enrichment

import services.movies.{CacheKey, MovieCache}
import tools.BoundedParallel

import java.time.Clock
import scala.concurrent.duration._

/**
 * OMDb IDENTIFIER backfill: recovers a missing `imdbId` (by title+year search)
 * and a missing `rottenTomatoesUrl` (OMDb's `tomatoURL`, keyed by imdb id). It
 * writes only those two IDENTIFIERS — never a rating value. The canonical
 * refreshers then fetch the scores FROM them on their next tick:
 * [[ImdbRatings]] fills `imdbRating` once an `imdbId` is present, and
 * [[RottenTomatoesRatings]] fills `rottenTomatoes` from a stored
 * `rottenTomatoesUrl` (skipping its own URL discovery). So each rating keeps
 * exactly one canonical writer; OMDb only unblocks them.
 *
 * FALLBACK SEMANTICS:
 *   - Acts only on a row MISSING `imdbId` or `rottenTomatoesUrl` (nothing to
 *     gain otherwise — skip the HTTP call).
 *   - Writes via `orElse` against the live cached row, so a canonical writer
 *     that filled the id/url in between keeps its value — OMDb never overrides.
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

  override protected def sourceName: String = "OMDb"

  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      val wantImdbId = e.imdbId.isEmpty
      val wantRtUrl  = e.rottenTomatoesUrl.isEmpty
      if (!wantImdbId && !wantRtUrl) None        // already fully identified
      else if (inBackoff(key)) None              // recently probed + missed → still backing off (no HTTP)
      else {
        // 1. Recover a missing IMDb id by title+year search. Original
        //    (production/English) title first — OMDb is an English DB; the
        //    cinema display title is the fallback spelling.
        val foundImdbId =
          if (wantImdbId)
            omdb.findImdbId((e.originalTitle.toSeq :+ e.displayTitle(key.cleanTitle)).distinct, key.year, e.director.toSet)
          else None
        // 2. Recover a missing RT url via the imdb id we have (or just found).
        val effectiveId = e.imdbId.orElse(foundImdbId)
        val foundRtUrl  = if (wantRtUrl) effectiveId.flatMap(omdb.rottenTomatoesUrl) else None
        // Back off when this probe didn't recover everything still wanted — the
        // next sweep skips the film until the (doubling) window elapses.
        val recoveredAll = (!wantImdbId || foundImdbId.isDefined) && (!wantRtUrl || foundRtUrl.isDefined)
        if (!recoveredAll) recordMiss(key)
        if (foundImdbId.isEmpty && foundRtUrl.isEmpty) None
        else {
          // `orElse` against the LIVE row: a canonical writer that won the race
          // keeps its value — OMDb only fills a still-empty identifier. The
          // rating numbers are then fetched FROM these by ImdbRatings /
          // RottenTomatoesRatings on their next tick; OMDb writes no score.
          cache.putIfPresent(key, cur => cur.copy(
            imdbId            = cur.imdbId.orElse(foundImdbId),
            rottenTomatoesUrl = cur.rottenTomatoesUrl.orElse(foundRtUrl)
          ))
          val badge = (foundImdbId.map("imdbId " + _).toSeq ++ foundRtUrl.map(_ => "RT-link").toSeq).mkString(", ")
          logger.info(s"OMDb: '${e.displayTitle(key.cleanTitle)}' (${key.year.getOrElse("?")}) recovered $badge")
          Some(badge)
        }
      }
    }

  private def filmKey(key: CacheKey): String =
    s"${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  private def inBackoff(key: CacheKey): Boolean =
    attempts.get(filmKey(key)).exists(a =>
      clock.instant().isBefore(a.at.plusMillis(OmdbBackfill.backoffWindow(a.level).toMillis)))

  private def recordMiss(key: CacheKey): Unit = {
    val nextLevel = math.min(attempts.get(filmKey(key)).map(_.level).getOrElse(0) + 1, OmdbBackfill.MaxBackoffLevel)
    attempts.record(filmKey(key), nextLevel, clock.instant())
  }

  private[services] def refreshAll(): Unit = {
    val snapshot = cache.entries
    logger.info(s"OMDb backfill: starting tick over ${snapshot.size} cached row(s).")
    BoundedParallel.foreach("OMDb-backfill", snapshot, refreshConcurrency) { case (key, e) =>
      refreshOne(key).foreach(v => recordCadenceChange(key, e.tmdbId, Some(v)))
    }
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
