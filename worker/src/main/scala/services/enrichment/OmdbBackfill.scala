package services.enrichment

import services.movies.{CacheKey, MovieCache}
import tools.BoundedParallel

/**
 * OMDb fallback for the three IMDb-keyed external ratings: `imdbRating`,
 * `rottenTomatoes` and `metascore`. A single OMDb round-trip per film (keyed by
 * `imdbId`) can fill any of the three a row is still MISSING after the canonical
 * sources ([[ImdbRatings]] / [[RottenTomatoesRatings]] / [[MetascoreRatings]])
 * have had their turn.
 *
 * FALLBACK SEMANTICS — this is the whole point of the class:
 *   - It only acts on a row that has an `imdbId` AND is missing at least one of
 *     the three ratings (nothing to gain otherwise — skip the HTTP call).
 *   - It writes ONLY the missing fields, via `orElse` against the live cached
 *     row, so it NEVER overrides a value a canonical writer already supplied.
 *     (Each of the three fields keeps "exactly one canonical writer"; OMDb is a
 *     strictly-additive backfill, not a competing writer.)
 *
 * Feature gate lives one level down in [[OMDbClient]]: with `OMDB_API_KEY`
 * unset, `omdb.ratings` returns `None` without any HTTP call, so every
 * `refreshOne` here is an immediate no-op. The whole refresher is only wired in
 * `WorkerWiring` when the key is present (see that file).
 *
 * Shares the per-row / full-corpus skeleton with the canonical refreshers via
 * [[CacheRefresher]].
 */
class OmdbBackfill(
  cache: MovieCache,
  omdb:  OMDbClient,
  cadenceRecorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
) extends CacheRefresher(cache, cadenceRecorder) {

  override protected def sourceName: String = "OMDb"

  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      val wantImdb = e.imdbRating.isEmpty
      val wantRt   = e.rottenTomatoes.isEmpty
      val wantMc   = e.metascore.isEmpty
      e.imdbId match {
        // Nothing to backfill unless there's an IMDb id AND a gap to fill.
        case Some(imdbId) if wantImdb || wantRt || wantMc =>
          omdb.ratings(imdbId).flatMap { r =>
            val newImdb = if (wantImdb) r.imdbRating     else None
            val newRt   = if (wantRt)   r.rottenTomatoes else None
            val newMc   = if (wantMc)   r.metascore      else None
            if (newImdb.isEmpty && newRt.isEmpty && newMc.isEmpty) None
            else {
              // `orElse` against the LIVE row is the concurrent-safe merge point:
              // a canonical writer that won the race in between keeps its value.
              cache.putIfPresent(key, cur => cur.copy(
                imdbRating     = cur.imdbRating.orElse(newImdb),
                rottenTomatoes = cur.rottenTomatoes.orElse(newRt),
                metascore      = cur.metascore.orElse(newMc)
              ))
              val badge = Seq(
                newImdb.map(v => s"IMDb $v"),
                newRt.map(v => s"RT $v%"),
                newMc.map(v => s"MC $v")
              ).flatten.mkString(", ")
              logger.info(s"OMDb: '${key.cleanTitle}' (${key.year.getOrElse("?")}) backfilled $badge")
              Some(badge)
            }
          }
        case _ => None
      }
    }

  private[services] def refreshAll(): Unit = {
    val snapshot = cache.entries
    logger.info(s"OMDb backfill: starting tick over ${snapshot.size} cached row(s).")
    BoundedParallel.foreach("OMDb-backfill", snapshot, refreshConcurrency) { case (key, e) =>
      refreshOne(key).foreach(v => recordCadenceChange(key, e.tmdbId, Some(v)))
    }
  }
}
