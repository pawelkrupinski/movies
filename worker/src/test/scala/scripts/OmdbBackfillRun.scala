package scripts

import services.enrichment.{OMDbClient, OmdbBackfill}
import services.movies.{CaffeineMovieCache, MongoMovieRepository}
import tools.RealHttpFetch

/**
 * One-shot OMDb IDENTIFIER backfill over the whole corpus. For every row that
 * lacks them, it recovers a missing `imdbId` (by title+year search) and
 * `rottenTomatoesUrl` (OMDb's `tomatoURL`), writing through to Mongo via the
 * cache. It writes NO rating value — the canonical refreshers (`ImdbRatings` /
 * `RottenTomatoesRatings`) fill `imdbRating` and `rottenTomatoes` from the new
 * ids/links on their next tick.
 *
 * Feature-gated on `OMDB_API_KEY` (Env / `.env.local` locally, Fly secret in
 * prod); a no-op when unset. Safe to re-run / schedule — `orElse` write-back
 * never overrides an identifier a canonical writer already supplied.
 *
 * Run: OMDB_API_KEY=… MONGODB_URI=… sbt "worker/Test/runMain scripts.OmdbBackfillRun"
 */
object OmdbBackfillRun {
  def main(args: Array[String]): Unit = {
    val repository = new MongoMovieRepository()
    if (!repository.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }
    if (OMDbClient.ApiKey.isEmpty) {
      println("OMDB_API_KEY not set — OMDb backfill is off.")
      sys.exit(1)
    }

    val cache = new CaffeineMovieCache(repository)
    val hydrated = cache.rehydrate()
    println(s"Hydrated $hydrated rows; running OMDb identifier backfill (imdbId + rottenTomatoesUrl)…")

    new OmdbBackfill(cache, new OMDbClient(new RealHttpFetch)).refreshAllNow()

    repository.close()
    println("Done. Canonical refreshers will fill the rating values from the new ids/links on their next tick.")
  }
}
