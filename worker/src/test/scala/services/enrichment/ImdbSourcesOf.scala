package services.enrichment

import services.movies.{LatestTitleYear, MovieCache}

import java.time.Clock

/** The IMDb sources as the composition root (`RatingsWiring`) builds them, for the specs: IMDb's ratings over Poland's
 *  country names (the specs' historical behaviour), and the OMDb id backfill from its client, backoff store and clock. */
object ImdbSourcesOf {
  def ratings(cache: MovieCache, imdb: ImdbClient,
              cadenceRecorder: (services.movies.CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()): ImdbRatings =
    new ImdbRatings(cache, imdb, new ImdbRowUpdates(models.Country.Poland.language), cadenceRecorder)

  def backfill(cache: MovieCache, omdb: OMDbClient, attempts: OmdbAttemptStore = OmdbAttemptStore.noop, clock: Clock): OmdbBackfill =
    new OmdbBackfill(cache, new OmdbSearch(omdb, cache.normalizer)(using LatestTitleYear(clock)), new OmdbBackoff(attempts, clock))
}
