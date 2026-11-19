package services.enrichment

import clients.TmdbClient
import services.movies.{CacheKey, LatestTitleYear, MovieCache}
import services.resolution.ResolutionCache

/** Each URL-keyed rating site's port run by the shared flow, as the composition root (`RatingsWiring`) builds them,
 *  for the specs: the site's collaborators in, its `SiteRatings` out. */
object SiteRatingsOf {
  type Cadence = (CacheKey, Option[Int], Option[String]) => Unit
  private val silent: Cadence = (_, _, _) => ()

  def metacritic(cache: MovieCache, tmdb: TmdbClient, metacritic: MetacriticClient,
                 mcLinkCache: ResolutionCache = ResolutionCache.passthrough, cadenceRecorder: Cadence = silent)(using LatestTitleYear): SiteRatings[Int] =
    new SiteRatings(cache, new MetascoreRatings(tmdb, metacritic, cache.normalizer, mcLinkCache), tools.SpecStopwatch.Frozen, cadenceRecorder)

  def rottenTomatoes(cache: MovieCache, tmdb: TmdbClient, rt: RottenTomatoesClient,
                     rtLinkCache: ResolutionCache = ResolutionCache.passthrough, cadenceRecorder: Cadence = silent)(using LatestTitleYear): SiteRatings[Int] =
    new SiteRatings(cache, new RottenTomatoesRatings(tmdb, rt, cache.normalizer, rtLinkCache), tools.SpecStopwatch.Frozen, cadenceRecorder)

  def filmweb(cache: MovieCache, tmdb: TmdbClient, filmweb: FilmwebClient,
              filmwebLinkCache: ResolutionCache = ResolutionCache.passthrough,
              onImdbIdMissing: (String, Option[Int], String) => Unit = (_, _, _) => (),
              cadenceRecorder: Cadence = silent)(using LatestTitleYear): SiteRatings[Double] =
    new SiteRatings(cache, new FilmwebRatings(tmdb, filmweb, cache.normalizer, filmwebLinkCache, onImdbIdMissing), tools.SpecStopwatch.Frozen, cadenceRecorder)
}
