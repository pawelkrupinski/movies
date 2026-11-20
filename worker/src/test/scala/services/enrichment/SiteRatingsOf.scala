package services.enrichment

import models.CacheKey

import clients.TmdbClient
import services.movies.MovieCache
import services.evidence.LatestTitleYear
import services.resolution.ResolutionCache

/** Each URL-keyed rating site's port run by the shared flow, as the composition root (`RatingsWiring`) builds them,
 *  for the specs: the site's collaborators in, its `SiteRatings` out. */
object SiteRatingsOf {
  type Cadence = (CacheKey, Option[Int], Option[String]) => Unit
  private val silent: Cadence = (_, _, _) => ()

  /** The film every site searches by, over the cache's title rules, as the wiring builds it. */
  def films(cache: MovieCache, tmdb: TmdbClient)(using LatestTitleYear): RatingSiteFilms = new RatingSiteFilms(tmdb, cache.normalizer)

  def metacritic(cache: MovieCache, tmdb: TmdbClient, metacritic: MetacriticClient,
                 mcLinkCache: ResolutionCache = ResolutionCache.passthrough, cadenceRecorder: Cadence = silent)(using LatestTitleYear): SiteRatings[Int] =
    new SiteRatings(cache, new MetascoreRatings(films(cache, tmdb), metacritic, cache.normalizer, mcLinkCache), RatingColumns.Metacritic,
      tools.SpecStopwatch.Frozen, cadenceRecorder)

  def rottenTomatoes(cache: MovieCache, tmdb: TmdbClient, rt: RottenTomatoesClient,
                     rtLinkCache: ResolutionCache = ResolutionCache.passthrough, cadenceRecorder: Cadence = silent)(using LatestTitleYear): SiteRatings[Int] =
    new SiteRatings(cache, new RottenTomatoesRatings(films(cache, tmdb), rt, cache.normalizer, rtLinkCache), RatingColumns.RottenTomatoes,
      tools.SpecStopwatch.Frozen, cadenceRecorder)

  def filmweb(cache: MovieCache, tmdb: TmdbClient, filmweb: FilmwebClient,
              filmwebLinkCache: ResolutionCache = ResolutionCache.passthrough,
              onImdbIdMissing: (String, Option[Int], String) => Unit = (_, _, _) => (),
              cadenceRecorder: Cadence = silent)(using LatestTitleYear): SiteRatings[Double] =
    new SiteRatings(cache, new FilmwebRatings(films(cache, tmdb), filmweb, cache.normalizer, filmwebLinkCache, onImdbIdMissing),
      RatingColumns.Filmweb, tools.SpecStopwatch.Frozen, cadenceRecorder)
}
