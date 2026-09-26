package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.resolution.{InMemoryResolutionStore, ResolutionCache, WriteThroughResolutionCache}
import tools.RoutingHttpFetch

import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The TMDB id resolution (search + verify + director-walk) is cached per hint
 * combination, so resolving the same film a second time runs no TMDB title search.
 * Without the cache (the `passthrough` control) each resolve hits `/search/movie`
 * again — that contrast is the fail-before / pass-after gate for the wiring.
 */
class MovieServiceTmdbCacheSpec extends AnyFlatSpec with Matchers {

  import TheVisitorOnTmdb._

  private def tmdb(http: RoutingHttpFetch): TmdbClient = new TmdbClient(http = http, apiKey = Some(settings.TmdbApiKey("stub")))

  /** How many title searches `http` served — what the cache exists to save. */
  private def searches(http: RoutingHttpFetch): Int = http.calls.count(_._2.contains(SearchPath))

  // A director-less row resolves to the lone popular hit, and stays a stable
  // hint-key across both resolves (no director means none gets added).
  private def seededCache(): CaffeineMovieCache = {
    val seed = MovieRecord(data = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(Title))))
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq((Title, Year, seed)), normalizer = titleNormalizer), normalizer = titleNormalizer)
  }

  "the TMDB id cache" should "resolve the search once for two resolves of the same hints" in {
    val http = RoutingHttpFetch.getOnly(Routes)
    val cache = seededCache()
    val service = new MovieService(cache, new InProcessEventBus(), tmdb(http),
      tmdbIdCache = new WriteThroughResolutionCache(new InMemoryResolutionStore(normalizer = titleNormalizer)))

    service.reEnrichSync(Title, Year).flatMap(_.tmdbId) shouldBe Some(TmdbId)
    val first = searches(http)
    service.reEnrichSync(Title, Year).flatMap(_.tmdbId) shouldBe Some(TmdbId)
    // The second resolve searches nothing: its id comes off the cache, and the film's title
    // search was already measured for the rating gate on the first.
    searches(http) shouldBe first
  }

  it should "hit the search on every resolve without a cache (control)" in {
    val http = RoutingHttpFetch.getOnly(Routes)
    val cache = seededCache()
    val service = new MovieService(cache, new InProcessEventBus(), tmdb(http),
      tmdbIdCache = ResolutionCache.passthrough)

    service.reEnrichSync(Title, Year)
    val first = searches(http)
    service.reEnrichSync(Title, Year)
    searches(http) should be > first
  }
}
