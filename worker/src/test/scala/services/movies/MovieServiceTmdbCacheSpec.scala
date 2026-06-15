package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.resolution.{InMemoryResolutionStore, ResolutionCache, WriteThroughResolutionCache}
import tools.GetOnlyHttpFetch

import java.util.concurrent.atomic.AtomicInteger

/**
 * The TMDB id resolution (search + verify + director-walk) is cached per hint
 * combination, so resolving the same film twice runs the TMDB title search ONCE.
 * Without the cache (the `passthrough` control) each resolve hits `/search/movie`
 * again — that contrast is the fail-before / pass-after gate for the wiring.
 */
class MovieServiceTmdbCacheSpec extends AnyFlatSpec with Matchers {

  private val Title = "The Visitor"
  private val Year  = Some(2022)
  private val TmdbId = 881487

  /** Counts `/search/movie` calls so we can prove the second resolve skips it. */
  private class CountingFetch(searchCalls: AtomicInteger) extends GetOnlyHttpFetch {
    private val routes = Seq(
      "/search/movie" -> s"""{"results":[
        |{"id":$TmdbId,"title":"Gość","original_title":"The Visitor","release_date":"2022-10-07","popularity":1.4}
        |]}""".stripMargin,
      s"/movie/$TmdbId/external_ids" -> s"""{"id":$TmdbId,"imdb_id":"tt15558152"}"""
    )
    override def get(url: String): String = {
      if (url.contains("/search/movie")) searchCalls.incrementAndGet()
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
    }
  }

  private def tmdb(searchCalls: AtomicInteger): TmdbClient =
    new TmdbClient(http = new CountingFetch(searchCalls), apiKey = Some("stub"))

  // A director-less row resolves to the lone popular hit, and stays a stable
  // hint-key across both resolves (no director means none gets added).
  private def seededCache(): CaffeineMovieCache = {
    val seed = MovieRecord(data = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(Title))))
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq((Title, Year, seed))))
  }

  "the TMDB id cache" should "resolve the search once for two resolves of the same hints" in {
    val searchCalls = new AtomicInteger(0)
    val cache = seededCache()
    val service = new MovieService(cache, new InProcessEventBus(), tmdb(searchCalls),
      tmdbIdCache = new WriteThroughResolutionCache(new InMemoryResolutionStore()))

    service.reEnrichSync(Title, Year).flatMap(_.tmdbId) shouldBe Some(TmdbId)
    service.reEnrichSync(Title, Year).flatMap(_.tmdbId) shouldBe Some(TmdbId)
    searchCalls.get() shouldBe 1
  }

  it should "hit the search on every resolve without a cache (control)" in {
    val searchCalls = new AtomicInteger(0)
    val cache = seededCache()
    val service = new MovieService(cache, new InProcessEventBus(), tmdb(searchCalls),
      tmdbIdCache = ResolutionCache.passthrough)

    service.reEnrichSync(Title, Year)
    service.reEnrichSync(Title, Year)
    searchCalls.get() shouldBe 2
  }
}
