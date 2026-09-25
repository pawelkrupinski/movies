package services.enrichment

import clients.TmdbClient
import models.{MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.{GetOnlyHttpFetch, RealHttpFetch, UpstreamNotFound}

import java.nio.file.{Files, Paths}

/**
 * A stored rating url that is another film's must drop on the next refresh — no manual
 * prod write. Prod, 2026-09-25: "Bogaci i martwi" (Romain Gavras's 2026 "Sacrifice", tmdb
 * 1284186) held Rotten Tomatoes' /m/sacrifice — Umberto Lenzi's 1972 "Sacrifice!", undated
 * on RT, so the year guard never saw it — and Metacritic's /movie/sacrifice-2021. Every page
 * here is the real one, recorded by "Record scrape fixtures" run 36181020153.
 */
class RatingPageIdentitySpec extends AnyFlatSpec with Matchers {

  private def recorded(name: String): String =
    new String(Files.readAllBytes(Paths.get(s"test/resources/fixtures/rating-page-identity/$name")), "UTF-8")

  private def http(pages: Map[String, String]) = new GetOnlyHttpFetch {
    def get(url: String): String = pages.getOrElse(url, UpstreamNotFound(url))
  }

  private val tmdb = new TmdbClient(new RealHttpFetch, apiKey = None)

  private def bogaci(rtUrl: Option[String] = None, rt: Option[Int] = None,
                     mcUrl: Option[String] = None, mc: Option[Int] = None) = {
    val row = MovieRecord(tmdbId = Some(1284186), rottenTomatoesUrl = rtUrl, rottenTomatoes = rt,
      metacriticUrl = mcUrl, metascore = mc,
      data = Map[Source, SourceData](Tmdb -> SourceData(title = Some("Bogaci i martwi"), originalTitle = Some("Sacrifice"),
        releaseYear = Some(2026), director = Seq("Romain Gavras"))))
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(("Bogaci i martwi", Some(2026), row)),
      normalizer = titleNormalizer), normalizer = titleNormalizer)
    (cache, cache.keyOf("Bogaci i martwi", Some(2026)))
  }

  private val RtLenzi  = "https://www.rottentomatoes.com/m/sacrifice"
  private val RtGavras = "https://www.rottentomatoes.com/m/sacrifice_2026"

  "a stored Rotten Tomatoes url whose page credits another director" should "be dropped with its score" in {
    val (cache, key) = bogaci(rtUrl = Some(RtLenzi), rt = Some(39))
    new RottenTomatoesRatings(cache, tmdb, new RottenTomatoesClient(http(Map(RtLenzi -> recorded("rt-sacrifice.html")))))
      .refreshOneSync(key)

    cache.get(key).flatMap(_.rottenTomatoesUrl) shouldBe None
    cache.get(key).flatMap(_.rottenTomatoes) shouldBe None
  }

  it should "be kept when the page credits the film's own director" in {
    val (cache, key) = bogaci(rtUrl = Some(RtGavras))
    new RottenTomatoesRatings(cache, tmdb, new RottenTomatoesClient(http(Map(RtGavras -> recorded("rt-sacrifice_2026.html")))))
      .refreshOneSync(key)

    cache.get(key).flatMap(_.rottenTomatoesUrl) shouldBe Some(RtGavras)
  }

  "Rotten Tomatoes discovery" should "not store an undated bare slug whose page credits another director" in {
    val (cache, key) = bogaci()
    new RottenTomatoesRatings(cache, tmdb, new RottenTomatoesClient(http(Map(RtLenzi -> recorded("rt-sacrifice.html")))))
      .refreshOneSync(key)

    cache.get(key).flatMap(_.rottenTomatoesUrl) shouldBe None
  }

  private val McBruno  = "https://www.metacritic.com/movie/sacrifice/"
  private val McGavras = "https://www.metacritic.com/movie/sacrifice-2025/"

  "a stored Metacritic url whose page credits another director" should "be dropped with its score" in {
    val (cache, key) = bogaci(mcUrl = Some("https://www.metacritic.com/movie/sacrifice"), mc = Some(61))
    new MetascoreRatings(cache, tmdb, new MetacriticClient(http(Map(McBruno -> recorded("mc-sacrifice.html")))))
      .refreshOneSync(key)

    cache.get(key).flatMap(_.metacriticUrl) shouldBe None
    cache.get(key).flatMap(_.metascore) shouldBe None
  }

  it should "be kept when the page credits the film's own director" in {
    val (cache, key) = bogaci(mcUrl = Some("https://www.metacritic.com/movie/sacrifice-2025"))
    new MetascoreRatings(cache, tmdb, new MetacriticClient(http(Map(McGavras -> recorded("mc-sacrifice-2025.html")))))
      .refreshOneSync(key)

    cache.get(key).flatMap(_.metacriticUrl) shouldBe Some("https://www.metacritic.com/movie/sacrifice-2025")
  }
}
