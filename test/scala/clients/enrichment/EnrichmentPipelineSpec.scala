package clients.enrichment

import clients.TmdbClient
import models.Enrichment
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.{FilmwebClient, ImdbClient, MetacriticClient, RottenTomatoesClient}
import tools.HttpFetch

/**
 * Glue-level tests that exercise the same TMDB → IMDb → Filmweb chain
 * EnrichmentService.fetchEnrichment runs, but driven by stub HttpFetch
 * instances so we can assert behaviour without touching the live APIs.
 *
 * Regression coverage:
 *   - Mortal Kombat II (tt17490712): just-released, IMDb has aggregated a
 *     rating before Cinemeta did — pipeline reads it from IMDb's GraphQL CDN.
 */
class EnrichmentPipelineSpec extends AnyFlatSpec with Matchers {

  // Minimal stub: route GETs (and POSTs) to canned bodies by URL substring.
  class StubFetch(routes: Map[String, String]) extends HttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
    override def post(url: String, body: String, contentType: String): String = get(url)
  }

  // Run the same chain EnrichmentService.fetchEnrichment runs so the test
  // catches drift between the production wiring and what we believe it does.
  // No OMDb step — Enrichment is built directly from TMDB + IMDb (GraphQL
  // CDN scrape) + Filmweb + URL validators.
  private def run(
    tmdb:    TmdbClient,
    filmweb: FilmwebClient,
    imdb:       ImdbClient,
    metacritic: MetacriticClient,
    rt:         RottenTomatoesClient,
    title: String, year: Option[Int]
  ): Option[Enrichment] =
    for {
      hit    <- tmdb.search(title, year)
      imdbId <- tmdb.imdbId(hit.id)
    } yield {
      val fw          = scala.util.Try(filmweb.lookup(title, year)).toOption.flatten
      val imdbRating  = scala.util.Try(imdb.lookup(imdbId)).toOption.flatten
      val originalTit = hit.originalTitle
      val linkTitle   = originalTit.getOrElse(title)
      val metacrUrl   = scala.util.Try(metacritic.urlFor(linkTitle)).toOption.flatten
      val rtomatoeUrl = scala.util.Try(rt.urlFor(linkTitle)).toOption.flatten
      Enrichment(
        imdbId            = Some(imdbId),
        imdbRating        = imdbRating,
        metascore         = None,
        originalTitle     = originalTit,
        filmwebUrl        = fw.map(_.url),
        filmwebRating     = fw.flatMap(_.rating),
        rottenTomatoes    = None,
        tmdbId            = Some(hit.id),
        metacriticUrl     = metacrUrl,
        rottenTomatoesUrl = rtomatoeUrl
      )
    }

  // Real-world responses, captured live, trimmed to the fields the pipeline
  // reads. tt17490712 = Mortal Kombat II (2026).
  private val Mk2TmdbSearch =
    """{"results":[
      |  {"id":931285, "title":"Mortal Kombat II", "original_title":"Mortal Kombat II",
      |   "release_date":"2026-05-06", "popularity":223.66}
      |]}""".stripMargin
  private val Mk2TmdbExternalIds = """{"id":931285, "imdb_id":"tt17490712"}"""
  private val Mk2FilmwebSearch   =
    """{"searchHits":[{"id":10007434,"type":"film","matchedTitle":"Mortal Kombat II"}]}"""
  private val Mk2FilmwebInfo     = """{"title":"Mortal Kombat II","year":2026}"""
  private val Mk2FilmwebRating   = """{"rate":6.72,"count":1000}"""
  // IMDb's GraphQL CDN has aggregated the rating (real-world: ~7.0/17k votes).
  private val Mk2ImdbGraphqlBody =
    """{"data":{"title":{"ratingsSummary":{"aggregateRating":7.0,"voteCount":16966}}}}"""

  "The enrichment pipeline" should
    "produce a usable record with IMDb rating from the GraphQL scrape (Mortal Kombat II)" in {

    val tmdb = new TmdbClient(
      http = new StubFetch(Map(
        "/search/movie" -> Mk2TmdbSearch,
        "/external_ids" -> Mk2TmdbExternalIds
      )),
      apiKey = Some("stub")
    )
    val filmweb = new FilmwebClient(
      http = new StubFetch(Map(
        "/live/search"          -> Mk2FilmwebSearch,
        "/film/10007434/info"   -> Mk2FilmwebInfo,
        "/film/10007434/rating" -> Mk2FilmwebRating
      ))
    )
    val imdb = new ImdbClient(http = new StubFetch(Map("caching.graphql.imdb.com" -> Mk2ImdbGraphqlBody)))
    // MC / RT URL validators return None for any probe — `urlFor` never
    // returns a search URL now, so the resulting fields stay None.
    val metacritic = new MetacriticClient(http = new StubFetch(Map.empty)) {
      override def urlFor(title: String, fallback: Option[String], year: Option[Int]): Option[String] = None
    }
    val rt = new RottenTomatoesClient(http = new StubFetch(Map.empty)) {
      override def urlFor(title: String, fallback: Option[String], year: Option[Int]): Option[String] = None
    }

    val e = run(tmdb, filmweb, imdb, metacritic, rt, "Mortal Kombat II", Some(2026)).get
    e.imdbId            shouldBe Some("tt17490712")
    e.imdbRating        shouldBe Some(7.0)        // IMDb GraphQL scrape
    e.metascore         shouldBe None             // not sourced anymore
    e.rottenTomatoes    shouldBe None             // not sourced anymore
    e.tmdbId            shouldBe Some(931285)
    e.filmwebRating     shouldBe Some(6.72)
    e.metacriticUrl     shouldBe None             // probes failed; no search URL persisted
    e.rottenTomatoesUrl shouldBe None
  }

}
