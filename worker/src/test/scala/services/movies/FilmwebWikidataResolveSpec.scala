package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.WikidataClient
import services.events.InProcessEventBus
import tools.{GetOnlyHttpFetch, HttpFetch}

/**
 * Once Filmweb enrichment is un-gated for `tmdbId`-less rows (see
 * [[services.tasks.RatingSources]]), a row that carries a Filmweb URL but no
 * tmdbId — a scraper-supplied Filmweb link, or one Filmweb enrichment
 * discovered — becomes a resolution route TMDB's own fuzzy search missed:
 * Filmweb entity id → Wikidata (P5032 → P4947 = TMDB id).
 *
 * The chain crosses two external cross-references (Filmweb→Wikidata,
 * Wikidata→TMDB) either of which can be mis-linked (the stored Filmweb URL can
 * point at the wrong edition — a 2025 row whose Filmweb slug is the 1997 film).
 * So the resolved TMDB film's OWN year must equal the row's year (hard
 * equality); anything else — a `/serial/` URL, a year mismatch, no year to
 * check — abstains rather than resolve.
 *
 * Real case (the dry-run's set C): Filmweb film 469205 "African Adventure:
 * Safari in the Okavango" (2007) → Wikidata → TMDB 435263.
 */
class FilmwebWikidataResolveSpec extends AnyFlatSpec with Matchers {

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  private def tmdb(routes: (String, String)*): TmdbClient =
    new TmdbClient(http = new StubFetch(routes), apiKey = Some("stub"))

  // A Wikidata client whose two Action-API calls (search by P5032, then
  // wbgetentities claims) are stubbed by URL fragment. `P4947` = TMDB id.
  private def wikidata(tmdbId: Int): WikidataClient =
    new WikidataClient(new HttpFetch {
      def get(url: String): String =
        if (url.contains("haswbstatement")) """{"query":{"search":[{"title":"Q123"}]}}"""
        else if (url.contains("wbgetentities"))
          s"""{"entities":{"Q123":{"claims":{"P4947":[{"mainsnak":{"datavalue":{"value":"$tmdbId"}}}]}}}}"""
        else throw new RuntimeException(s"unstubbed Wikidata URL: $url")
      override def get(url: String, headers: Map[String, String]): String = get(url)
      override def post(url: String, body: String, contentType: String): String =
        throw new RuntimeException("WikidataClient should not POST")
    })

  // A tmdbId-less row that carries a Filmweb URL (scraper-supplied / discovered)
  // and a year, keyed at (title, year) so resolution reads that year back.
  private def rowWithFilmweb(title: String, year: Int, filmwebUrl: String): CaffeineMovieCache = {
    val seed = MovieRecord(
      filmwebUrl = Some(filmwebUrl),
      data       = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(title)))
    )
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq((title, Some(year), seed))))
  }

  // TMDB stubs: fuzzy search always misses (the realistic case — Filmweb is the
  // only route), plus the by-id details the resolve builds its record from.
  // `release_date` year drives BOTH the corroboration check and the record.
  private def tmdbFor(tmdbId: Int, releaseYear: Int, title: String): TmdbClient =
    tmdb(
      "/search/movie"                    -> """{"results":[]}""",
      s"/movie/$tmdbId/external_ids"     -> s"""{"id":$tmdbId,"imdb_id":"tt1099921"}""",
      "append_to_response=credits"       -> s"""{"id":$tmdbId,"title":"$title","release_date":"$releaseYear-04-01","credits":{"crew":[],"cast":[]}}"""
    )

  private val Title = "African Adventure: Safari in the Okavango"
  private val Film  = "https://www.filmweb.pl/film/African+Adventure-2007-469205"

  "the TMDB stage" should "resolve a tmdbId-less row via Filmweb→Wikidata (P4947) when the TMDB film's year corroborates the row" in {
    val cache   = rowWithFilmweb(Title, 2007, Film)
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdbFor(435263, releaseYear = 2007, title = Title),
      wikidata = Some(wikidata(435263)))

    service.reEnrichSync(Title, Some(2007))

    cache.snapshot().flatMap(_.record.tmdbId) should contain (435263)
  }

  it should "NOT resolve from a Filmweb /serial/ URL — a TV series is never the film a cinema screens" in {
    val serial  = "https://www.filmweb.pl/serial/Brudny+Henryk-2017-801993"
    val cache   = rowWithFilmweb("Brudny Henryk", 2017, serial)
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdbFor(801993, releaseYear = 2017, title = "Brudny Henryk"),
      wikidata = Some(wikidata(801993)))

    service.reEnrichSync("Brudny Henryk", Some(2017))

    cache.snapshot().flatMap(_.record.tmdbId) shouldBe empty
  }

  it should "NOT resolve when the Wikidata-resolved TMDB film's year disagrees with the row's year" in {
    val cache   = rowWithFilmweb(Title, 2007, Film)
    val service = new MovieService(cache, new InProcessEventBus(),
      // Wikidata hands back tmdb 435263 but that film is dated 2010 ≠ row's 2007:
      // a mis-linked Filmweb slug signature — must abstain, not resolve.
      tmdbFor(435263, releaseYear = 2010, title = Title),
      wikidata = Some(wikidata(435263)))

    service.reEnrichSync(Title, Some(2007))

    cache.snapshot().flatMap(_.record.tmdbId) shouldBe empty
  }
}
