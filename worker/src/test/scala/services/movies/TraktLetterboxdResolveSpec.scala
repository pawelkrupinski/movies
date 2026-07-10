package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.{LetterboxdClient, LetterboxdIdResolver, TraktClient, TraktIdResolver}
import services.events.InProcessEventBus
import tools.GetOnlyHttpFetch

/**
 * A tmdbId-less row can carry a known `imdbId` (OMDb backfill recovers one for
 * exactly the films TMDB's fuzzy search misses). TMDB's own `/find` is tried
 * first, but TMDB's external-id index is itself blank for obscure titles — so
 * after it misses we cross to the OTHER id-keyed sources, which hold the same
 * imdbId→tmdbId mapping: Trakt's `/search/imdb`, then Letterboxd's film page.
 * Both are exact (id-keyed), so no corroboration is needed and neither guesses.
 */
class TraktLetterboxdResolveSpec extends AnyFlatSpec with Matchers {

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  private def tmdbStub(routes: (String, String)*): TmdbClient =
    new TmdbClient(http = new StubFetch(routes), apiKey = Some("stub"))

  // A row OMDb backfill would leave behind: one cinema slot, a recovered imdbId,
  // no tmdbId. Fuzzy search already failed on this title.
  private def backfilledRow(title: String, imdbId: String): CaffeineMovieCache = {
    val seed = MovieRecord(
      imdbId = Some(imdbId),
      data   = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(title)))
    )
    new CaffeineMovieCache(new InMemoryMovieRepository(Seq((title, None, seed))))
  }

  // TMDB routes where fuzzy search AND /find both miss, but the crosswalk-
  // resolved id has details so `buildResolvedRecord` can fetch them.
  private def tmdbMissesButHasDetails(imdbId: String, id: Int): Seq[(String, String)] = Seq(
    "/search/movie"              -> """{"results":[]}""",
    s"/find/$imdbId"             -> """{"movie_results":[]}""",
    s"/movie/$id/external_ids"   -> s"""{"id":$id,"imdb_id":"$imdbId"}""",
    "language=en-US"             -> s"""{"id":$id,"title":"Long Tail Film","release_date":"2015-03-01"}""",
    "append_to_response=credits" -> s"""{"id":$id,"title":"Długi Ogon","release_date":"2015-03-01","credits":{"crew":[],"cast":[]}}"""
  )

  "the TMDB stage" should "resolve a tmdbId-less row via Trakt when TMDB search and /find both miss" in {
    val imdbId = "tt5550001"
    val cache  = backfilledRow("Zupełnie Nieznany Tytuł", imdbId)
    val trakt  = new TraktIdResolver(new TraktClient(
      new StubFetch(Seq(s"/search/imdb/$imdbId" ->
        s"""[{"type":"movie","movie":{"title":"Long Tail Film","year":2015,"ids":{"trakt":1,"tmdb":8801,"imdb":"$imdbId"}}}]""")),
      apiKey = Some("stub")))
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdbStub(tmdbMissesButHasDetails(imdbId, 8801)*),
      traktIdResolver = Some(trakt))

    service.reEnrichSync("Zupełnie Nieznany Tytuł", None)
    cache.snapshot().flatMap(_.record.tmdbId) should contain (8801)
  }

  it should "resolve a tmdbId-less row via Letterboxd when TMDB and Trakt both miss" in {
    val imdbId     = "tt5550002"
    val cache      = backfilledRow("Inny Nieznany Tytuł", imdbId)
    val letterboxd = new LetterboxdIdResolver(new LetterboxdClient(
      new StubFetch(Seq(s"/imdb/$imdbId/" ->
        s"""<html><body data-tmdb-id="8802" data-tmdb-type="movie"><a href="https://www.imdb.com/title/$imdbId/">imdb</a></body></html>"""))))
    val service = new MovieService(cache, new InProcessEventBus(),
      tmdbStub(tmdbMissesButHasDetails(imdbId, 8802)*),
      letterboxdIdResolver = Some(letterboxd))

    service.reEnrichSync("Inny Nieznany Tytuł", None)
    cache.snapshot().flatMap(_.record.tmdbId) should contain (8802)
  }
}
