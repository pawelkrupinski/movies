package services.movies

import clients.TmdbClient
import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.GetOnlyHttpFetch

/**
 * The misresolution sweep re-examines a resolved row its own cinemas contradict. When
 * the evidence still names the SAME film, the row must come out as it went in.
 *
 * DE "Überleben" (2020), 2026-09-24: its venue credits a director TMDB does not, so the
 * sweep flagged it; the forced re-resolve stripped it to its cinema slots, found the same
 * film, and re-fetched its ratings — and the row was flagged again, so it was stripped
 * and rebuilt once per sweep period for ever. The convergence legs' fixpoint pass caught
 * it as a full tick over unchanged input writing ten documents.
 */
class ResolutionReexaminationSpec extends AnyFlatSpec with Matchers {

  private val Id    = 270303   // "It Follows"
  private val Other = 999001

  private class StubFetch(routes: Map[String, String]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed URL: $url"))
  }

  private def details(id: Int, title: String) =
    s"""{"id":$id,"title":"$title","original_title":"$title","release_date":"2014-05-17","runtime":100,"overview":"…",
       |"genres":[{"id":27,"name":"Horror"}],"credits":{"crew":[{"job":"Director","name":"David Robert Mitchell"}],"cast":[]}}""".stripMargin

  /** TMDB, answering the title search with `searchHit`. */
  private def tmdb(searchHit: Int): TmdbClient = new TmdbClient(http = new StubFetch(Map(
    "/search/movie"              -> s"""{"results":[{"id":$searchHit,"title":"Coś za mną chodzi","original_title":"It Follows","release_date":"2014-05-17"}]}""",
    s"/movie/$Id/external_ids"    -> s"""{"id":$Id,"imdb_id":"tt3235888"}""",
    s"/movie/$Other/external_ids" -> s"""{"id":$Other,"imdb_id":"tt0000001"}""",
    s"/movie/$Id?"                -> details(Id, "Coś za mną chodzi"),
    s"/movie/$Other?"             -> details(Other, "Coś za mną chodzi"))),
    apiKey = Some("stub"))

  private def resolvedRow: MovieRecord = MovieRecord(
    tmdbId = Some(Id), imdbId = Some("tt3235888"), imdbRating = Some(6.8), rottenTomatoes = Some(95),
    rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/it_follows"),
    data = Map[Source, SourceData](
      Helios -> SourceData(title = Some("Coś za mną chodzi"), releaseYear = Some(2014), runtimeMinutes = Some(100)),
      Tmdb   -> SourceData(title = Some("Coś za mną chodzi"), originalTitle = Some("It Follows"), releaseYear = Some(2014),
                           runtimeMinutes = Some(100))))

  "re-examining a resolution" should "leave the row untouched when the evidence still names the same film" in {
    val repository = new InMemoryMovieRepository()
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val key        = cache.keyOf("Coś za mną chodzi", Some(2014))
    // The row as a real resolution leaves it: its Tmdb slot is what TMDB answers for its id.
    cache.put(key, resolvedRow.copy(data = resolvedRow.data - Tmdb))
    var ratingsForced = 0
    val service = new MovieService(cache, new InProcessEventBus(), tmdb(searchHit = Id),
      forceRatingRefresh = (_, _) => ratingsForced += 1)
    service.refillTmdbSlot(key) shouldBe true
    val stored = repository.findAll()

    service.reexamineResolution(key)
    service.stop()

    val after = cache.get(key).get
    after.tmdbId            shouldBe Some(Id)
    after.rottenTomatoes    shouldBe Some(95)
    after.rottenTomatoesUrl shouldBe resolvedRow.rottenTomatoesUrl
    after.imdbRating        shouldBe Some(6.8)
    ratingsForced           shouldBe 0
    repository.findAll()    shouldBe stored
  }

  it should "still re-resolve the row when the evidence names a different film" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    val key     = cache.keyOf("Coś za mną chodzi", Some(2014))
    cache.put(key, resolvedRow)
    val service = new MovieService(cache, new InProcessEventBus(), tmdb(searchHit = Other))

    service.reexamineResolution(key)
    service.stop()

    val after = cache.snapshot().find(_.record.tmdbId.isDefined).map(_.record).get
    after.tmdbId         shouldBe Some(Other)
    after.rottenTomatoes shouldBe None   // the other film's ratings are not this one's
  }

  it should "keep the resolution when the evidence names no film at all" in {
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    val key     = cache.keyOf("Coś za mną chodzi", Some(2014))
    cache.put(key, resolvedRow)
    val nothing = new TmdbClient(http = new StubFetch(Map(
      "/search/"                 -> """{"results":[]}""",
      s"/movie/$Id/external_ids" -> s"""{"id":$Id,"imdb_id":"tt3235888"}""",
      s"/movie/$Id?"             -> details(Id, "Coś za mną chodzi"))), apiKey = Some("stub"))
    val service = new MovieService(cache, new InProcessEventBus(), nothing)

    service.reexamineResolution(key)
    service.stop()

    val after = cache.get(key).get
    after.tmdbId         shouldBe Some(Id)
    after.rottenTomatoes shouldBe Some(95)
  }
}
