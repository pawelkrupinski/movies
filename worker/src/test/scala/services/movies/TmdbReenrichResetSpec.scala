package services.movies

import clients.TmdbClient
import models.{KinoPalacowe, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import tools.GetOnlyHttpFetch

/**
 * The operator's forced re-enrich (the /debug button) re-resolves off SCRAPED data,
 * not the previously-resolved data. A row can get self-locked on a wrong film: the
 * resolved TMDB release year is baked into the cache key (`…|year`), and re-resolution
 * reads the search year from that stale key, so it re-confirms the same wrong film
 * forever — the force flag re-runs the search but feeds the stale year right back in.
 *
 * Modelled on the real "Plenerowe Pałacowe: Parasite" case: Kino Pałacowe reports the
 * 2019 Bong Joon-ho film (tmdb 496243 / imdb tt6751668) but the row resolved to the
 * 1982 Charles Band "Parasite" (tmdb 48311) and re-keyed to `…|1982`. A `&year=1982`
 * search returns only the 1982 film, so the button can't escape. Resetting the row to
 * its cinema slots drops the TMDB-derived year, so resolution re-scopes to the scraped
 * 2019 and lands the correct film, re-keying to `…|2019`.
 */
class TmdbReenrichResetSpec extends AnyFlatSpec with Matchers {

  private val Title   = "Parasite"
  private val Wrong   = 48311     // 1982 Charles Band film — the self-locked resolution
  private val Correct = 496243    // 2019 Bong Joon-ho film — what the cinema is showing

  private class StubFetch(routes: Seq[(String, String)]) extends GetOnlyHttpFetch {
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  // `&year=2019` resolves the correct film (a single exact "Parasite" hit, so
  // `searchUnique` succeeds); `&year=1982` resolves the wrong one. `year=2019`
  // is listed first so it wins `url.contains` over the `1982` route.
  private def tmdb(): TmdbClient = new TmdbClient(
    http = new StubFetch(Seq(
      "year=2019" -> s"""{"results":[
        |{"id":$Correct,"title":"Parasite","original_title":"기생충","release_date":"2019-05-30","popularity":34.6}
        |]}""".stripMargin,
      "year=1982" -> s"""{"results":[
        |{"id":$Wrong,"title":"Pasożyt","original_title":"Parasite","release_date":"1982-03-12","popularity":1.1}
        |]}""".stripMargin,
      s"/movie/$Correct/external_ids" -> s"""{"id":$Correct,"imdb_id":"tt6751668"}""",
      s"/movie/$Wrong/external_ids"   -> s"""{"id":$Wrong,"imdb_id":"tt0084472"}"""
    )),
    apiKey = Some("stub")
  )

  // A row self-locked at |1982: the resolved Tmdb slot carries 1982, the cinema slot
  // reports the true 2019, and the stale wrong ids/ratings sit on the record.
  private def lockedRow(): MovieRecord = MovieRecord(
    tmdbId     = Some(Wrong),
    imdbId     = Some("tt0084472"),
    imdbRating = Some(4.1),
    data = Map[Source, SourceData](
      Tmdb         -> SourceData(releaseYear = Some(1982), originalTitle = Some("Parasite")),
      KinoPalacowe -> SourceData(title = Some(Title), releaseYear = Some(2019))
    )
  )

  private def wire(): (CaffeineMovieCache, MovieService) = {
    val repository = new InMemoryMovieRepository(Seq((Title, Some(1982), lockedRow())))
    val cache      = new CaffeineMovieCache(repository)
    (cache, new MovieService(cache, new InProcessEventBus(), tmdb()))
  }

  "a forced re-enrich" should
    "reset the row to scraped data and re-resolve to the cinema-shown film, off the stale year" in {
    val (cache, service) = wire()

    service.resolveTmdbOnce(Title, Some(1982), originalTitle = None, director = None, force = true)

    // Re-resolved to the 2019 film, re-keyed off the stale 1982 year.
    cache.get(cache.keyOf(Title, Some(2019))).flatMap(_.tmdbId) shouldBe Some(Correct)
    cache.get(cache.keyOf(Title, Some(2019))).flatMap(_.imdbId) shouldBe Some("tt6751668")
    // The stale |1982 row is gone — not left as a duplicate/orphan.
    cache.get(cache.keyOf(Title, Some(1982))) shouldBe None
  }

  it should "NOT reset on a non-forced resolve — the stale year still pins the wrong film" in {
    val (cache, service) = wire()

    // Without force, the row keeps its |1982 key and re-confirms the wrong 1982 film.
    service.resolveTmdbOnce(Title, Some(1982), originalTitle = None, director = None, force = false)

    cache.get(cache.keyOf(Title, Some(1982))).flatMap(_.tmdbId) shouldBe Some(Wrong)
    cache.get(cache.keyOf(Title, Some(2019))) shouldBe None
  }
}
