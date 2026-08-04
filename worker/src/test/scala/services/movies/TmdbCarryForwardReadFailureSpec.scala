package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.resolution.ResolutionCache
import tools.GetOnlyHttpFetch
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The last instance of "a failed read is not an absent row" in the write paths.
 *
 * The TMDB stage carries forward the row it is about to overwrite — `buildResolvedRecord`
 * merges onto `cache.stored(writeKey)` so a re-resolve doesn't null the scores the
 * `*Ratings` refreshers own, nor drop the cinema slots. That read already survived the
 * COLD-cache case (it reads `stored`, not the Caffeine-only `get`). It did not survive the
 * FAILED-read case: `stored` returns `None` for both, so an unreadable row was carried
 * forward as `MovieRecord()` and the resolve wrote a film stripped of every rating and
 * every cinema.
 *
 * `None` is not available as the signal here — `runTmdbStageSync` documents it as "TMDB has
 * no match", and `resolveTmdbOnce` turns that into `markMissing` + `tmdbNoMatch = true`.
 * Poisoning a film as unmatched is a different wrong answer, so a failed read THROWS
 * instead: `resolveTmdbOnce` already wraps the stage in a `Try` and treats `Failure` as
 * "will retry", which is exactly the deferral this wants.
 */
class TmdbCarryForwardReadFailureSpec extends AnyFlatSpec with Matchers {

  private val Title  = "The Visitor"
  private val Year   = Some(2022)
  private val TmdbId = 881487

  private class StubTmdb extends GetOnlyHttpFetch {
    private val routes = Seq(
      "/search/movie" -> s"""{"results":[
        |{"id":$TmdbId,"title":"Gość","original_title":"The Visitor","release_date":"2022-10-07","popularity":1.4}
        |]}""".stripMargin,
      s"/movie/$TmdbId/external_ids" -> s"""{"id":$TmdbId,"imdb_id":"tt15558152"}"""
    )
    override def get(url: String): String =
      routes.collectFirst { case (frag, body) if url.contains(frag) => body }
        .getOrElse(throw new RuntimeException(s"unstubbed TMDB URL: $url"))
  }

  /** The row as stored: one cinema, and a rating a refresher owns. */
  private def seed = MovieRecord(
    imdbRating = Some(7.4),
    data = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(Title))))

  private def serviceOver(cache: MovieCache) =
    new MovieService(cache, new InProcessEventBus(), new TmdbClient(http = new StubTmdb, apiKey = Some("stub")),
      tmdbIdCache = ResolutionCache.passthrough)

  "a TMDB resolve whose carry-forward read FAILS" should
    "defer rather than write the film stripped of its cinemas and ratings" in {
    val repository = new InMemoryMovieRepository(Seq((Title, Year, seed)))
    // The read the carry-forward depends on cannot be answered. Everything else is normal.
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer) {
      override private[services] def storedChecked(key: CacheKey): (Option[MovieRecord], Boolean) =
        (None, false)
    }

    val resolved = serviceOver(cache).resolveTmdbOnce(Title, Year, None, None, force = false)

    withClue("the stage must report not-resolved so the task retries: ")(resolved shouldBe false)
    // The stored row is untouched — still its cinema, still its rating.
    val stored = repository.findAll().find(_.title == Title).map(_.record)
    withClue(s"stored row after the failed resolve: $stored: ") {
      stored.map(_.data.keySet)  shouldBe Some(Set[Source](CinemaCityPoznanPlaza))
      stored.flatMap(_.imdbRating) shouldBe Some(7.4)
    }
  }

  // The read succeeding must still resolve normally — the guard must not cost a resolve.
  it should "resolve normally when the carry-forward read succeeds" in {
    val repository = new InMemoryMovieRepository(Seq((Title, Year, seed)))
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer)

    serviceOver(cache).resolveTmdbOnce(Title, Year, None, None, force = false) shouldBe true

    val stored = repository.findAll().find(_.title == Title).map(_.record)
    withClue(s"stored row after a healthy resolve: $stored: ") {
      stored.flatMap(_.tmdbId)     shouldBe Some(TmdbId)
      // the resolve adds its own Tmdb slot; the CINEMA slot must survive alongside it
      stored.map(_.data.keySet).getOrElse(Set.empty) should contain (CinemaCityPoznanPlaza: Source)
      stored.flatMap(_.imdbRating) shouldBe Some(7.4)   // the refresher's score preserved
    }
  }
}
