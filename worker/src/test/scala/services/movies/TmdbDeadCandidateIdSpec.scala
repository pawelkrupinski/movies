package services.movies

import clients.TmdbClient
import models.{CinemaCityPoznanPlaza, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.resolution.ResolutionCache
import tools.{GetOnlyHttpFetch, HttpStatusException}

/**
 * TMDB deletes movie entries (duplicates, cancelled productions, re-merged
 * records) while its search/find index keeps serving them for a while, so a
 * search can hand back an id whose `/movie/{id}/external_ids` answers 404.
 *
 * That fetch was the ONE unguarded throw in `lookupTmdb` — `fullDetails` already
 * swallows — and `ResolveTmdbHandler` reschedules every throw as transient with no
 * attempts ceiling, so a dead candidate parked the task at the 30-min backoff cap
 * forever. Live example: the UK row `Blade (2025)` (dir. Yann Demange) sat at
 * `attempts = 20` over six hours, every attempt logging
 * `HTTP 404 for GET .../movie/1715017/external_ids`, holding the "oldest waiting
 * task age" panel on a perfect 1s/s diagonal while it blocked nothing else from
 * ever concluding.
 *
 * A dead id is a PERMANENT answer about that candidate, so the lookup concludes it
 * as a no-match (which the missing-id reaper re-attempts on its own cadence) and
 * forgets the memoised id so the next cycle re-searches rather than replaying the
 * corpse for 24h. Only a genuinely transient failure (5xx/429/IO) still defers.
 */
class TmdbDeadCandidateIdSpec extends AnyFlatSpec with Matchers {

  private val Title  = "The Visitor"
  private val Year   = Some(2022)
  private val TmdbId = 881487

  /** Search resolves normally; the chosen id's cross-references answer `status`. */
  private class StubTmdb(externalIdsStatus: Int) extends GetOnlyHttpFetch {
    private val search = s"""{"results":[
      |{"id":$TmdbId,"title":"Gość","original_title":"The Visitor","release_date":"2022-10-07","popularity":1.4}
      |]}""".stripMargin
    override def get(url: String): String =
      if (url.contains(s"/movie/$TmdbId/external_ids"))
        throw new HttpStatusException(externalIdsStatus, "GET", url, retryAfter = None)
      else if (url.contains("/search/movie")) search
      else throw new RuntimeException(s"unstubbed TMDB URL: $url")
  }

  /** Records whether the dead id's memoised resolution was dropped. */
  private class RecordingResolutionCache extends ResolutionCache {
    var forgotten: Seq[String] = Seq.empty
    def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String] = resolve
    override def forget(cleanTitle: String): Unit = forgotten :+= cleanTitle
  }

  private def seed = MovieRecord(
    imdbRating = Some(7.4),
    data = Map[Source, SourceData](CinemaCityPoznanPlaza -> SourceData(title = Some(Title))))

  private def serviceOver(cache: MovieCache, http: GetOnlyHttpFetch, ids: ResolutionCache) =
    new MovieService(cache, new InProcessEventBus(), new TmdbClient(http = http, apiKey = Some("stub")),
      tmdbIdCache = ids)

  "a search hit whose TMDB id no longer exists" should
    "conclude as a no-match instead of retrying the dead id forever" in {
    val repository = new InMemoryMovieRepository(Seq((Title, Year, seed)))
    val cache      = new CaffeineMovieCache(repository)
    val ids        = new RecordingResolutionCache

    val resolved = serviceOver(cache, new StubTmdb(404), ids)
      .resolveTmdbOnce(Title, Year, None, None, force = false)

    withClue("the stage must CONCLUDE so the task stops rescheduling: ")(resolved shouldBe true)
    val stored = repository.findAll().find(_.title == Title).map(_.record)
    withClue(s"stored row after the dead-candidate resolve: $stored: ") {
      stored.flatMap(_.tmdbId) shouldBe None       // the corpse must never be written on
      stored.map(_.tmdbNoMatch) shouldBe Some(true)
      stored.flatMap(_.imdbRating) shouldBe Some(7.4)                       // nothing else disturbed
      stored.map(_.data.keySet) shouldBe Some(Set[Source](CinemaCityPoznanPlaza))
    }
    withClue("the memoised dead id must be dropped so the next cycle re-searches: ")(
      ids.forgotten should contain (Title))
  }

  it should "still DEFER when the same fetch fails transiently" in {
    val repository = new InMemoryMovieRepository(Seq((Title, Year, seed)))
    val cache      = new CaffeineMovieCache(repository)

    val resolved = serviceOver(cache, new StubTmdb(503), new RecordingResolutionCache)
      .resolveTmdbOnce(Title, Year, None, None, force = false)

    withClue("a 5xx is a real outage — the task must retry, not poison the row: ")(resolved shouldBe false)
    val stored = repository.findAll().find(_.title == Title).map(_.record)
    withClue(s"stored row after the transient failure: $stored: ")(
      stored.map(_.tmdbNoMatch) shouldBe Some(false))
  }
}
