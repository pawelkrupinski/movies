package services.enrichment

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{CacheKey, CaffeineMovieCache, InMemoryMovieRepository}
import services.tasks.BulkRefreshResult

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/**
 * The URL-then-score corpus walk on its own. A refresher whose hooks record
 * when they run pins the walk's contract here — re-resolve, then fetch off the
 * URL the row NOW holds, then persist through the cache's write path, then tell
 * the cadence — instead of leaving it to be inferred from the MC / RT specs.
 */
class CacheRefresherSpec extends AnyFlatSpec with Matchers {

  private val urlA = "https://scores.example/a"
  private val urlB = "https://scores.example/b"
  private val urlC = "https://scores.example/c"
  private val urlD = "https://scores.example/d"

  /** Scores by URL; an unmapped URL throws on fetch. `discover` maps a row to
   *  what its re-resolution does: find and store a URL, or fail. The fake keeps
   *  its URL and score in the Metacritic fields — the walk itself is
   *  field-agnostic. */
  private class RecordingRefresher(
    cache:    CaffeineMovieCache,
    scores:   Map[String, Option[Int]],
    discover: Map[CacheKey, Try[String]],
    recorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => ()
  ) extends CacheRefresher(cache, recorder) {
    val events = new ConcurrentLinkedQueue[String]

    override protected def sourceName: String = "Fake"
    protected def refreshOne(key: CacheKey): Option[String] = None

    private[services] def refreshAll(): BulkRefreshResult =
      refreshAllUrlThenScore[Int](
        walkLabel     = "Fake refresh",
        urlOf         = _.metacriticUrl,
        scoreOf       = _.metascore,
        rediscoverUrl = (key, _) => {
          events.add(s"resolve ${key.cleanTitle}")
          discover.get(key) match {
            case None               => Success(false)
            case Some(Success(url)) => Success(cache.putIfPresent(key, _.copy(metacriticUrl = Some(url))))
            case Some(Failure(e))   => Failure(e)
          }
        },
        fetchScore    = url => {
          events.add(s"fetch $url")
          scores.getOrElse(url, throw new RuntimeException(s"HTTP 503 $url"))
        },
        withScore     = (row, fresh) => { events.add(s"persist ${row.metacriticUrl.get}"); row.copy(metascore = fresh) },
        badge         = s => s"$s!"
      )
  }

  "refreshAllUrlThenScore" should "re-resolve, fetch off the URL the row now holds, persist, and tell the cadence — per row, in that order" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("A", None, MovieRecord(tmdbId = Some(1), metacriticUrl = Some(urlA), metascore = Some(1))),  // score moves
      ("B", None, MovieRecord(tmdbId = None,    metacriticUrl = Some(urlB), metascore = Some(2))),  // no tmdbId: no re-resolve; unchanged
      ("C", None, MovieRecord(tmdbId = Some(3), metacriticUrl = None,       metascore = None)),     // URL discovered, then scored off it
      ("D", None, MovieRecord(tmdbId = None,    metacriticUrl = Some(urlD), metascore = Some(4)))   // fetch throws
    ))
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val keyOf = (title: String) => cache.keyOf(title, None)
    val cadence = new ConcurrentLinkedQueue[(CacheKey, Option[Int], Option[String])]
    val refresher = new RecordingRefresher(cache,
      scores   = Map(urlA -> Some(9), urlB -> Some(2), urlC -> Some(7)),
      discover = Map(keyOf("C") -> Success(urlC)),
      recorder = (key, tmdbId, value) => cadence.add((key, tmdbId, value)))

    val result = refresher.refreshAll()

    val events = refresher.events.asScala.toSeq
    def rowEvents(title: String, url: String) =
      events.filter(e => e.endsWith(s" ${keyOf(title).cleanTitle}") || e.endsWith(s" $url"))
    rowEvents("A", urlA) shouldBe Seq(s"resolve ${keyOf("A").cleanTitle}", s"fetch $urlA", s"persist $urlA")
    rowEvents("B", urlB) shouldBe Seq(s"fetch $urlB")
    rowEvents("C", urlC) shouldBe Seq(s"resolve ${keyOf("C").cleanTitle}", s"fetch $urlC", s"persist $urlC")
    rowEvents("D", urlD) shouldBe Seq(s"fetch $urlD")

    cache.get(keyOf("A")).flatMap(_.metascore) shouldBe Some(9)
    cache.get(keyOf("B")).flatMap(_.metascore) shouldBe Some(2)
    cache.get(keyOf("C")).map(r => (r.metacriticUrl, r.metascore)) shouldBe Some((Some(urlC), Some(7)))
    cache.get(keyOf("D")).flatMap(_.metascore) shouldBe Some(4)   // a failed fetch leaves the stored score alone

    // The cadence hears the badge of every moved score, keyed by the snapshot row's tmdbId.
    cadence.asScala.toSet shouldBe Set((keyOf("A"), Some(1), Some("9!")), (keyOf("C"), Some(3), Some("7!")))

    result.walked     shouldBe Some(4)
    result.changed    shouldBe Some(2)
    result.discovered shouldBe Some(1)
    result.failed     shouldBe Some(1)
    result.message should include ("2 score(s) changed, 1 URL(s) newly discovered, 1 failed")
  }

  it should "count a failed re-resolution and still refresh the score off the URL the row already had" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("E", None, MovieRecord(tmdbId = Some(5), metacriticUrl = Some(urlA), metascore = Some(5)))))
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val key = cache.keyOf("E", None)
    val refresher = new RecordingRefresher(cache,
      scores   = Map(urlA -> Some(6)),
      discover = Map(key -> Failure(new RuntimeException("soft-blocked"))))

    val result = refresher.refreshAll()

    refresher.events.asScala.toSeq shouldBe Seq(s"resolve ${key.cleanTitle}", s"fetch $urlA", s"persist $urlA")
    cache.get(key).flatMap(_.metascore) shouldBe Some(6)
    result.failed  shouldBe Some(1)
    result.changed shouldBe Some(1)
    result.message should include ("1 score(s) changed, 0 URL(s) newly discovered, 1 failed")
  }
}
