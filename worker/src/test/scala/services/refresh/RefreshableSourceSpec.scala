package services.refresh

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.enrichment.CacheRefresher
import services.movies.{CacheKey, CaffeineMovieCache, InMemoryMovieRepository, MovieCache}
import services.titles.TitleNormalizer
import services.resolution.ResolutionCache
import services.tasks.{BulkRefreshResult, HandlerOutcome, InMemoryBulkTaskResultStore, RefreshableSource, RefreshableSources, Task}
import services.tasktypes.TaskType

import scala.collection.mutable

/** An operator's refresh of one stored source for given keys (`RefreshSource` task): the task names the source and its
 *  keys, the handler runs exactly that source over exactly those keys and records what it came to; a source the worker
 *  does not wire fails, never retried. A rating source refreshes the rows of the films named — their memoised links
 *  forgotten first — and no other row. */
class RefreshableSourceSpec extends AnyFlatSpec with Matchers {

  private final class Recording(val name: String, fails: Boolean = false) extends RefreshableSource {
    val keys = "ids"
    val asked = mutable.Buffer.empty[Seq[String]]
    def refresh(keys: Seq[String]): BulkRefreshResult = {
      asked += keys
      if (fails) throw new IllegalStateException("TMDB down")
      BulkRefreshResult.message(s"${keys.size} refreshed")
    }
  }

  private def task(source: String, keys: String*) =
    Task("t", TaskType.RefreshSource, RefreshableSources.dedupKey(source, keys), RefreshableSources.payload(source, keys), attempts = 1)

  "a RefreshSource task" should "carry its source and keys, a repeat of the same request collapsing into one" in {
    val t = task("tmdb-films", "2", "1", "2")
    RefreshableSources.sourceOf(t) shouldBe "tmdb-films"
    RefreshableSources.keysOf(t) shouldBe Seq("1", "2")
    RefreshableSources.dedupKey("imdb", Seq("2", "1")) shouldBe RefreshableSources.dedupKey("imdb", Seq("1", "2"))
    RefreshableSources.dedupKey("imdb", Seq("1")) should not be RefreshableSources.dedupKey("rt", Seq("1"))
    TaskType.byName("RefreshSource") shouldBe Some(TaskType.RefreshSource)
  }

  "the handler" should "run the named source over the task's keys, and only that source, recording what it did" in {
    val (tmdb, rt) = (new Recording("tmdb-films"), new Recording("rt"))
    val results    = new InMemoryBulkTaskResultStore
    new RefreshSourceHandler(new RefreshableSources(Seq(tmdb, rt)), results, tools.SpecClock.Pinned).handle(task("tmdb-films", "7", "8")) shouldBe
      HandlerOutcome.Done
    tmdb.asked shouldBe Seq(Seq("7", "8"))
    rt.asked shouldBe empty
    val recorded = results.latest()(TaskType.RefreshSource)
    (recorded.succeeded, recorded.message) shouldBe (true, "tmdb-films: 2 refreshed")
  }

  it should "fail a source this worker does not wire, done rather than retried" in {
    val results = new InMemoryBulkTaskResultStore
    new RefreshSourceHandler(new RefreshableSources(Seq(new Recording("imdb"))), results, tools.SpecClock.Pinned).handle(task("filmweb", "1")) shouldBe
      HandlerOutcome.Done
    results.latest()(TaskType.RefreshSource).succeeded shouldBe false
    results.latest()(TaskType.RefreshSource).message should include("no such source")
  }

  it should "record a refresh that threw and hand it back to the queue's retry" in {
    val results = new InMemoryBulkTaskResultStore
    an[IllegalStateException] should be thrownBy
      new RefreshSourceHandler(new RefreshableSources(Seq(new Recording("imdb", fails = true))), results, tools.SpecClock.Pinned).handle(task("imdb", "1"))
    results.latest()(TaskType.RefreshSource).message shouldBe "imdb: failed: TMDB down"
  }

  "the registry" should "refuse two sources under one name" in {
    an[IllegalArgumentException] should be thrownBy new RefreshableSources(Seq(new Recording("imdb"), new Recording("imdb")))
  }

  private val normalizer = TitleNormalizer.forCountry(models.Country.Poland)

  /** A rating source whose every refresh moves its row's badge, recording each row it was asked. */
  private final class Refresher(rows: MovieCache, cadence: mutable.Buffer[(String, Option[Int], Option[String])] = mutable.Buffer.empty,
                                failing: Set[String] = Set.empty)
      extends CacheRefresher(rows, (key, tmdbId, value) => cadence += ((key.cleanTitle, tmdbId, value))) {
    val refreshed = mutable.Buffer.empty[String]
    protected def sourceName: String = "RT"
    protected def refreshOne(key: CacheKey): Option[String] = {
      refreshed += key.cleanTitle
      if (failing(key.cleanTitle.toLowerCase(java.util.Locale.ROOT))) throw new IllegalStateException("RT answered 503")
      Some("91%")
    }
    private[services] def refreshAll(): BulkRefreshResult = BulkRefreshResult.message("unused")
  }
  private final class Links extends ResolutionCache {
    val forgotten = mutable.Buffer.empty[String]
    def getOrResolve(hintKey: String)(resolve: => Option[String]): Option[String] = resolve
    override def forget(cleanTitle: String): Unit = forgotten += cleanTitle
  }

  "a rating source" should "forget the links of the named films' rows and refresh exactly those rows" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
      ("Oczy Ghany", Some(2025), MovieRecord(tmdbId = Some(1525208))),
      ("Żar", Some(2025), MovieRecord(tmdbId = Some(1658185))),
      ("Lalka", Some(2026), MovieRecord(tmdbId = Some(42)))), normalizer = normalizer), normalizer = normalizer, clock = tools.SpecClock.Pinned)
    val (refresher, links) = (new Refresher(cache), new Links)
    val result = new RatingRefreshSource("rt", refresher, cache, links).refresh(Seq("1525208", "1658185", "999"))
    // the rows as the cache keys them: the two films', never Lalka's
    refresher.refreshed.map(_.toLowerCase(java.util.Locale.ROOT)).sorted shouldBe Seq("oczyghany", "zar")
    links.forgotten shouldBe refresher.refreshed
    (result.walked, result.changed) shouldBe (Some(2), Some(2))
    result.message should include("no row holds 999")
  }

  // The queue's RatingHandler tells the adaptive cadence of every badge it moves; an operator's refresh runs no task, so
  // a move it made unreported was the gap a later per-row refresh mis-reads as a fresh change (CacheRefresher's
  // recordBulkChange, which the full-corpus walk reports through for the same reason).
  it should "tell the adaptive cadence of each badge it moved, under the row's TMDB id" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
      ("Żar", Some(2025), MovieRecord(tmdbId = Some(1658185)))), normalizer = normalizer), normalizer = normalizer, clock = tools.SpecClock.Pinned)
    val cadence = mutable.Buffer.empty[(String, Option[Int], Option[String])]
    new RatingRefreshSource("rt", new Refresher(cache, cadence), cache, new Links).refresh(Seq("1658185"))
    cadence.map { case (title, tmdbId, value) => (title.toLowerCase(java.util.Locale.ROOT), tmdbId, value) } shouldBe
      Seq(("zar", Some(1658185), Some("91%")))
  }

  // One row's failure ended the task: the rows after it went unasked, the ratings already moved went unreported, and the
  // queue's retry forgot and fetched every row again.
  it should "count a row whose refresh throws as failed and refresh the rest" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
      ("Oczy Ghany", Some(2025), MovieRecord(tmdbId = Some(1525208))),
      ("Żar", Some(2025), MovieRecord(tmdbId = Some(1658185)))), normalizer = normalizer), normalizer = normalizer, clock = tools.SpecClock.Pinned)
    val refresher = new Refresher(cache, failing = Set("oczyghany"))
    val result    = new RatingRefreshSource("rt", refresher, cache, new Links).refresh(Seq("1525208", "1658185"))
    refresher.refreshed.map(_.toLowerCase(java.util.Locale.ROOT)).sorted shouldBe Seq("oczyghany", "zar")
    (result.walked, result.changed, result.failed) shouldBe (Some(2), Some(1), Some(1))
    result.message should include("1 failed")
  }

  // The operator's refresh of given films exists to correct them, as the /tasks button corrects the whole corpus. It
  // re-read the score off the row's stored URL alone (the per-row path the queue takes), so a row on another film's page
  // stayed there however often it was refreshed, though its memoised links were forgotten for it.
  it should "derive a URL-keyed source's link again for a row that already holds one, as the full-corpus walk does" in {
    val stored = "https://www.rottentomatoes.com/m/the_odyssey"
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository(Seq(
      ("The Odyssey", Some(2026), MovieRecord(tmdbId = Some(1368337), rottenTomatoesUrl = Some(stored)))), normalizer = normalizer),
      normalizer = normalizer, clock = tools.SpecClock.Pinned)
    val asked = mutable.Set.empty[String]
    val rt = services.enrichment.SiteRatingsOf.rottenTomatoes(cache, new clients.HttpTmdbClient(clients.tools.FailingHttpFetch.Unexpected, apiKey = None),
      new services.enrichment.RottenTomatoesClient(http = new clients.tools.ScriptedByUrlHttpFetch(url => { asked += url; tools.UpstreamNotFound(url) })))(using tools.SpecClock.latestTitleYear)
    new RatingRefreshSource("rt", rt, cache, new Links).refresh(Seq("1368337"))
    asked should contain("https://www.rottentomatoes.com/m/theodyssey")   // the link derived again
    asked should contain(stored)                                           // and the score still read off the row's URL
  }
}
