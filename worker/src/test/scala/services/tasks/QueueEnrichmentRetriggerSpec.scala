package services.tasks

import models.{MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CacheKey, RetriggerKind}

import scala.concurrent.duration._

class QueueEnrichmentRetriggerSpec extends AnyFlatSpec with Matchers {

  private def fixture = {
    val queue   = new InMemoryTaskQueue
    val fresh   = new InMemoryFreshnessStore
    val trigger = new QueueEnrichmentRetrigger(queue, fresh)
    (queue, fresh, trigger)
  }

  private def drain(queue: InMemoryTaskQueue): Seq[Task] =
    Iterator.continually(queue.claim("w", 5.minutes)).takeWhile(_.isDefined).flatten.toSeq

  private val filmKey    = CacheKey("Ojczyzna", Some(2026))
  private val resolved = MovieRecord(tmdbId = Some(1437696), imdbId = Some("tt37304295"),
    data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Fatherland"))))

  "QueueEnrichmentRetrigger" should "enqueue exactly the tasks for the given kinds — one per case" in {
    val (queue, _, trigger) = fixture
    trigger.retrigger(filmKey, resolved, Set(RetriggerKind.ImdbRating, RetriggerKind.FilmwebRating))
    drain(queue).map(_.taskType).toSet shouldBe Set(TaskType.ImdbRating, TaskType.FilmwebRating)
  }

  it should "INVALIDATE the tmdbId-keyed freshness stamp so the re-fetch isn't deduped away" in {
    val (queue, fresh, trigger) = fixture
    val dedup = RatingTasks.dedupKey(FreshnessKind.ImdbRating, filmKey, resolved.tmdbId)
    fresh.markFresh(dedup, FreshnessKind.ImdbRating)
    fresh.lastFetchedAt(dedup) should not be empty            // fresh before

    trigger.retrigger(filmKey, resolved, Set(RetriggerKind.ImdbRating))

    fresh.lastFetchedAt(dedup) shouldBe empty                 // cleared, so the handler re-gate runs it
    drain(queue).map(_.dedupKey) should contain (dedup)       // enqueued under the same filmKey
  }

  it should "enqueue a ResolveTmdb task carrying the title/year + originalTitle hint" in {
    val (queue, _, trigger) = fixture
    trigger.retrigger(filmKey, resolved, Set(RetriggerKind.ResolveTmdb))
    val tasks = drain(queue)
    tasks should have size 1
    val task = tasks.head
    task.taskType shouldBe TaskType.ResolveTmdb
    task.dedupKey shouldBe EnrichTaskKeys.resolveTmdbDedup(filmKey.cleanTitle, filmKey.year)
    EnrichTaskKeys.originalTitleOf(task.payload) shouldBe Some("Fatherland")
  }

  it should "enqueue a ResolveImdbId task with a search title" in {
    val (queue, _, trigger) = fixture
    trigger.retrigger(filmKey, resolved.copy(searchTitle = Some("Fatherland")), Set(RetriggerKind.ResolveImdbId))
    val tasks = drain(queue)
    tasks should have size 1
    tasks.head.taskType shouldBe TaskType.ResolveImdbId
    EnrichTaskKeys.searchTitleOf(tasks.head.payload) shouldBe Some("Fatherland")
  }
}
