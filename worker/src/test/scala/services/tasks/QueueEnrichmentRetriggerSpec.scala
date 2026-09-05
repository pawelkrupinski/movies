package services.tasks

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Country, Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CacheKey, RetriggerKind}

import scala.concurrent.duration._

class QueueEnrichmentRetriggerSpec extends AnyFlatSpec with Matchers {

  private def fixtureFor(country: Country) = {
    val queue   = new InMemoryTaskQueue
    val fresh   = new InMemoryFreshnessStore
    val trigger = new QueueEnrichmentRetrigger(queue, fresh, country, titleNormalizer)
    (queue, fresh, trigger)
  }

  private def fixture = fixtureFor(Country.Poland)

  private def drain(queue: InMemoryTaskQueue): Seq[Task] =
    Iterator.continually(queue.claim("w", 5.minutes)).takeWhile(_.isDefined).flatten.toSeq

  private val filmKey    = CacheKey("Ojczyzna", Some(2026), titleNormalizer)
  private val resolved = MovieRecord(tmdbId = Some(1437696), imdbId = Some("tt37304295"),
    data = Map[Source, SourceData](Tmdb -> SourceData(originalTitle = Some("Fatherland"))))

  "QueueEnrichmentRetrigger" should "enqueue exactly the tasks for the given kinds — one per case" in {
    val (queue, _, trigger) = fixture
    trigger.retrigger(filmKey, resolved, Set(RetriggerKind.ImdbRating, RetriggerKind.FilmwebRating))
    drain(queue).map(_.taskType).toSet shouldBe Set(TaskType.ImdbRating, TaskType.FilmwebRating)
  }

  it should "DROP a FilmwebRating retrigger in a non-Filmweb country (UK) — no handler-less task" in {
    // The merge decision (common) is country-blind and emits FilmwebRating for any
    // resolved row; the UK worker wires no Filmweb handler, so enqueuing it would
    // hot-loop forever in `waiting` ("no handler for FilmwebRating"). The country
    // gate must drop it while still enqueuing the sources UK DOES wire.
    val (queue, _, trigger) = fixtureFor(Country.UnitedKingdom)
    trigger.retrigger(filmKey, resolved,
      Set(RetriggerKind.ImdbRating, RetriggerKind.FilmwebRating, RetriggerKind.RtRating, RetriggerKind.McRating))
    drain(queue).map(_.taskType).toSet shouldBe
      Set(TaskType.ImdbRating, TaskType.RtRating, TaskType.McRating) // FilmwebRating dropped
  }

  it should "KEEP a FilmwebRating retrigger in a Filmweb country (Poland)" in {
    val (queue, _, trigger) = fixtureFor(Country.Poland)
    trigger.retrigger(filmKey, resolved, Set(RetriggerKind.FilmwebRating))
    drain(queue).map(_.taskType) shouldBe Seq(TaskType.FilmwebRating)
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

  // The hint pair is cinema-only, BOTH halves of it. The director half already was
  // (see the comment at the call site); the original-title half was still reading
  // `record.originalTitle`, which is the Tmdb slot verbatim — so a re-resolve was
  // handed the PREVIOUS resolution's own original title as if a cinema had
  // published it, and then ranked it as cinema evidence. That is the leak
  // `MovieRecord.cinemaOriginalTitle` exists to close, and how "Mistyczka" kept
  // re-confirming another film's identity.
  //
  // A derived original title still REACHES the resolver — `resolveTmdbId` mines
  // every slot's `originalTitle` into its candidate set, which is what lets a
  // Filmweb-supplied original crack a film TMDB missed. It just no longer arrives
  // wearing a cinema's clothes.
  it should "enqueue a ResolveTmdb task hinting the CINEMA original title, not the Tmdb slot's" in {
    val (queue, _, trigger) = fixture
    val withCinema = resolved.copy(data = resolved.data +
      (Helios -> SourceData(title = Some("Ojczyzna"), originalTitle = Some("Ojczyzna (oryg.)"))))
    trigger.retrigger(filmKey, withCinema, Set(RetriggerKind.ResolveTmdb))
    val tasks = drain(queue)
    tasks should have size 1
    val task = tasks.head
    task.taskType shouldBe TaskType.ResolveTmdb
    task.dedupKey shouldBe EnrichTaskKeys.resolveTmdbDedup(filmKey.cleanTitle, filmKey.year)
    EnrichTaskKeys.originalTitleOf(task.payload) shouldBe Some("Ojczyzna (oryg.)")
  }

  it should "hint NO original title when only the derived slots carry one" in {
    val (queue, _, trigger) = fixture
    trigger.retrigger(filmKey, resolved, Set(RetriggerKind.ResolveTmdb))
    EnrichTaskKeys.originalTitleOf(drain(queue).head.payload) shouldBe None
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
