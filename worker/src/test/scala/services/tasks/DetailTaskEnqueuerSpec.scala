package services.tasks

import models.{KinoApollo, KinoBulgarska}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.FakeDetailEnricher
import services.events.CinemaMovieAdded
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}
import services.events.InProcessEventBus

import scala.concurrent.duration._

class DetailTaskEnqueuerSpec extends AnyFlatSpec with Matchers {

  private def fixture = {
    val cache    = new CaffeineMovieCache(new InMemoryMovieRepo(), new InProcessEventBus())
    val queue    = new InMemoryTaskQueue
    val fresh    = new InMemoryFreshnessStore
    val enricher = new FakeDetailEnricher(KinoApollo, "kino-apollo")
    val enqueuer = new DetailTaskEnqueuer(enricher, cache, queue, fresh)
    (cache, queue, fresh, enqueuer)
  }

  private def waiting(queue: InMemoryTaskQueue) = queue.countByState().getOrElse(TaskState.Waiting, 0L)

  "DetailTaskEnqueuer" should "enqueue one EnrichDetails task for its cinema's new movie" in {
    val (cache, queue, _, enqueuer) = fixture
    enqueuer.onCinemaMovieAdded(CinemaMovieAdded(KinoApollo, "Dune", None, Some("http://ref")))
    waiting(queue) shouldBe 1L
    // dedup key matches what the handler/reaper would build for the same film
    val dk = EnrichDetailsTasks.dedupKey("kino-apollo", cache.keyOf("Dune", None))
    queue.claim("w", 5.minutes).map(_.dedupKey) shouldBe Some(dk)
  }

  it should "ignore an event for a different cinema" in {
    val (_, queue, _, enqueuer) = fixture
    enqueuer.onCinemaMovieAdded.isDefinedAt(CinemaMovieAdded(KinoBulgarska, "Dune", None, Some("http://ref"))) shouldBe false
    waiting(queue) shouldBe 0L
  }

  it should "ignore an event with no filmUrl" in {
    val (_, queue, _, enqueuer) = fixture
    enqueuer.onCinemaMovieAdded.isDefinedAt(CinemaMovieAdded(KinoApollo, "Dune", None, None)) shouldBe false
    waiting(queue) shouldBe 0L
  }

  it should "not enqueue when the detail is already fresh" in {
    val (cache, queue, fresh, enqueuer) = fixture
    fresh.markFresh(EnrichDetailsTasks.dedupKey("kino-apollo", cache.keyOf("Dune", None)), FreshnessKind.DetailEnrich)
    enqueuer.onCinemaMovieAdded(CinemaMovieAdded(KinoApollo, "Dune", None, Some("http://ref")))
    waiting(queue) shouldBe 0L
  }
}
