package services.readmodel

import models.{CityScreening, ResolvedMovie, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Unit cover for the read cache's read surface. `allScreenings()` exists for the
 * dev `/debug/readmodel` dump, which needs every cached screening across cities
 * (the per-city `screeningsForCity` is the request-time read key, not a dump).
 *
 * The `backstopTick` cases pin the CPU-saving contract: the periodic backstop
 * must NOT re-read the whole corpus while the change streams keep the model
 * current (that decode burst is what stalled requests on the single-vCPU web
 * box), yet must still fall back to a full reload when a stream dies or a count
 * drifts.
 */
class WebReadModelSpec extends AnyFlatSpec with Matchers {

  private def ratings = ResolvedRatings(None, None, None, "", None, "", None, "")
  private def movie(id: String) =
    ResolvedMovie(id, id, None, None, Nil, None, None, Nil, Nil, Nil, Nil, None, Nil, ratings, 0.0)
  private def screening(id: String, film: String, city: String) =
    CityScreening(id, film, city, "Cinema " + id, None, Nil)

  "allScreenings" should "return every cached screening flattened across all city buckets" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    repository.upsertScreening(screening("s2", "belle|2021", "krakow"))
    repository.upsertScreening(screening("s3", "belle|2021", "wroclaw"))
    val rm = new WebReadModel(repository)
    rm.reload()

    rm.allScreenings().map(_._id) should contain theSameElementsAs Seq("s1", "s2", "s3")
    // The per-city read key still partitions them — the dump is the union.
    rm.screeningsForCity("wroclaw").map(_._id) should contain theSameElementsAs Seq("s1", "s3")
  }

  it should "be empty when the cache holds no screenings" in {
    new WebReadModel(new InMemoryReadModelRepository).allScreenings() shouldBe empty
  }

  // ── Backstop: cheap drift check, not an unconditional full reload ────────────

  private def started(repository: InMemoryReadModelRepository): WebReadModel = {
    val rm = new WebReadModel(repository)
    rm.start() // hydrates once + opens the watches; reset the counters so we only
    repository.findAllMoviesCalls.set(0)     // measure what the backstop tick itself does
    repository.findAllScreeningsCalls.set(0)
    rm
  }

  "backstopTick" should "skip the full reload while streams are live and counts match" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = started(repository)

    rm.backstopTick()

    repository.findAllMoviesCalls.get()     shouldBe 0
    repository.findAllScreeningsCalls.get() shouldBe 0
    rm.stop()
  }

  it should "fall back to a full reload when a change stream has died" in {
    val repository = new InMemoryReadModelRepository
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = started(repository)

    repository.failMovieStream()
    rm.backstopTick()

    repository.findAllMoviesCalls.get()     should be >= 1
    repository.findAllScreeningsCalls.get() should be >= 1
    rm.stop()
  }

  it should "reload when a server-side count drifts from the in-memory model" in {
    // countScreenings reports one more than was streamed in — standing in for a
    // delivered event the applier dropped, which a count-blind backstop misses.
    val repository = new InMemoryReadModelRepository {
      override def countScreenings(): Long = super.countScreenings() + 1
    }
    repository.upsertMovie(movie("belle|2021"))
    repository.upsertScreening(screening("s1", "belle|2021", "wroclaw"))
    val rm = started(repository)

    rm.backstopTick()

    repository.findAllScreeningsCalls.get() should be >= 1
    rm.stop()
  }
}
