package controllers

import models.Krakow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo, StoredMovieRecord}

import java.time.LocalDateTime
import java.util.concurrent.atomic.AtomicInteger

/**
 * `toCinemaSchedules` builds one section per cinema in a city. The corpus is
 * global (one row per film across all cities), so the per-cinema loop must read
 * a single snapshot of the cache, not re-snapshot for every cinema: each
 * `snapshot()` allocates and title-sorts the whole corpus, so calling it once
 * per cinema turns one page render into N full-corpus snapshots (N = cinemas in
 * the city — 4 for Kraków, 10+ for Warszawa).
 */
class CinemaSchedulesSnapshotSpec extends AnyFlatSpec with Matchers {

  /** Counts how many times the controller reads a cache snapshot. */
  private class CountingCache extends CaffeineMovieCache(new InMemoryMovieRepo(Nil)) {
    val snapshotCalls = new AtomicInteger(0)
    override def snapshot(): Seq[StoredMovieRecord] = {
      snapshotCalls.incrementAndGet()
      super.snapshot()
    }
  }

  "toCinemaSchedules" should "read the cache snapshot once per request, not once per cinema" in {
    val cache   = new CountingCache
    val service = new MovieControllerService(cache)

    service.toCinemaSchedules(Krakow, LocalDateTime.now(Krakow.zoneId))

    cache.snapshotCalls.get() shouldBe 1
  }
}
