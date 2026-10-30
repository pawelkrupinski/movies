package services.identity
package adapters

import models.{CinemaMovie, KinoMuza, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{ManualScheduler, MutableClock, TestWiring}

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration._

/** The history never holds its callers ([[AsyncIdentityHistory]]): with its store blocked outright — Mongo stalled — the
 *  model's drains and the TMDB store's filings go on and finish all the same, a full queue drops and counts, and
 *  whatever was queued is written once the store answers. A blocking insert per write ran Germany's convergence
 *  tick past its 30-minute timeout (red #2301). */
class AsyncIdentityHistorySpec extends AnyFlatSpec with Matchers {
  /** A history store that writes nothing until `open` is counted down: every insert waits on it. `entered` counts down
   *  when the first insert has begun waiting. */
  private final class Blocked extends IdentityHistory {
    val open    = new CountDownLatch(1)
    val entered = new CountDownLatch(1)
    val written = new InMemoryIdentityHistory
    def append(entries: Seq[HistoryEntry]): Unit = { entered.countDown(); open.await(); written.append(entries) }
  }
  private def entry(i: Int) = HistoryEntry(HistoryEntry.Failure, s"u$i", i.toLong, None, None, "b1", Some("http 429"))

  "an append" should "return at once with the store blocked, drop what overflows the queue, and count it" in {
    val store   = new Blocked
    val dropped = new AtomicLong
    val history = new AsyncIdentityHistory(store, n => { dropped.addAndGet(n); () }, capacity = 10)
    // The writer holds one batch out of the queue while the store waits; how much of a burst that batch takes depends on
    // when its thread runs (one entry, or ten more taken mid-burst: 5 dropped on Main #2828's runner). So it is handed
    // exactly one first, and the burst follows only once the store holds it.
    history.append(Seq(entry(1)))
    store.entered.await(tools.SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe true
    (2 to 25).foreach(i => history.append(Seq(entry(i))))
    // the queue's ten fill, and the other fourteen are dropped — never waited for
    dropped.get shouldBe 14L
    store.written.all shouldBe empty
    store.open.countDown()
    history.stopWithin(10.seconds)
    store.written.all.map(_.id) shouldBe (1 to 11).map(i => s"u$i")
  }

  "the model's drains and the TMDB store's filings" should "finish with the history's store blocked outright" in {
    val clock   = new MutableClock(TestWiring.FixedInstant)
    val store   = new Blocked
    val dropped = new AtomicLong
    val history = new AsyncIdentityHistory(store, n => { dropped.addAndGet(n); () })
    val docs    = new HistoryTmdbDocuments(new InMemoryTmdbDocuments, history, clock, "b1")
    val tmdb    = new TmdbStore(docs, clock, history = history, build = "b1")
    val titles  = services.movies.SingleCountryNormalizer.titleNormalizer
    val reads   = new ObservationReads
    def row(cinema: models.Cinema, title: String) = CinemaMovie(Movie(title), cinema, None, Some(s"${cinema.displayName}/$title"), None, Nil, Nil,
      Seq(Showtime(java.time.LocalDateTime.of(2026, 10, 1, 18, 0), None)))
    val service = new IdentityModelService(
      () => new IncrementalResolver(new StoredTmdbLookups(tmdb, "pl-PL", UnansweredTmdbLookups, reads), titles, IdentityCalibration.resolver),
      reads, () => Listing.distinct(Listing.all(Seq(Multikino -> Seq(row(Multikino, "Lalka"))), titles)), titles, 1.second,
      // The spec drains on its own thread, so nothing else may: the model is single-threaded. With a real scheduler, every
      // minute the clock moved put the event's settle deadline in the past, its drain ran at once on the scheduler's
      // thread beside the spec's, and one drain's release of its prefetched documents broke the other's (Main #2701).
      new ManualScheduler(clock), clock = clock, history = history, build = "b1")
    service.takeUp()
    (1 to 50).foreach { i =>
      tmdb.people(s"q$i", Seq(i))
      service.venueScraped(KinoMuza, (1 to i).map(n => row(KinoMuza, s"Film $n")))
      clock.advance(java.time.Duration.ofMinutes(1))
      service.drain()
    }
    store.written.all shouldBe empty   // nothing written yet: the store never answered, and nothing waited on it
    store.open.countDown()
    history.stopWithin(10.seconds)
    (store.written.all.size.toLong + dropped.get) should be > 50L
    store.written.all.map(_.kind).toSet should contain allOf (HistoryEntry.Listing, HistoryEntry.answer(TmdbKind.Query))
  }
}
