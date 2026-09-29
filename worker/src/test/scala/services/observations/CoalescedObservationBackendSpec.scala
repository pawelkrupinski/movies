package services.observations

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.Instant
import java.util.concurrent.{Callable, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters._

/** Concurrent readers of the lookup store: what their reads cost in round-trips, and that the
 *  store's own lock no longer takes them one at a time. */
class CoalescedObservationBackendSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-09-28T20:00:00Z")
  private def stored(key: String) =
    StoredObservation(key, "api.themoviedb.org", s"hash-$key", Array.emptyByteArray, t0, t0, t0.plusSeconds(86400), current = true)

  /** An in-memory backend whose every round-trip takes a while, counted. */
  private final class SlowBackend(keys: Seq[String]) extends InMemoryObservationBackend {
    keys.foreach(key => insert(stored(key)))
    val trips = new AtomicInteger
    override def current(key: String): Option[StoredObservation] = { trips.incrementAndGet(); Thread.sleep(5); super.current(key) }
    override def currents(keys: Seq[String]): Map[String, StoredObservation] = {
      trips.incrementAndGet(); Thread.sleep(5); keys.flatMap(key => super.current(key).map(key -> _)).toMap
    }
  }

  private def concurrently[A](n: Int)(read: Int => A): Seq[A] = {
    val pool = Executors.newFixedThreadPool(n, Thread.ofVirtual().factory())
    try pool.invokeAll((0 until n).map(i => (() => read(i)): Callable[A]).asJava, 30, TimeUnit.SECONDS).asScala.map(_.get).toSeq
    finally pool.shutdownNow()
  }

  "coalesced reads" should "answer each reader its own key, in far fewer round-trips than readers" in {
    val keys    = (0 until 64).map(i => s"GET https://api.themoviedb.org/3/movie/$i")
    val backend = new SlowBackend(keys.take(48))                            // 16 never observed
    val reads   = concurrently(64)(i => new CoalescedObservationBackend(backend).current(keys(i)).map(_.key))
    reads shouldBe keys.zipWithIndex.map { case (key, i) => Option.when(i < 48)(key) }
    // Sixty-four separate backends would each lead alone: coalescing is per backend instance.
    val shared  = new CoalescedObservationBackend(backend)
    backend.trips.set(0)
    concurrently(64)(i => shared.current(keys(i)).map(_.key)) shouldBe reads
    backend.trips.get should be <= 16
  }

  it should "keep several batches in flight at once, so readers never queue behind one round-trip" in {
    val inFlight = new AtomicInteger
    val overlap  = new AtomicInteger
    val backend  = new InMemoryObservationBackend {
      override def currents(keys: Seq[String]): Map[String, StoredObservation] = {
        overlap.accumulateAndGet(inFlight.incrementAndGet(), math.max)
        val until = System.nanoTime() + 1_000_000_000L
        while (inFlight.get < 2 && System.nanoTime() < until) Thread.onSpinWait()
        overlap.accumulateAndGet(inFlight.get, math.max)
        try super.currents(keys) finally inFlight.decrementAndGet()
      }
    }
    val coalesced = new CoalescedObservationBackend(backend, maxBatch = 1)
    concurrently(2)(i => coalesced.current(s"k$i")) shouldBe Seq(None, None)
    overlap.get shouldBe 2
  }

  it should "fail every reader of a batch whose round-trip failed, and serve the next batch anew" in {
    var failing = true
    val backend = new InMemoryObservationBackend {
      override def currents(keys: Seq[String]): Map[String, StoredObservation] =
        if (failing) throw new IllegalStateException("store down") else super.currents(keys)
    }
    backend.insert(stored("k"))
    val coalesced = new CoalescedObservationBackend(backend)
    an[IllegalStateException] should be thrownBy coalesced.current("k")
    failing = false
    coalesced.current("k").map(_.key) shouldBe Some("k")
  }

  "the store's lookups" should "read side by side, even of one key: only a renewal is a write it serialises" in {
    val search   = LookupQuery.of("GET", "https://api.themoviedb.org/3/search/movie?query=Belle")
    val inFlight = new AtomicInteger
    val overlap  = new AtomicInteger
    // Each read lingers until another is in flight (or a second passes).
    val backend  = new InMemoryObservationBackend {
      override def current(key: String): Option[StoredObservation] = {
        overlap.accumulateAndGet(inFlight.incrementAndGet(), math.max)
        val until = System.nanoTime() + 1_000_000_000L
        while (inFlight.get < 2 && System.nanoTime() < until) Thread.onSpinWait()
        overlap.accumulateAndGet(inFlight.get, math.max)
        try super.current(key) finally inFlight.decrementAndGet()
      }
    }
    val store = new ObservationStore(new InMemoryObservationBackend, backend, new MutableClock(t0))
    store.observeLookup(search, LookupAnswer.Body("{}"))
    overlap.set(0)
    concurrently(2)(_ => store.lookup(search).map(_.answer)) shouldBe Seq.fill(2)(Some(LookupAnswer.Body("{}")))
    overlap.get shouldBe 2
  }
}
