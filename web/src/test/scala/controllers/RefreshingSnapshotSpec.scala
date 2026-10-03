package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, ZoneOffset}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
 * The /debug snapshots: a country switch must be answered from memory, never wait
 * on the whole-collection re-read behind it, and never show a failed re-read as data.
 */
class RefreshingSnapshotSpec extends AnyFlatSpec with Matchers {

  private given ExecutionContext = ExecutionContext.global

  /** A clock the spec moves by hand. */
  private final class ManualClock(start: Instant) extends Clock {
    val now = new AtomicReference(start)
    def advance(by: FiniteDuration): Unit = { now.updateAndGet(_.plusMillis(by.toMillis)); () }
    override def instant(): Instant = now.get()
    override def getZone = ZoneOffset.UTC
    override def withZone(zone: java.time.ZoneId): Clock = this
  }

  /** A read the spec can hold open, counting how often it ran and answering 1, 2, 3… */
  private final class GatedRead {
    val reads = new AtomicInteger(0)
    @volatile var gate: CountDownLatch = new CountDownLatch(0)
    @volatile var failNext = false
    def apply(): Int = {
      gate.await(10, TimeUnit.SECONDS)
      if (failNext) { failNext = false; throw new IllegalStateException("mirror went away") }
      reads.incrementAndGet()
    }
  }

  private def eventually(cond: => Boolean): Unit = {
    val deadline = System.nanoTime() + 5.seconds.toNanos
    while (!cond && System.nanoTime() < deadline) Thread.sleep(5)
    cond shouldBe true
  }

  private val Start = Instant.parse("2026-10-03T12:00:00Z")

  private def snapshotOf(read: GatedRead, clock: Clock, newest: Option[Instant] = None, store: SnapshotStore = SnapshotStore.none) =
    new RefreshingSnapshot[Int]("spec", () => read(), () => newest, refreshAfter = 15.seconds, clock, store)

  /** What a previous app run left behind. */
  private final class MapStore extends SnapshotStore {
    val saved = scala.collection.concurrent.TrieMap.empty[String, DebugSnapshot[?]]
    def load[A](key: String): Option[DebugSnapshot[A]] = saved.get(key).map(_.asInstanceOf[DebugSnapshot[A]])
    def save[A](key: String, snapshot: DebugSnapshot[A]): Unit = saved(key) = snapshot
  }

  "get" should "wait for the very first read" in {
    val read = new GatedRead
    snapshotOf(read, new ManualClock(Start)).get().value shouldBe 1
  }

  it should "answer from the current snapshot without re-reading while it is young" in {
    val read     = new GatedRead
    val snapshot = snapshotOf(read, new ManualClock(Start))
    snapshot.get()
    (1 to 5).foreach(_ => snapshot.get().value shouldBe 1)
    read.reads.get() shouldBe 1
  }

  it should "serve the old snapshot AT ONCE while a stale one's re-read is still running, then the new one" in {
    val read     = new GatedRead
    val clock    = new ManualClock(Start)
    val snapshot = snapshotOf(read, clock)
    snapshot.get()
    clock.advance(16.seconds)
    read.gate = new CountDownLatch(1)   // the re-read hangs until released

    val started = System.nanoTime()
    snapshot.get().value shouldBe 1
    (System.nanoTime() - started).nanos should be < 1.second

    read.gate.countDown()
    eventually(snapshot.get().value == 2)
  }

  it should "start only ONE re-read however many stale gets arrive while it runs" in {
    val read     = new GatedRead
    val clock    = new ManualClock(Start)
    val snapshot = snapshotOf(read, clock)
    snapshot.get()
    clock.advance(16.seconds)
    read.gate = new CountDownLatch(1)
    (1 to 20).foreach(_ => snapshot.get())
    read.gate.countDown()
    eventually(snapshot.get().value == 2)
    read.reads.get() shouldBe 2
  }

  it should "keep the previous snapshot when a re-read fails" in {
    val read     = new GatedRead
    val clock    = new ManualClock(Start)
    val snapshot = snapshotOf(read, clock)
    snapshot.get()
    clock.advance(16.seconds)
    read.failNext = true
    snapshot.get().value shouldBe 1
    // The failed re-read has finished: the next stale get may try again, and still shows 1 until it lands.
    eventually(!read.failNext)
    Thread.sleep(50)
    snapshot.get().value shouldBe 1
    eventually(snapshot.get().value == 2)
  }

  it should "throw when the FIRST read fails, so the page shows an error, not an empty table" in {
    val read = new GatedRead
    read.failNext = true
    an[IllegalStateException] should be thrownBy snapshotOf(read, new ManualClock(Start)).get()
  }

  it should "carry the mirror's newest stamp as of the read" in {
    val stamp = Start.minusSeconds(42)
    snapshotOf(new GatedRead, new ManualClock(Start), Some(stamp)).get().mirrorNewest shouldBe Some(stamp)
  }

  it should "serve what a previous run stored AT ONCE on its first get, and re-read it behind" in {
    val store = new MapStore
    store.saved("spec") = DebugSnapshot(41, None, Some(Start.minusSeconds(3600)))
    val read     = new GatedRead
    read.gate    = new CountDownLatch(1)          // the fresh read hangs until released
    val snapshot = snapshotOf(read, new ManualClock(Start), store = store)

    val started = System.nanoTime()
    snapshot.get().value shouldBe 41
    (System.nanoTime() - started).nanos should be < 1.second

    read.gate.countDown()
    eventually(snapshot.get().value == 1)
  }

  it should "store every read, stamped with when it was taken" in {
    val store = new MapStore
    snapshotOf(new GatedRead, new ManualClock(Start), store = store).get()
    eventually(store.saved.contains("spec"))
    store.saved("spec") shouldBe DebugSnapshot(1, None, Some(Start))
  }

  "refreshIfOlderThan" should "re-read a snapshot past the threshold and leave a younger one alone" in {
    val read     = new GatedRead
    val clock    = new ManualClock(Start)
    val snapshot = snapshotOf(read, clock)
    scala.concurrent.Await.ready(snapshot.refreshIfOlderThan(60.seconds), 5.seconds)   // nothing read yet: warms it, and says when
    read.reads.get() shouldBe 1
    clock.advance(30.seconds)
    snapshot.refreshIfOlderThan(60.seconds)
    Thread.sleep(50)
    read.reads.get() shouldBe 1
    clock.advance(31.seconds)
    snapshot.refreshIfOlderThan(60.seconds)
    eventually(read.reads.get() == 2)
  }
}
