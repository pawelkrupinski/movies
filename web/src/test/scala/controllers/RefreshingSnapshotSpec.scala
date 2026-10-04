package controllers

import tools.SpecTimeouts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import tools.{Eventually, MutableClock}

/**
 * The /debug snapshots: a country switch must be answered from memory, never wait
 * on the whole-collection re-read behind it, and never show a failed re-read as data.
 */
class RefreshingSnapshotSpec extends AnyFlatSpec with Matchers {

  private given ExecutionContext = ExecutionContext.global

  /** A read the spec can hold open, counting how often it ran and answering 1, 2, 3… */
  private final class GatedRead {
    val reads = new AtomicInteger(0)
    @volatile var gate: CountDownLatch = new CountDownLatch(0)
    @volatile var failNext = false
    def apply(): Int = {
      gate.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
      if (failNext) { failNext = false; throw new IllegalStateException("mirror went away") }
      reads.incrementAndGet()
    }
  }

  private def eventually(cond: => Boolean): Unit = { Eventually.eventually(cond shouldBe true, pollMs = 5); () }

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
    snapshotOf(read, new MutableClock(Start)).get().value shouldBe 1
  }

  it should "answer from the current snapshot without re-reading while it is young" in {
    val read     = new GatedRead
    val snapshot = snapshotOf(read, new MutableClock(Start))
    snapshot.get()
    (1 to 5).foreach(_ => snapshot.get().value shouldBe 1)
    read.reads.get() shouldBe 1
  }

  it should "serve the old snapshot AT ONCE while a stale one's re-read is still running, then the new one" in {
    val read     = new GatedRead
    val clock    = new MutableClock(Start)
    val snapshot = snapshotOf(read, clock)
    snapshot.get()
    clock.advanceSeconds(16)
    read.gate = new CountDownLatch(1)   // the re-read hangs until released

    val started = System.nanoTime()
    snapshot.get().value shouldBe 1
    (System.nanoTime() - started).nanos should be < 1.second

    read.gate.countDown()
    eventually(snapshot.get().value == 2)
  }

  it should "start only ONE re-read however many stale gets arrive while it runs" in {
    val read     = new GatedRead
    val clock    = new MutableClock(Start)
    val snapshot = snapshotOf(read, clock)
    snapshot.get()
    clock.advanceSeconds(16)
    read.gate = new CountDownLatch(1)
    (1 to 20).foreach(_ => snapshot.get())
    read.gate.countDown()
    eventually(snapshot.get().value == 2)
    read.reads.get() shouldBe 2
  }

  it should "keep the previous snapshot when a re-read fails" in {
    val read     = new GatedRead
    val clock    = new MutableClock(Start)
    val snapshot = snapshotOf(read, clock)
    snapshot.get()
    clock.advanceSeconds(16)
    read.failNext = true
    snapshot.get().value shouldBe 1
    // The failed re-read has finished: the next stale get may try again, and still shows 1 until it lands.
    eventually(!read.failNext)
    snapshot.get().value shouldBe 1
    eventually(snapshot.get().value == 2)
  }

  it should "throw when the FIRST read fails, so the page shows an error, not an empty table" in {
    val read = new GatedRead
    read.failNext = true
    an[IllegalStateException] should be thrownBy snapshotOf(read, new MutableClock(Start)).get()
  }

  it should "carry the mirror's newest stamp as of the read" in {
    val stamp = Start.minusSeconds(42)
    snapshotOf(new GatedRead, new MutableClock(Start), Some(stamp)).get().mirrorNewest shouldBe Some(stamp)
  }

  it should "serve what a previous run stored AT ONCE on its first get, and re-read it behind" in {
    val store = new MapStore
    store.saved("spec") = DebugSnapshot(41, None, Some(Start.minusSeconds(3600)))
    val read     = new GatedRead
    read.gate    = new CountDownLatch(1)          // the fresh read hangs until released
    val snapshot = snapshotOf(read, new MutableClock(Start), store = store)

    val started = System.nanoTime()
    snapshot.get().value shouldBe 41
    (System.nanoTime() - started).nanos should be < 1.second

    read.gate.countDown()
    eventually(snapshot.get().value == 1)
  }

  it should "store every read, stamped with when it was taken" in {
    val store = new MapStore
    snapshotOf(new GatedRead, new MutableClock(Start), store = store).get()
    eventually(store.saved.contains("spec"))
    store.saved("spec") shouldBe DebugSnapshot(1, None, Some(Start))
  }

  // The read's in-flight marker was cleared by a callback queued AFTER its future completed, so a
  // stale ask landing between the two joined the finished read and started none. Stepped one task
  // at a time, the ask lands exactly there.
  it should "have finished a read before its future completes, so the next stale ask starts another" in {
    val read     = new GatedRead
    val clock    = new MutableClock(Start)
    val steps    = new tools.ManualScheduler(clock)
    val snapshot = new RefreshingSnapshot[Int]("spec", () => read(), () => None, refreshAfter = 15.seconds, clock)(
      using ExecutionContext.fromExecutor(steps))
    snapshot.refreshIfOlderThan(60.seconds)
    steps.runUntil(read.reads.get() == 1)   // the read's own task has run; its callbacks have not
    clock.advanceSeconds(61)
    val second = snapshot.refreshIfOlderThan(60.seconds)
    steps.runUntil(second.isCompleted)
    steps.runDue()
    read.reads.get() shouldBe 2
  }

  // A read the executor refused never ran its body, so the marker its body clears stayed set: every
  // later ask joined that failed read and none was ever started again.
  it should "start a fresh read after one the executor refused" in {
    val read     = new GatedRead
    val refusals = new AtomicInteger(1)
    val refusing = new ExecutionContext {
      def execute(task: Runnable): Unit =
        if (refusals.getAndDecrement() > 0) throw new java.util.concurrent.RejectedExecutionException("busy") else task.run()
      def reportFailure(cause: Throwable): Unit = ()
    }
    val snapshot = new RefreshingSnapshot[Int]("spec", () => read(), () => None, refreshAfter = 15.seconds, new MutableClock(Start))(
      using refusing)
    an[java.util.concurrent.RejectedExecutionException] should be thrownBy snapshot.get()
    snapshot.get().value shouldBe 1
  }

  // The in-flight marker clears before a read's save callback runs, so the next read can finish
  // and save first; the older save must not then overwrite it.
  it should "never store an older read over a newer one whose save ran first" in {
    val read  = new GatedRead
    val clock = new MutableClock(Start)
    val store = new MapStore
    val tasks = scala.collection.mutable.ArrayBuffer.empty[Runnable]
    def runAll(): Unit = while (tasks.nonEmpty) { val t = tasks.remove(0); t.run() }
    val queued   = new ExecutionContext {
      def execute(task: Runnable): Unit = tasks += task
      def reportFailure(cause: Throwable): Unit = throw cause
    }
    val snapshot = new RefreshingSnapshot[Int]("spec", () => read(), () => None, refreshAfter = 15.seconds, clock, store)(using queued)
    snapshot.refreshIfOlderThan(60.seconds)
    tasks.remove(0).run()                                    // the first read; its save callback now queued
    val firstSave = tasks.toSeq; tasks.clear()
    clock.advanceSeconds(61)
    snapshot.refreshIfOlderThan(60.seconds)
    runAll()                                                 // the second read, and its save
    firstSave.foreach(_.run())                               // the first read's save, last
    runAll()
    store.saved("spec").value shouldBe 2
  }

  "refreshIfOlderThan" should "re-read a snapshot past the threshold and leave a younger one alone" in {
    val read     = new GatedRead
    val clock    = new MutableClock(Start)
    val snapshot = snapshotOf(read, clock)
    scala.concurrent.Await.ready(snapshot.refreshIfOlderThan(60.seconds), SpecTimeouts.Io)   // nothing read yet: warms it, and says when
    read.reads.get() shouldBe 1
    clock.advanceSeconds(30)
    // Young: completes at once without a read — awaited, so "no read" is an answer, not a race lost.
    scala.concurrent.Await.ready(snapshot.refreshIfOlderThan(60.seconds), SpecTimeouts.Io)
    read.reads.get() shouldBe 1
    clock.advanceSeconds(31)
    snapshot.refreshIfOlderThan(60.seconds)
    eventually(read.reads.get() == 2)
  }
}
