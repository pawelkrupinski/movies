package services.tasks

import models.{CinemaMovie, Movie, Multikino, Showtime}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper}
import services.events.TaskFinished
import services.freshness.InMemoryFreshnessStore
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.InMemoryScheduledRunStore
import tools.{ManualScheduler, MutableClock}

import java.time.{Duration, Instant, LocalDateTime}
import scala.collection.mutable
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

/** The chunked-scrape reaper's timing and its reads of `scrape_runs`. It used to read every
 *  run doc once a minute to find the few runs that had timed out; a run's only time-based
 *  outcome is its own deadline, so it now waits on that deadline and sweeps the whole
 *  collection only at boot and once an hour, for runs whose deadline died with a process. */
class ChunkScrapeReaperSpec extends AnyFlatSpec with Matchers {

  private val cinema  = Multikino.displayName
  private val t0      = Instant.parse("2026-10-04T10:00:00Z")
  private val timeout = ChunkScrapePlanner.RunTimeout(15.minutes)

  /** Counts the whole-collection reads the reaper makes. */
  private class CountingStore extends InMemoryChunkScrapeStore {
    val sweeps = new AtomicInteger
    override def activeRuns(): Seq[ChunkRun] = { sweeps.incrementAndGet(); super.activeRuns() }
  }

  private class Rig(start: Instant = t0) {
    val clock     = new MutableClock(start)
    val scheduler = new ManualScheduler(clock)
    val store     = new CountingStore
    val queue     = new InMemoryTaskQueue
    val coord     = new ChunkScrapeCoordinator(store, queue)
    val runStore  = new InMemoryScheduledRunStore
    /** A reaper as a freshly booted process builds it: nothing armed in memory. */
    def reaper(): ChunkScrapeReaper =
      new ChunkScrapeReaper(store, queue, coord, staleAfter = timeout, runStore = runStore, clock = clock, scheduler = scheduler)
    def reduces: Int = queue.waitingCount(TaskType.ScrapeChunkReduce)
    def advanceTo(at: Instant): Unit = scheduler.advance(Duration.between(clock.instant(), at))
  }

  /** The planner + handlers over `scraper` on `rig`'s store and queue, arming each run's
   *  deadline on `armedBy`, as the wiring does. */
  private class Flow(rig: Rig, scraper: ChunkedCinemaScraper, armedBy: ChunkScrapeReaper) {
    val published = mutable.ListBuffer.empty[Seq[CinemaMovie]]
    private val publish: CinemaScraper => Unit = s => { published += s.fetch(); () }
    private val policy  = new ScrapeFreshnessPolicy(new InMemoryFreshnessStore, clock = rig.clock)
    private val map     = Map(cinema -> scraper)
    val planner = new ChunkScrapePlanner(map, rig.store, rig.queue, publish, policy, timeout, rig.clock, runStarted = armedBy.armDeadline)
    private val chunkH  = new ScrapeChunkHandler(map, rig.store, rig.clock)
    private val reduceH = new ScrapeChunkReduceHandler(map, rig.store, publish, policy, rig.clock)

    /** Run every claimable task once; `announce` = whether a finished chunk reaches the
     *  coordinator (the TaskFinished event). A failing task backs off for an hour. */
    def drain(announce: Boolean = true): Unit =
      Iterator.continually(rig.queue.claim("w", 30.seconds, rig.clock.instant())).takeWhile(_.isDefined).flatten.foreach { task =>
        val handler = if (task.taskType == TaskType.ScrapeChunk) chunkH else reduceH
        handler.handle(task) match {
          case HandlerOutcome.Done | HandlerOutcome.Skipped =>
            rig.queue.complete(task.id, "w")
            if (announce && task.taskType == TaskType.ScrapeChunk)
              rig.coord.onTaskFinished(TaskFinished(task.taskType, task.dedupKey, task.payload))
          case _ => rig.queue.release(task.id, "w", None, Some(rig.clock.instant().plusSeconds(3600)))
        }
      }
  }

  private def film(title: String): CinemaMovie =
    CinemaMovie(Movie(title), Multikino, None, Some(s"https://f/$title"), None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2026, 10, 4, 18, 0), None)), Map.empty, None)
  private val twoChunks = Map("a" -> Seq(film("X")), "b" -> Seq(film("Y")))

  "The chunk-scrape reaper" should "read the run collection once at boot and then once an hour, not every minute" in {
    val rig = new Rig()
    rig.reaper().start()
    rig.scheduler.advance(Duration.ofHours(3))
    // Boot sweep (0:00:45), then 1:00:45 and 2:00:45. The minute poll made 180.
    rig.store.sweeps.get() shouldBe 3
  }

  it should "reduce a run on its last chunk's completion, with no sweep or deadline run" in {
    val rig  = new Rig()
    val flow = new Flow(rig, new FakeChunkedScraper(twoChunks), rig.reaper())
    flow.planner.plan(cinema)
    flow.drain()
    flow.published.map(_.map(_.movie.title).toSet) shouldBe Seq(Set("X", "Y"))
    rig.store.activeRun(cinema) shouldBe None
    rig.store.sweeps.get() shouldBe 0
  }

  it should "partial-reduce a run whose chunks never finish at the deadline its planner armed" in {
    val rig  = new Rig()
    val flow = new Flow(rig, new FakeChunkedScraper(twoChunks, failAlways = Set("b")), rig.reaper())
    flow.planner.plan(cinema)
    flow.drain() // 'a' lands; 'b' fails and backs off past the deadline
    rig.advanceTo(t0.plusSeconds(15 * 60))
    rig.reduces shouldBe 0
    rig.advanceTo(t0.plusSeconds(15 * 60 + 1))
    rig.reduces shouldBe 1
    flow.drain()
    flow.published.map(_.map(_.movie.title)) shouldBe Seq(Seq("X"))
    rig.store.sweeps.get() shouldBe 0 // the reaper never started: the armed deadline alone did it
  }

  it should "fully reduce, at its deadline, a run whose last completion was never announced" in {
    val rig  = new Rig()
    val flow = new Flow(rig, new FakeChunkedScraper(twoChunks), rig.reaper())
    flow.planner.plan(cinema)
    flow.drain(announce = false) // both chunks land; their TaskFinished events are lost
    rig.reduces shouldBe 0
    rig.advanceTo(t0.plusSeconds(15 * 60 + 1))
    rig.reduces shouldBe 1
    flow.drain()
    flow.published.map(_.map(_.movie.title).toSet) shouldBe Seq(Set("X", "Y"))
  }

  it should "do nothing at the deadline of a run already reduced" in {
    val rig  = new Rig()
    val flow = new Flow(rig, new FakeChunkedScraper(Map("a" -> Seq(film("X")))), rig.reaper())
    flow.planner.plan(cinema)
    flow.drain()
    rig.advanceTo(t0.plusSeconds(16 * 60))
    rig.reduces shouldBe 0
    flow.published should have size 1
  }

  it should "recover a run no process holds a deadline for, and reduce it at that deadline" in {
    val rig   = new Rig()
    // A run another (since crashed) process started; this one boots five minutes later.
    val runId = rig.store.startRun(cinema, Seq("a", "b"), t0, timeout.value).get
    rig.store.storeChunk(cinema, runId, "a", StoredChunk("[]"), t0)
    rig.clock.advance(Duration.ofMinutes(5))
    rig.reaper().start()

    rig.advanceTo(t0.plusSeconds(15 * 60))
    rig.reduces shouldBe 0 // not abandoned yet: 'b' may still land
    rig.advanceTo(t0.plusSeconds(15 * 60 + 1))
    rig.reduces shouldBe 1 // the partial reduce, on the deadline the boot sweep armed
    rig.store.sweeps.get() shouldBe 1
  }

  it should "partial-reduce at boot a run that went stale while no process was up" in {
    val rig = new Rig()
    rig.store.startRun(cinema, Seq("a", "b"), t0, timeout.value)
    rig.clock.advance(Duration.ofMinutes(20))
    rig.reaper().start()
    rig.scheduler.advance(Duration.ofSeconds(45))
    rig.reduces shouldBe 1
  }

  it should "recover, at the hourly sweep, a run started by another replica after this one booted" in {
    val rig = new Rig()
    rig.reaper().start()
    rig.scheduler.advance(Duration.ofMinutes(10))
    rig.store.startRun(cinema, Seq("a", "b"), rig.clock.instant(), timeout.value) // no deadline here
    rig.advanceTo(t0.plusSeconds(60 * 60))
    rig.reduces shouldBe 0
    rig.advanceTo(t0.plusSeconds(60 * 60 + 45))
    rig.reduces shouldBe 1
  }

  it should "reduce once even when the deadline and a sweep both find the run abandoned" in {
    val rig = new Rig()
    rig.store.startRun(cinema, Seq("a", "b"), t0, timeout.value)
    val reaper = rig.reaper()
    reaper.start()
    rig.scheduler.advance(Duration.ofMinutes(16)) // the boot-armed deadline fires
    reaper.sweep()                                 // and a sweep finds the same stale run
    rig.reduces shouldBe 1
  }
}
