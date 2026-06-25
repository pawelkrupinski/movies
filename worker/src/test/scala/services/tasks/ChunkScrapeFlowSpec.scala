package services.tasks

import models.{CinemaMovie, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{ChunkedCinemaScraper, CinemaScraper}
import services.events.TaskFinished
import services.freshness.{FreshnessKind, InMemoryFreshnessStore}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.collection.mutable
import scala.concurrent.duration._

/**
 * End-to-end of the chunked-scrape mechanism over the real `InMemoryTaskQueue`,
 * store, planner, handlers, coordinator and reaper — no client converted yet, a
 * fake `ChunkedCinemaScraper` stands in. Covers the happy path, per-chunk retry,
 * the supersession/conflict guard, and partial reduce on an abandoned run.
 */
class ChunkScrapeFlowSpec extends AnyFlatSpec with Matchers {
  import HandlerOutcome._

  private val cinema = Multikino
  private val cinemaName    = cinema.displayName
  private val now    = Instant.parse("2026-06-25T00:00:00Z")
  private val stale  = 15.minutes

  private def film(title: String, day: Int): CinemaMovie =
    CinemaMovie(Movie(title), cinema, None, Some(s"https://f/$title"), None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2026, 6, day, 18, 0), None)), Map.empty, None)

  /** A fake chunked cinema: each cinemaName maps to its slice; keys in `failOnce` throw
   *  on their first `fetchChunk` then succeed; `planThrows` fails enumeration. */
  private class FakeChunked(slices: Map[String, Seq[CinemaMovie]], failOnce: Set[String] = Set.empty,
                            failAlways: Set[String] = Set.empty, planThrows: Boolean = false) extends ChunkedCinemaScraper {
    private val failed = mutable.Set.empty[String]
    val cinema: models.Cinema = ChunkScrapeFlowSpec.this.cinema
    def scrapeHosts: Set[String] = Set("fake.pl")
    def planChunks(): Seq[String] = if (planThrows) throw new RuntimeException("nav down") else slices.keys.toSeq.sorted
    def fetchChunk(k: String): Seq[CinemaMovie] =
      if (failAlways.contains(k)) throw new RuntimeException(s"chunk $k permanently down")
      else if (failOnce.contains(k) && failed.add(k)) throw new RuntimeException(s"chunk $k transient")
      else slices.getOrElse(k, Nil)
  }

  private class Harness(scraper: FakeChunked, clock: Clock = Clock.fixed(now, ZoneOffset.UTC)) {
    val queue     = new InMemoryTaskQueue
    val store     = new InMemoryChunkScrapeStore
    val freshness = new InMemoryFreshnessStore
    val published = mutable.ListBuffer.empty[Seq[CinemaMovie]]
    val publishScrape: CinemaScraper => Unit = s => { published += scala.util.Try(s.fetch()).getOrElse(Seq.empty); () }
    private val map = Map(cinemaName -> (scraper: ChunkedCinemaScraper))
    val planner = new ChunkScrapePlanner(map, store, queue, publishScrape, stale, clock)
    val chunkH  = new ScrapeChunkHandler(map, store, clock)
    val reduceH = new ScrapeChunkReduceHandler(map, store, publishScrape, freshness, clock)
    val coord   = new ChunkScrapeCoordinator(store, queue)
    def reaper(c: Clock) = new ChunkScrapeReaper(store, queue, coord, staleAfter = stale, clock = c)

    /** Claim+handle every currently-claimable task once; on a finished ScrapeChunk
     *  fire the coordinator (as the EventBus subscription does in prod). Rescheduled
     *  tasks are held back so the pass terminates. Returns tasks processed. */
    def drain(at: Instant = now): Int = {
      var n = 0
      var next = queue.claim("w", 30.seconds, at)
      while (next.isDefined) {
        val task = next.get
        val handler = if (task.taskType == TaskType.ScrapeChunk) chunkH else reduceH
        handler.handle(task) match {
          case Done | Skipped =>
            queue.complete(task.id, "w")
            if (task.taskType == TaskType.ScrapeChunk)
              coord.onTaskFinished(TaskFinished(task.taskType, task.dedupKey, task.payload))
          case Reschedule(err) => queue.release(task.id, "w", err, Some(at.plusSeconds(60)))
        }
        n += 1
        next = queue.claim("w", 30.seconds, at)
      }
      n
    }
  }

  "a chunked scrape" should "fan out, gather, and publish the merged listing once every chunk lands" in {
    val h = new Harness(new FakeChunked(Map(
      "2026-06-25" -> Seq(film("Dune", 25)),
      "2026-06-26" -> Seq(film("Dune", 26), film("Wicked", 26)))))
    h.planner.plan(cinemaName) shouldBe 2
    h.queue.waitingCount(TaskType.ScrapeChunk) shouldBe 2

    h.drain() // chunks → coordinator enqueues reduce → reduce publishes

    h.published should have size 1
    val byTitle = h.published.head.map(m => m.movie.title -> m.showtimes.size).toMap
    byTitle shouldBe Map("Dune" -> 2, "Wicked" -> 1) // Dune merged across both days
    h.freshness.isFresh(ScrapeCinemaHandler.dedupKey(cinema), FreshnessKind.CinemaScrape, now) shouldBe true
    h.store.activeRun(cinemaName) shouldBe None // run cleaned up
  }

  it should "NOT reduce until every expected chunk has landed" in {
    val h = new Harness(new FakeChunked(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25)))))
    val runId = { h.planner.plan(cinemaName); h.store.activeRun(cinemaName).get.runId }
    h.chunkH.handle(Task("t", TaskType.ScrapeChunk, "d", ChunkScrapeKeys.chunkPayload(cinemaName,runId, "a"), 1)) shouldBe Done
    h.coord.maybeReduce(cinemaName, runId) shouldBe false // only 1 of 2 chunks
    h.queue.waitingCount(TaskType.ScrapeChunkReduce) shouldBe 0
  }

  it should "retry a failing chunk and still complete" in {
    val h = new Harness(new FakeChunked(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25))), failOnce = Set("b")))
    h.planner.plan(cinemaName) shouldBe 2
    h.drain()                       // chunk b fails once (rescheduled, held back), a stores
    h.published shouldBe empty      // run not complete yet
    h.drain(now.plusSeconds(120))   // b retried → stores → coordinator → reduce
    h.published should have size 1
    h.published.head.map(_.movie.title).toSet shouldBe Set("X", "Y")
  }

  it should "refuse a second concurrent run for the same cinema (the conflict guard)" in {
    val h = new Harness(new FakeChunked(Map("a" -> Seq(film("X", 25)))))
    h.planner.plan(cinemaName) shouldBe 1
    val runId = h.store.activeRun(cinemaName).get.runId
    h.planner.plan(cinemaName) shouldBe 0 // a run is already active → no second run
    h.store.activeRun(cinemaName).get.runId shouldBe runId
    h.queue.waitingCount(TaskType.ScrapeChunk) shouldBe 1 // no extra chunk tasks
  }

  it should "drop a stale run's chunk once a superseding run is active" in {
    val h = new Harness(new FakeChunked(Map("a" -> Seq(film("X", 25)))))
    h.planner.plan(cinemaName)
    val stale1 = h.store.activeRun(cinemaName).get.runId
    // Supersede: a fresh plan after the run goes stale starts a new run.
    val later  = now.plusSeconds(16 * 60)
    val h2runId = h.store.startRun(cinemaName, Seq("a"), later, stale).get
    h2runId should not be stale1
    // The leftover stale-run chunk task is dropped, not stored.
    h.chunkH.handle(Task("t", TaskType.ScrapeChunk, "d", ChunkScrapeKeys.chunkPayload(cinemaName,stale1, "a"), 1)) shouldBe Skipped
    h.store.storedKeys(cinemaName, stale1) shouldBe empty
  }

  it should "partial-reduce an abandoned run via the backstop reaper" in {
    // Chunk 'b' is permanently dead, so the run never completes on its own.
    val h = new Harness(new FakeChunked(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25))), failAlways = Set("b")))
    h.planner.plan(cinemaName)
    h.drain()                 // 'a' stores; 'b' fails (rescheduled, held back); not complete
    h.published shouldBe empty
    val past = now.plusSeconds(16 * 60)
    h.reaper(Clock.fixed(past, ZoneOffset.UTC)).tick() shouldBe 1 // abandoned → enqueue partial reduce
    h.drain(past)
    h.published should have size 1
    h.published.head.map(_.movie.title) shouldBe Seq("X") // partial: only the chunk that landed
  }

  it should "complete a run whose chunks are processed by DIFFERENT worker instances (shared store)" in {
    // Two instances share ONE queue + ONE store (prod = shared Mongo); each has
    // its own coordinator (its own in-process EventBus). No data is duplicated and
    // exactly one reduce/publish happens regardless of which instance did what.
    val queue = new InMemoryTaskQueue
    val store = new InMemoryChunkScrapeStore
    val freshness = new InMemoryFreshnessStore
    val published = mutable.ListBuffer.empty[Seq[CinemaMovie]]
    val publish: CinemaScraper => Unit = s => { published += scala.util.Try(s.fetch()).getOrElse(Seq.empty); () }
    val scraper = new FakeChunked(Map("a" -> Seq(film("X", 25)), "b" -> Seq(film("Y", 25))))
    val map = Map(cinemaName -> (scraper: ChunkedCinemaScraper))
    val clk = Clock.fixed(now, ZoneOffset.UTC)
    val planner = new ChunkScrapePlanner(map, store, queue, publish, stale, clk)
    val chunkH  = new ScrapeChunkHandler(map, store, clk)
    val reduceH = new ScrapeChunkReduceHandler(map, store, publish, freshness, clk)
    val coordA  = new ChunkScrapeCoordinator(store, queue) // instance A
    val coordB  = new ChunkScrapeCoordinator(store, queue) // instance B

    planner.plan(cinemaName) // one instance plans; the run + chunk tasks are shared

    // Instance A handles one chunk and fires ITS coordinator (not complete yet).
    val ta = queue.claim("A", 30.seconds, now).get
    chunkH.handle(ta) shouldBe Done; queue.complete(ta.id, "A")
    coordA.onTaskFinished(TaskFinished(ta.taskType, ta.dedupKey, ta.payload))
    queue.waitingCount(TaskType.ScrapeChunkReduce) shouldBe 0

    // Instance B handles the other chunk; its coordinator reads the shared store,
    // sees the run complete, and enqueues the single reduce.
    val tb = queue.claim("B", 30.seconds, now).get
    chunkH.handle(tb) shouldBe Done; queue.complete(tb.id, "B")
    coordB.onTaskFinished(TaskFinished(tb.taskType, tb.dedupKey, tb.payload))
    queue.waitingCount(TaskType.ScrapeChunkReduce) shouldBe 1

    // Either instance reduces — exactly once.
    val tr = queue.claim("A", 30.seconds, now).get
    reduceH.handle(tr) shouldBe Done; queue.complete(tr.id, "A")
    published should have size 1
    published.head.map(_.movie.title).toSet shouldBe Set("X", "Y")
    store.activeRun(cinemaName) shouldBe None
  }

  it should "re-process a chunk whose worker instance crashed mid-run (lease expiry)" in {
    val h = new Harness(new FakeChunked(Map("a" -> Seq(film("X", 25)))))
    h.planner.plan(cinemaName)
    // Instance A claims the chunk on a 1s lease, then "crashes" (never completes).
    val ta = h.queue.claim("A", 1.second, now).get
    ta.taskType shouldBe TaskType.ScrapeChunk
    // Lease expires → the task returns to waiting and another instance drains it.
    h.queue.reapExpiredLeases(now.plusSeconds(2)) should be >= 1
    h.drain(now.plusSeconds(2))
    h.published should have size 1
    h.published.head.map(_.movie.title) shouldBe Seq("X")
  }

  it should "record the scrape's failure when chunk-plan enumeration throws" in {
    val h = new Harness(new FakeChunked(Map.empty, planThrows = true))
    h.planner.plan(cinemaName) shouldBe 0
    h.store.activeRun(cinemaName) shouldBe None  // no run started
    h.published should have size 1        // the failure was published through the recorder path
  }
}
