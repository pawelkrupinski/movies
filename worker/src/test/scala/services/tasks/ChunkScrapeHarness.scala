package services.tasks

import models.{Cinema, CinemaMovie, Multikino}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper}
import services.events.TaskFinished
import services.freshness.InMemoryFreshnessStore

import java.time.{Clock, Instant}
import scala.collection.mutable
import scala.concurrent.duration._

/** A fake chunked cinema: each chunk key maps to its slice; keys in `failOnce` throw
 *  on their first `fetchChunk` then succeed, keys in `failAlways` always throw, keys in
 *  `circuitOpen` fast-fail as a breaker-blocked host would, keys in `gone` 404,
 *  `planThrows` fails enumeration, and `planSaysNoSchedule` answers an empty plan as the
 *  source listing no schedule. */
class FakeChunkedScraper(
  slices:      Map[String, Seq[CinemaMovie]],
  failOnce:    Set[String] = Set.empty,
  failAlways:  Set[String] = Set.empty,
  planThrows:  Boolean = false,
  circuitOpen: Set[String] = Set.empty,
  gone:        Set[String] = Set.empty,
  val cinema:  Cinema = Multikino,
  planSaysNoSchedule: Boolean = false,
  // A page inside these chunks fails while the rest of the chunk reads (a multi-page walk's
  // tolerated failure); `planPageFails` does the same to the plan's day walk.
  pageFailsIn: Set[String] = Set.empty,
  planPageFails: Boolean = false,
  // as `pageFailsIn`, on the chunk's first read only
  pageFailsOnceIn: Set[String] = Set.empty
) extends ChunkedCinemaScraper {
  import FakeChunkedScraper.{CircuitBlockMs, Host}
  private val failed     = mutable.Set.empty[String]
  private val pageFailed = mutable.Set.empty[String]
  def scrapeHosts: Set[String] = Set(Host)
  def planChunks(): Seq[String] =
    if (planThrows) throw new RuntimeException("nav down")
    else {
      if (planPageFails) services.cinemas.common.ListingReads.pageFailed(new RuntimeException("a day probe failed"))
      slices.keys.toSeq.sorted
    }
  override def planSchedule(): services.cinemas.common.ChunkPlan =
    if (planSaysNoSchedule) services.cinemas.common.ChunkPlan.NoScheduleListed else super.planSchedule()
  def fetchChunk(k: String): Seq[CinemaMovie] =
    if (circuitOpen.contains(k)) throw new tools.CircuitOpenException(Host, CircuitBlockMs)
    else if (gone.contains(k)) throw new tools.HttpStatusException(404, "GET", s"https://$Host/$k", None)
    else if (failAlways.contains(k)) throw new RuntimeException(s"chunk $k permanently down")
    else if (failOnce.contains(k) && failed.add(k)) throw new RuntimeException(s"chunk $k transient")
    else {
      if (pageFailsIn.contains(k) || (pageFailsOnceIn.contains(k) && pageFailed.add(k))) services.cinemas.common.ListingReads.pageFailed(new RuntimeException(s"a page of chunk $k failed"))
      slices.getOrElse(k, Nil)
    }
}

object FakeChunkedScraper {
  val Host           = "fake.pl"
  /** How long a `circuitOpen` chunk says its host is blocked for. */
  val CircuitBlockMs = 45000L
}

/**
 * The chunked-scrape stack over one cinema — the real `InMemoryTaskQueue`, store,
 * planner, chunk + reduce handlers, coordinator and reaper — publishing through
 * `publish`. [[rescraping]] re-points it at another scraper for the same cinema while
 * keeping the queue, store and freshness policy, as a later scrape of the venue would.
 */
class ChunkScrapeHarness private (
  scraper:          ChunkedCinemaScraper,
  val publish:      CinemaScraper => Unit,
  clock:            Clock,
  val store:        InMemoryChunkScrapeStore,
  val queue:        InMemoryTaskQueue,
  val freshness:    InMemoryFreshnessStore,
  val policy:       ScrapeFreshnessPolicy,
  staleAfter:       FiniteDuration
) {
  def this(
    scraper:      ChunkedCinemaScraper,
    publish:      CinemaScraper => Unit,
    clock:        Clock,
    store:        InMemoryChunkScrapeStore = new InMemoryChunkScrapeStore,
    freshness:    InMemoryFreshnessStore = new InMemoryFreshnessStore,
    venueCadence: Option[VenueCadenceStore] = None,
    staleAfter:   FiniteDuration = 15.minutes
  ) = this(scraper, publish, clock, store, new InMemoryTaskQueue, freshness,
    new ScrapeFreshnessPolicy(freshness, clock = clock, venueCadence = venueCadence), staleAfter)

  val cinemaName: String = scraper.cinema.displayName
  private val map        = Map(cinemaName -> scraper)
  private val runTimeout = ChunkScrapePlanner.RunTimeout(staleAfter)

  val planner = new ChunkScrapePlanner(map, store, queue, publish, policy, runTimeout, clock)
  val chunkH  = new ScrapeChunkHandler(map, store, clock)
  val reduceH = new ScrapeChunkReduceHandler(map, store, publish, policy, clock)
  val coord   = new ChunkScrapeCoordinator(store, queue, _root_.tools.SpecClock.Pinned)
  def reaper(c: Clock) = new ChunkScrapeReaper(store, queue, coord, staleAfter = runTimeout, clock = c)

  def rescraping(other: ChunkedCinemaScraper): ChunkScrapeHarness =
    new ChunkScrapeHarness(other, publish, clock, store, queue, freshness, policy, staleAfter)

  /** The handler a claimed task runs on. */
  def handlerFor(task: Task): TaskHandler = if (task.taskType == TaskType.ScrapeChunk) chunkH else reduceH

  /** Claim+handle every currently-claimable task once; on a finished ScrapeChunk fire
   *  the coordinator (as the EventBus subscription does in prod). Rescheduled and
   *  deferred tasks are held back so the pass terminates. Returns tasks processed.
   *  Bounded like `StagingQueueEndToEndSpec`'s pump: a task released to an instant
   *  this pass has already reached is claimable again immediately, and an unguarded
   *  loop then spins the suite instead of failing it. */
  def drain(at: Instant): Int = {
    import HandlerOutcome._
    var n = 0
    var next = queue.claim("w", 30.seconds, at)
    while (next.isDefined && n < 500) {
      val task = next.get
      handlerFor(task).handle(task) match {
        case Done | Skipped =>
          queue.complete(task.id, "w")
          if (task.taskType == TaskType.ScrapeChunk)
            coord.onTaskFinished(TaskFinished(task.taskType, task.dedupKey, task.payload))
        case Reschedule(err) => queue.release(task.id, "w", err, Some(at.plusSeconds(60)))
        // Mirrors TaskWorker: a deferred chunk waits out the block it named and
        // gets its attempt back, since it never ran.
        case Deferred(err, notBefore) =>
          queue.release(task.id, "w", err, Some(notBefore.getOrElse(at.plusSeconds(60))), refundAttempt = true)
      }
      n += 1
      next = queue.claim("w", 30.seconds, at)
    }
    n
  }
}
