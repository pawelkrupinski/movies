package services.tasks

import models.{CinemaMovie, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScrapeRunner, CinemaScraper}
import services.events.InProcessEventBus
import services.freshness.InMemoryFreshnessStore
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * A chunked cinema is scraped one DATE at a time. When some of those chunks never land,
 * `ChunkScrapeReaper` gives up waiting and `ScrapeChunkReduceHandler` publishes whatever
 * did arrive — deliberately, so one dead chunk degrades to a partial listing instead of
 * losing the venue. But it publishes it down the SAME path a complete scrape takes, so
 * `MovieCache.recordCinemaScrape` cannot tell the two apart and prunes every film the
 * listing does not mention.
 *
 * The films that lose is the point. A title screening daily appears in whichever chunks
 * DID land, so it survives; a title screening on ONE date lives entirely inside a single
 * chunk, so a missing chunk erases it. Prod, 2026-07-27, UK — three unrelated Cineworld
 * venues pruning the same 19-22 films within the same minute, and the list is all
 * single-date advance-booking stock: `Metopera202627*`, `Rbocinemaseason202627*`,
 * `Ntlivethemisanthrope`, `Startrekivthevoyagehome40thanniversary`,
 * `Trainspotting (30th Anniversary)`.
 *
 * The existing breadth guard (`scrapeLooksPartial`) cannot catch this: it compares the
 * batch size against the cinema's known slots and only engages below half, while a
 * partial reduce typically returns most of the board. And the guard is guessing at
 * something the reduce handler already KNOWS — it computes the missing chunks and logs
 * them. This spec is about carrying that fact instead of discarding it.
 */
class PartialReducePruneSpec extends AnyFlatSpec with Matchers {
  import HandlerOutcome._

  private val cinema     = Multikino
  private val cinemaName = cinema.displayName
  private val now        = Instant.parse("2026-06-25T00:00:00Z")
  private val stale      = 15.minutes

  /** `day` doubles as the chunk key: one date per chunk, exactly like the real clients. */
  private def film(title: String, day: Int): CinemaMovie =
    CinemaMovie(Movie(title), cinema, None, Some(s"https://f/$title"), None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2026, 6, day, 18, 0), None)), Map.empty, None)

  private class FakeChunked(slices: Map[String, Seq[CinemaMovie]], failAlways: Set[String] = Set.empty)
    extends ChunkedCinemaScraper {
    val cinema: models.Cinema = PartialReducePruneSpec.this.cinema
    def scrapeHosts: Set[String] = Set("fake.pl")
    def planChunks(): Seq[String] = slices.keys.toSeq.sorted
    def fetchChunk(k: String): Seq[CinemaMovie] =
      if (failAlways.contains(k)) throw new RuntimeException(s"chunk $k down")
      else slices.getOrElse(k, Nil)
  }

  // The daily film is in every chunk; the advance-booking film sits alone in chunk "b".
  private val daily   = film("Daily Blockbuster", 25)
  private val advance = film("Met Opera 2026/27 Macbeth", 26)

  private class Harness(scraper: FakeChunked) {
    val queue     = new InMemoryTaskQueue
    val store     = new InMemoryChunkScrapeStore
    val freshness = new InMemoryFreshnessStore
    val cache     = new CaffeineMovieCache(new InMemoryMovieRepository(), normalizer = titleNormalizer)
    // The REAL publish path: runner → MovieCache.recordCinemaScrape, which is where the
    // prune lives. The existing ChunkScrapeFlowSpec stubs this out, which is exactly why
    // it never saw this.
    val runner    = new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set.empty)
    val publish: CinemaScraper => Unit = s => { runner.run(s); () }
    val map = Map(cinemaName -> (scraper: ChunkedCinemaScraper))
    val clk     = Clock.fixed(now, ZoneOffset.UTC)
    val policy  = new ScrapeFreshnessPolicy(freshness, clock = clk)
    val planner = new ChunkScrapePlanner(map, store, queue, publish, policy, stale, clk)
    val chunkH  = new ScrapeChunkHandler(map, store, clk)
    val reduceH = new ScrapeChunkReduceHandler(map, store, publish, policy, clk)
    val coord   = new ChunkScrapeCoordinator(store, queue)
    def reaper(c: Clock) = new ChunkScrapeReaper(store, queue, coord, staleAfter = stale, clock = c)

    def drain(at: Instant = now): Unit = {
      var next = queue.claim("w", 30.seconds, at)
      while (next.isDefined) {
        val task = next.get
        val handler = if (task.taskType == TaskType.ScrapeChunk) chunkH else reduceH
        handler.handle(task) match {
          case Done | Skipped =>
            queue.complete(task.id, "w")
            // Only ScrapeChunk drives the coordinator, exactly as the prod subscription does.
            if (task.taskType == TaskType.ScrapeChunk) coord.onTaskFinished(
              services.events.TaskFinished(task.taskType, task.dedupKey, task.payload))
          case _ => queue.complete(task.id, "w")
        }
        next = queue.claim("w", 30.seconds, at)
      }
    }
  }

  /** Every title this cinema currently holds a slot for, as the cache sees it. */
  private def slotTitles(cache: CaffeineMovieCache): Set[String] =
    cache.snapshot().flatMap(_.record.data.iterator.collect {
      case (s, sd) if models.Source.cinemaOf(s).contains(cinema) => sd.title
    }.flatten).toSet

  "a healthy chunked scrape" should "hold both the daily and the advance-booking film" in {
    val h = new Harness(new FakeChunked(Map("a" -> Seq(daily), "b" -> Seq(advance))))
    h.planner.plan(cinemaName)
    h.drain()
    slotTitles(h.cache) should contain allOf ("Daily Blockbuster", "Met Opera 2026/27 Macbeth")
  }

  // THE regression. Chunk "b" — the only chunk the advance-booking title appears in —
  // never lands, so the reduce publishes a listing containing just the daily film. That
  // listing is not evidence the advance title stopped screening; it is evidence that
  // nobody looked at its date.
  it should "not prune a film whose only chunk never landed" in {
    // First, a COMPLETE run, so the cinema legitimately holds both films.
    val healthy = new Harness(new FakeChunked(Map("a" -> Seq(daily), "b" -> Seq(advance))))
    healthy.planner.plan(cinemaName)
    healthy.drain()
    slotTitles(healthy.cache) should contain ("Met Opera 2026/27 Macbeth")

    // Now the same cinema re-scrapes and chunk "b" is dead. The reaper gives up and
    // partial-reduces: the published listing has only the daily film.
    val partial = new FakeChunked(Map("a" -> Seq(daily), "b" -> Seq(advance)), failAlways = Set("b"))
    val map     = Map(cinemaName -> (partial: ChunkedCinemaScraper))
    val clk     = Clock.fixed(now, ZoneOffset.UTC)
    val planner = new ChunkScrapePlanner(map, healthy.store, healthy.queue, healthy.publish, healthy.policy, stale, clk)
    val chunkH  = new ScrapeChunkHandler(map, healthy.store, clk)
    val reduceH = new ScrapeChunkReduceHandler(map, healthy.store, healthy.publish, healthy.policy, clk)
    planner.plan(cinemaName)
    // drain chunk 'a' (stores) and 'b' (fails); the run cannot complete on its own
    var next = healthy.queue.claim("w", 30.seconds, now)
    while (next.isDefined) {
      val t = next.get
      (if (t.taskType == TaskType.ScrapeChunk) chunkH else reduceH).handle(t)
      healthy.queue.complete(t.id, "w")
      next = healthy.queue.claim("w", 30.seconds, now)
    }
    val past = now.plusSeconds(16 * 60)
    healthy.reaper(Clock.fixed(past, ZoneOffset.UTC)).tick() shouldBe 1
    next = healthy.queue.claim("w", 30.seconds, past)
    while (next.isDefined) {
      val t = next.get
      (if (t.taskType == TaskType.ScrapeChunk) chunkH else reduceH).handle(t)
      healthy.queue.complete(t.id, "w")
      next = healthy.queue.claim("w", 30.seconds, past)
    }

    withClue(s"cinema now holds ${slotTitles(healthy.cache)}: ") {
      slotTitles(healthy.cache) should contain ("Met Opera 2026/27 Macbeth")
    }
  }

  // The other half of the contract, and the reason this is a completeness flag rather
  // than "stop pruning chunked cinemas": a COMPLETE run that no longer lists a film is
  // real evidence it stopped screening, and must still prune. Otherwise every chunked
  // venue accumulates films forever.
  it should "still prune a film a COMPLETE run no longer lists" in {
    val h = new Harness(new FakeChunked(Map("a" -> Seq(daily), "b" -> Seq(advance))))
    h.planner.plan(cinemaName)
    h.drain()
    slotTitles(h.cache) should contain ("Met Opera 2026/27 Macbeth")

    // Same cinema, every chunk lands, but the advance title is gone from the listing.
    val dropped = new FakeChunked(Map("a" -> Seq(daily), "b" -> Seq.empty))
    val map     = Map(cinemaName -> (dropped: ChunkedCinemaScraper))
    val clk     = Clock.fixed(now, ZoneOffset.UTC)
    val planner = new ChunkScrapePlanner(map, h.store, h.queue, h.publish, h.policy, stale, clk)
    val chunkH  = new ScrapeChunkHandler(map, h.store, clk)
    val reduceH = new ScrapeChunkReduceHandler(map, h.store, h.publish, h.policy, clk)
    planner.plan(cinemaName)
    var next = h.queue.claim("w", 30.seconds, now)
    while (next.isDefined) {
      val t = next.get
      (if (t.taskType == TaskType.ScrapeChunk) chunkH else reduceH).handle(t) match {
        case Done | Skipped =>
          h.queue.complete(t.id, "w")
          if (t.taskType == TaskType.ScrapeChunk) h.coord.onTaskFinished(
            services.events.TaskFinished(t.taskType, t.dedupKey, t.payload))
        case _ => h.queue.complete(t.id, "w")
      }
      next = h.queue.claim("w", 30.seconds, now)
    }

    withClue(s"cinema now holds ${slotTitles(h.cache)}: ") {
      slotTitles(h.cache) should not contain "Met Opera 2026/27 Macbeth"
      slotTitles(h.cache) should contain ("Daily Blockbuster")
    }
  }
}
