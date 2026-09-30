package services.tasks

import models.{CinemaMovie, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScrapeRunner
import services.events.InProcessEventBus
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
  private val cinema     = Multikino
  private val cinemaName = cinema.displayName
  private val now        = Instant.parse("2026-06-25T00:00:00Z")
  private val stale      = 15.minutes

  /** `day` doubles as the chunk key: one date per chunk, exactly like the real clients. */
  private def film(title: String, day: Int): CinemaMovie =
    CinemaMovie(Movie(title), cinema, None, Some(s"https://f/$title"), None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2026, 6, day, 18, 0), None)), Map.empty, None)

  // The daily film is in every chunk; the advance-booking film sits alone in chunk "b".
  private val daily   = film("Daily Blockbuster", 25)
  private val advance = film("Met Opera 2026/27 Macbeth", 26)

  private val clock = Clock.fixed(now, ZoneOffset.UTC)

  /** A cache, and the chunked stack over it publishing down the REAL path: runner →
   *  `MovieCache.recordCinemaScrape`, which is where the prune lives. `ChunkScrapeFlowSpec`
   *  stubs the publish out, which is exactly why it never saw this. */
  private def harness(scraper: FakeChunkedScraper): (CaffeineMovieCache, ChunkScrapeHarness) = {
    val cache  = new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer)
    val runner = new CinemaScrapeRunner(cache, new InProcessEventBus(), deferredCinemas = Set.empty)
    (cache, new ChunkScrapeHarness(scraper, s => { runner.run(s); () }, clock, staleAfter = stale))
  }

  /** Run every claimable task once and complete it, WITHOUT firing the coordinator —
   *  so a run whose chunk failed cannot complete on its own. */
  private def runEachOnce(h: ChunkScrapeHarness, at: Instant): Unit = {
    var next = h.queue.claim("w", 30.seconds, at)
    while (next.isDefined) {
      val t = next.get
      h.handlerFor(t).handle(t)
      h.queue.complete(t.id, "w")
      next = h.queue.claim("w", 30.seconds, at)
    }
  }

  /** Every title this cinema currently holds a slot for, as the cache sees it. */
  private def slotTitles(cache: CaffeineMovieCache): Set[String] =
    cache.snapshot().flatMap(_.record.data.iterator.collect {
      case (s, sd) if models.Source.cinemaOf(s).contains(cinema) => sd.title
    }.flatten).toSet

  "a healthy chunked scrape" should "hold both the daily and the advance-booking film" in {
    val (cache, h) = harness(new FakeChunkedScraper(Map("a" -> Seq(daily), "b" -> Seq(advance))))
    h.planner.plan(cinemaName)
    h.drain(now)
    slotTitles(cache) should contain allOf ("Daily Blockbuster", "Met Opera 2026/27 Macbeth")
  }

  // THE regression. Chunk "b" — the only chunk the advance-booking title appears in —
  // never lands, so the reduce publishes a listing containing just the daily film. That
  // listing is not evidence the advance title stopped screening; it is evidence that
  // nobody looked at its date.
  it should "not prune a film whose only chunk never landed" in {
    // First, a COMPLETE run, so the cinema legitimately holds both films.
    val (cache, healthy) = harness(new FakeChunkedScraper(Map("a" -> Seq(daily), "b" -> Seq(advance))))
    healthy.planner.plan(cinemaName)
    healthy.drain(now)
    slotTitles(cache) should contain ("Met Opera 2026/27 Macbeth")

    // Now the same cinema re-scrapes and chunk "b" is dead. The reaper gives up and
    // partial-reduces: the published listing has only the daily film.
    val partial = healthy.rescraping(new FakeChunkedScraper(Map("a" -> Seq(daily), "b" -> Seq(advance)), failAlways = Set("b")))
    partial.planner.plan(cinemaName)
    // drain chunk 'a' (stores) and 'b' (fails); the run cannot complete on its own
    runEachOnce(partial, now)
    val past = now.plusSeconds(16 * 60)
    partial.reaper(Clock.fixed(past, ZoneOffset.UTC)).tick() shouldBe 1
    runEachOnce(partial, past)

    withClue(s"cinema now holds ${slotTitles(cache)}: ") {
      slotTitles(cache) should contain ("Met Opera 2026/27 Macbeth")
    }
  }

  // The other half of the contract, and the reason this is a completeness flag rather
  // than "stop pruning chunked cinemas": a COMPLETE run that no longer lists a film is
  // real evidence it stopped screening, and must still prune. Otherwise every chunked
  // venue accumulates films forever.
  it should "still prune a film a COMPLETE run no longer lists" in {
    val (cache, h) = harness(new FakeChunkedScraper(Map("a" -> Seq(daily), "b" -> Seq(advance))))
    h.planner.plan(cinemaName)
    h.drain(now)
    slotTitles(cache) should contain ("Met Opera 2026/27 Macbeth")

    // Same cinema, every chunk lands, but the advance title is gone from the listing.
    val dropped = h.rescraping(new FakeChunkedScraper(Map("a" -> Seq(daily), "b" -> Seq.empty)))
    dropped.planner.plan(cinemaName)
    dropped.drain(now)

    withClue(s"cinema now holds ${slotTitles(cache)}: ") {
      slotTitles(cache) should not contain "Met Opera 2026/27 Macbeth"
      slotTitles(cache) should contain ("Daily Blockbuster")
    }
  }
}
