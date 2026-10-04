package services.identity

import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryScrapeGuardLedger, ListingIntakeMetrics, SingleCountryNormalizer}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeAttempt}
import tools.QueryPlans
import tools.costs.PerformanceBudgets

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/**
 * The projection's listing read, held to what it fetches from Mongo — every five minutes on every
 * cut-over worker, and once most of Mongo's outbound traffic (the US's 308 MB a read; 2.6 GB a tick of
 * worker allocation decoding it). A store's result says nothing about this: the in-memory archive and a
 * Mongo one that fetched every row and dropped the venues nobody keeps answer the same. So this reads
 * the wire ([[QueryPlans.traffic]]): the whole rows a read fetches must be exactly the venues it keeps,
 * and a read over archives nothing was written to since fetches none.
 */
class ListingReadBudgetIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val clock = Clock.fixed(Instant.parse("2026-09-26T10:00:00Z"), ZoneOffset.UTC)
  private val start = LocalDateTime.of(2026, 9, 27, 18, 0)

  private def listing(cinema: Cinema, n: Int): Seq[CinemaMovie] = (1 to 12).map { f =>
    CinemaMovie(Movie(s"Film $n-$f"), cinema, None, None, None, Nil, Nil, (0 until 30).map(h => Showtime(start.plusHours(h.toLong), None)))
  }

  /** The finds that fetch whole listing rows — by venue id, as against the id and stamp scans — and the rows
   *  they returned. */
  private def rowsFetched(traffic: QueryPlans.Traffic): Long =
    traffic.all.filter(e => e.command.getFirstKey == "find" && Option(e.command.getDocument("filter", null))
      .exists(f => f.containsKey("_id") && f.get("_id").isDocument && f.getDocument("_id").containsKey("$in"))).map(_.returned).sum

  "the projection's listing read" should "fetch exactly the venues it keeps, and nothing over unchanged archives" in {
    QueryPlans.traffic(mongoTarget, "listing-read-budget") { (db, traffic) =>
      val venues   = Cinema.all.distinct.take(60)
      val archive  = new MongoScrapeArchiveRepository(Some(db))
      val accepted = new MongoScrapeArchiveRepository(Some(db), IdentityListingIntake.Collection)
      def store(into: MongoScrapeArchiveRepository, cinema: Cinema, at: Instant, n: Int): Unit =
        into.record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), at, listingComplete = true, listing(cinema, n), error = None))
      venues.zipWithIndex.foreach { case (c, n) => store(archive, c, clock.instant(), n) }
      // A quarter of the live venues hold an accepted listing, and so do five venues no longer live.
      val live = venues.take(40)
      (live.take(10) ++ venues.drop(55)).foreach(c => store(accepted, c, clock.instant(), 100))
      val intake = new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, SingleCountryNormalizer.titleNormalizer,
        3, clock, ListingIntakeMetrics.noop)

      traffic.reset()
      val first = intake.projectedByVenue(live)
      first.map(_._1).toSet shouldBe live.toSet
      val wholeRead = rowsFetched(traffic)
      info(s"first read: $wholeRead whole rows for ${live.size} venues kept, ${traffic.all.size} reads")
      PerformanceBudgets.ListingReadRowsBeyondKept.check(wholeRead - live.size, s"$wholeRead rows fetched for ${live.size} venues kept")
      wholeRead shouldBe live.size.toLong

      traffic.reset()
      intake.projectedByVenue(live)
      val quiet = traffic.all
      info(s"quiet read: ${rowsFetched(traffic)} whole rows, ${quiet.size} reads returning ${quiet.map(_.returned).sum} documents")
      PerformanceBudgets.QuietListingReadRows.check(rowsFetched(traffic))
      val sent = quiet.map(e => s"${e.collection} ${e.command.getFirstKey} → ${e.returned}").mkString(", ")
      PerformanceBudgets.QuietListingReadCommands.check(quiet.size.toLong, sent)
      PerformanceBudgets.QuietListingReadDocuments.check(quiet.map(_.returned).sum, sent)

      // One venue re-scraped: it, and it alone, is fetched again.
      store(archive, live.last, clock.instant().plusSeconds(60), 7)
      traffic.reset()
      intake.projectedByVenue(live)
      rowsFetched(traffic) shouldBe 1L
    }
  }
}
