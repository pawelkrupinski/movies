package services.identity

import models.{CinemaMovie, Movie, Multikino}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{CinemaScrapeRunner, CinemaScraper}
import services.movies.{DepthGuardTime, InMemoryScrapeGuardLedger}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.scrapes.InMemoryScrapeArchiveRepository

/** A cut-over venue's scrape reaches the intake through the production runner. The runner used to
 *  archive the scrape BEFORE the intake judged it, so a venue with no accepted listing of its own was
 *  judged against the very scrape being judged: the shrink below was published, and the first scrape
 *  never became the accepted listing. It now archives after the sink decides. */
class IdentityListingIntakeRunnerSpec extends AnyFlatSpec with Matchers {

  private def films(count: Int): Seq[CinemaMovie] =
    (1 to count).map(i => CinemaMovie(Movie(s"Film $i"), Multikino, None, None, None, Nil, Nil, DepthGuardTime.showtimes(4)))

  private final class Board(var listing: Seq[CinemaMovie]) extends CinemaScraper {
    val cinema                    = Multikino
    def scrapeHosts: Set[String]  = Set.empty
    def fetch(): Seq[CinemaMovie] = listing
  }

  "a scrape that loses most of a venue's showtimes" should "be held back by the depth guard" in {
    val archive = new InMemoryScrapeArchiveRepository
    val ledger  = new InMemoryScrapeGuardLedger
    val intake  = new IdentityListingIntake(new InMemoryScrapeArchiveRepository, archive, ledger, titleNormalizer, 3,
      DepthGuardTime.clock, services.movies.ListingIntakeMetrics.noop)
    val runner  = new CinemaScrapeRunner(intake, archive)
    val board   = new Board(films(10))
    runner.run(board)
    board.listing = films(1) // 40 upcoming showtimes → 4, under the guard's floor
    runner.run(board)
    intake.listingOf(Multikino).size shouldBe 10
  }

  "the same shrink landed straight into the intake, the archive still holding the venue's last scrape" should
    "be held back by the depth guard (the control)" in {
    val archive = new InMemoryScrapeArchiveRepository
    val metrics = new services.movies.RecordingListingIntakeMetrics
    val intake  = new IdentityListingIntake(new InMemoryScrapeArchiveRepository, archive, new InMemoryScrapeGuardLedger,
      titleNormalizer, 3, DepthGuardTime.clock, metrics)
    archive.record(services.scrapes.ScrapeAttempt(Multikino, None, DepthGuardTime.Now, listingComplete = true, films(10), error = None))
    intake.recordCinemaScrape(Multikino, films(1), listingIsComplete = true, sourceKey = None, viaFallback = false)
    intake.listingOf(Multikino).size shouldBe 10
    // Counted as the landing counted it, so a guard stuck rejecting stays alertable on the new path.
    metrics.verdicts shouldBe Vector("depth" -> "reject")
  }
}
