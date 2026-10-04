package services.identity

import models.{Cinema, CinemaMovie, Helios, KinoApollo, KinoMuza, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryScrapeGuardLedger, SingleCountryNormalizer}
import services.scrapes.{ArchivedScrape, ForwardingScrapeArchive, InMemoryScrapeArchiveRepository, LeanListing, ScrapeAttempt}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/** The projection's listing read (`IdentityListingIntake.projected`) holds what it read and reads again only the
 *  venues whose listing moved — read whole, the two archives were most of Mongo's outbound traffic (the US's 308 MB
 *  every five minutes) — and always gives exactly what a whole read gives, a write filed at the same instant as the
 *  last included: the intake files an accepted listing at the wiring's clock, which a harness pins. */
class IdentityListingIntakeProjectedSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val clock      = Clock.fixed(Instant.parse("2026-09-26T10:00:00Z"), ZoneOffset.UTC)
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)
  private val live       = Seq(Multikino, Helios, KinoApollo, KinoMuza)

  private def film(cinema: Cinema, title: String, hours: Int*): CinemaMovie =
    CinemaMovie(Movie(title), cinema, None, None, None, Nil, Nil, hours.map(h => Showtime(start.plusHours(h.toLong), None)))

  /** An in-memory archive that counts the rows its keyed reads hand over, with their showtimes and without. */
  private final class Counting extends ForwardingScrapeArchive(new InMemoryScrapeArchiveRepository) {
    var wholeRowsRead, leanRowsRead = 0
    def rowsRead: Int = wholeRowsRead + leanRowsRead
    override def scanVenues(keep: Cinema => Boolean)(consume: Seq[ArchivedScrape] => Unit): tools.ScanOutcome =
      super.scanVenues(keep) { rows => wholeRowsRead += rows.size; consume(rows) }
    override def scanLean(keep: Cinema => Boolean)(consume: Seq[LeanListing] => Unit): tools.ScanOutcome =
      super.scanLean(keep) { rows => leanRowsRead += rows.size; consume(rows) }
    def store(cinema: Cinema, at: Instant, films: CinemaMovie*): Unit =
      record(ScrapeAttempt(cinema, Cinema.cityOf(cinema), at, listingComplete = true, films, error = None))
  }

  private final class World {
    val accepted = new Counting
    val archive  = new Counting
    val intake   = new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, normalizer, 3, clock,
      services.movies.ScrapeLandingMetrics.noop)
    def reads: Int = accepted.rowsRead + archive.rowsRead
    /** `projected(venues)`, and the rows it read. */
    def project(venues: Seq[Cinema] = live): (Seq[ProjectedListing], Int) = { val before = reads; val p = intake.projected(venues); (p, reads - before) }
    /** What a whole read gives, through a reader that holds nothing. */
    def whole: Seq[ProjectedListing] =
      intake.listings(live).flatMap { case (c, films) => films.map(cm => ProjectedListing.of(Listing.of(c, cm, normalizer), cm)) }
  }

  "the projection's listing read" should "read again only the venues whose listing moved, and give what a whole read gives" in {
    val w = new World
    w.archive.store(Multikino, clock.instant(), film(Multikino, "Lalka", 0, 1))
    w.archive.store(Helios, clock.instant(), film(Helios, "Diuna", 2))
    w.archive.store(KinoApollo, clock.instant(), film(KinoApollo, "Obcy", 3))
    w.project() shouldBe ((w.whole, 3))                        // the first read is whole
    w.project() shouldBe ((w.whole, 0))                        // nothing moved: no row read
    w.archive.store(Helios, clock.instant().plusSeconds(60), film(Helios, "Diuna", 2, 4))
    w.project() shouldBe ((w.whole, 1))                        // Helios, and Helios alone
  }

  it should "see an accepted listing filed at the same instant as the one it holds" in {
    val w = new World
    w.archive.store(Multikino, clock.instant(), film(Multikino, "Lalka", 0, 1))
    w.intake.recordCinemaScrape(Multikino, Seq(film(Multikino, "Lalka", 0, 1)), listingIsComplete = true, sourceKey = None, viaFallback = false)
    w.intake.projected(live) shouldBe w.whole
    // The same clock instant, another listing: the stamp cannot tell, the write can.
    w.intake.recordCinemaScrape(Multikino, Seq(film(Multikino, "Lalka", 0, 1), film(Multikino, "Diuna", 5)), listingIsComplete = true,
      sourceKey = None, viaFallback = false)
    w.intake.projected(live).map(_.listing.title).toSet shouldBe Set("Lalka", "Diuna")
    w.intake.projected(live) shouldBe w.whole
  }

  it should "see a venue's archived scrape filed at the same instant as the one it holds, once the intake has taken it" in {
    val w = new World
    w.archive.store(Helios, clock.instant(), film(Helios, "Diuna", 2))
    w.project()
    // Archived first at the pinned instant, then handed to the intake, which keeps the archive's as the venue's own.
    w.archive.store(Helios, clock.instant(), film(Helios, "Diuna", 2, 4))
    w.intake.recordCinemaScrape(Helios, Seq(film(Helios, "Diuna", 2, 4)), listingIsComplete = true, sourceKey = None, viaFallback = false)
    w.project()._1 shouldBe w.whole
  }

  it should "drop a venue no longer live, and read whole every so often whatever the stamps say" in {
    val w = new World
    w.archive.store(Multikino, clock.instant(), film(Multikino, "Lalka", 0))
    w.archive.store(KinoMuza, clock.instant(), film(KinoMuza, "Obcy", 1))
    w.project()._2 shouldBe 2                                  // call 1: whole
    w.project(Seq(Multikino))._1.map(_.listing.cinema).toSet shouldBe Set(Multikino)
    w.project()._2 shouldBe 1                                  // KinoMuza back: read again
    (4 to IdentityListingIntake.WholeReadEvery).foreach(_ => w.project()._2 shouldBe 0)
    w.project() shouldBe ((w.whole, 2))                        // the thirteenth call: whole again
  }

  // Neither the identity model's take-up nor the projection keeps a showtime, yet a US boot decoded every one in the
  // archive for them: ~11 CPU-s of `ShowtimeCodec.read` at take-up (JFR). The projection wants only their digest.
  "the identity model's take-up and the projection's listing read" should "read the archives without their showtimes" in {
    val w = new World
    w.archive.store(Multikino, clock.instant(), film(Multikino, "Lalka", 0, 1))
    w.archive.store(Helios, clock.instant(), film(Helios, "Diuna", 2))
    w.intake.recordCinemaScrape(Helios, Seq(film(Helios, "Diuna", 2, 3)), listingIsComplete = true, sourceKey = None, viaFallback = false)
    val taken = w.intake.identities(live)
    taken.flatMap(_._2).flatMap(_.showtimes) shouldBe empty
    taken.map { case (c, fs) => c -> fs.map(_.movie.title) } shouldBe w.intake.listings(live).map { case (c, fs) => c -> fs.map(_.movie.title) }
    def wholeRead[A](body: => A): (A, Int) = {
      val before = w.accepted.wholeRowsRead + w.archive.wholeRowsRead
      val value  = body
      (value, w.accepted.wholeRowsRead + w.archive.wholeRowsRead - before)
    }
    wholeRead(w.intake.identities(live))._2 shouldBe 0
    val expected = w.whole
    wholeRead(w.project()._1) shouldBe ((expected, 0))
    w.archive.store(Helios, clock.instant().plusSeconds(60), film(Helios, "Diuna", 2, 4))
    w.intake.recordCinemaScrape(Helios, Seq(film(Helios, "Diuna", 2, 4)), listingIsComplete = true, sourceKey = None, viaFallback = false)
    val moved = w.whole
    moved should not be expected
    wholeRead(w.project()._1) shouldBe ((moved, 0))             // a showtime moved: its digest tells
  }

  it should "project a listing the identity model holds the same as the model's object, not a copy of it" in {
    // A venue read again is projected from new rows. Each projected as a listing of its own, the projection held a second
    // copy of every listing the model holds (worker-us: 100k listings, their keys and catalogue ids).
    val w = new World
    w.archive.store(Multikino, clock.instant(), film(Multikino, "Lalka", 0, 1), film(Multikino, "Obcy", 2))
    val modelled = w.whole.map(_.listing).filter(_.title == "Lalka")
    w.intake.adopt(modelled)
    w.archive.store(Multikino, clock.instant().plusSeconds(60), film(Multikino, "Lalka", 0, 1, 3), film(Multikino, "Obcy", 2))
    val (read, _) = w.project()
    read shouldBe w.whole
    read.find(_.listing.title == "Lalka").get.listing should be theSameInstanceAs modelled.head
    read.find(_.listing.title == "Obcy").get.listing should not be theSameInstanceAs (w.whole.find(_.listing.title == "Obcy").get.listing)
  }
}
