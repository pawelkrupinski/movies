package services.identity

import models.{Cinema, CinemaMovie, Helios, KinoApollo, KinoMuza, Movie, Multikino, Rialto, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryScrapeGuardLedger, SingleCountryNormalizer}
import services.scrapes.{ArchivedScrape, ScrapeArchiveRepository, SuccessfulScrape}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}

/** The identity intake's listing set reads both archives a page at a time — never
 *  either whole, which on the US corpus is hundreds of megabytes held at once — and
 *  gives exactly the listings the whole-archive read gave: the accepted listing first, else the
 *  archive's, live venues only, venues publishing nothing left out, a failed read not taken as data. */
class IdentityListingIntakeListingsSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val clock      = Clock.fixed(Instant.parse("2026-09-26T10:00:00Z"), ZoneOffset.UTC)
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)

  private def film(cinema: Cinema, title: String): CinemaMovie =
    CinemaMovie(Movie(title), cinema, None, None, None, Nil, Nil, Seq(Showtime(start, None)))

  private def row(cinema: Cinema, titles: String*): ArchivedScrape =
    ArchivedScrape(cinema, Cinema.cityOf(cinema), Some(SuccessfulScrape(clock.instant(), listingComplete = true, titles.map(film(cinema, _)))), None)

  private val barren = ArchivedScrape(KinoMuza, Cinema.cityOf(KinoMuza), None, None)

  // Multikino: accepted wins over the archive. Helios: archive only. KinoApollo: accepted but empty,
  // so left out even though the archive lists a film. Rialto: not live. KinoMuza: never succeeded.
  private val acceptedRows = Seq(row(Multikino, "Lalka"), row(KinoApollo), row(Rialto, "Obcy"))
  private val archiveRows  = Seq(row(Multikino, "Stara Lalka"), row(Helios, "Diuna", "Obcy"), row(KinoApollo, "Lalka"),
    row(Rialto, "Obcy"), barren)
  private val live         = Seq(Multikino, Helios, KinoApollo, KinoMuza, Helios)

  private def intake(accepted: ScrapeArchiveRepository, archive: ScrapeArchiveRepository) =
    new IdentityListingIntake(accepted, archive, new InMemoryScrapeGuardLedger, normalizer, 3, clock,
      services.movies.ListingIntakeMetrics.noop)

  /** The listing set as the whole-archive read computed it, before the reader streamed. */
  private def wholeArchiveListings(accepted: Seq[ArchivedScrape], archive: Seq[ArchivedScrape]): Seq[(Cinema, Seq[CinemaMovie])] = {
    val acceptedByVenue = accepted.flatMap(a => a.lastSuccess.map(a.cinema -> _.films)).toMap
    val archivedByVenue = archive.flatMap(a => a.lastSuccess.map(a.cinema -> _.films)).toMap
    live.distinct.sortBy(_.displayName).flatMap(c => acceptedByVenue.get(c).orElse(archivedByVenue.get(c)).map(c -> _)).filter(_._2.nonEmpty)
  }

  "the cutover's listing set" should "be read from both archives a page at a time, never either whole" in {
    val accepted = new PagedScrapeArchive(acceptedRows, pageSize = 2)
    val archive  = new PagedScrapeArchive(archiveRows, pageSize = 2)
    intake(accepted, archive).listings(live) should not be empty
    accepted.pagesServed shouldBe 1
    archive.pagesServed shouldBe 1
  }

  // Every venue of a cut-over country soon has an accepted listing, and the archive's copy of each was
  // read whole and thrown away: 2.6 GB of decode every five minutes on the US corpus instead of 1.3.
  it should "never read the archive row of a venue with an accepted listing, nor either archive's rows of a venue not live" in {
    val accepted = new PagedScrapeArchive(acceptedRows, pageSize = 2)
    val archive  = new PagedScrapeArchive(archiveRows, pageSize = 2)
    intake(accepted, archive).listings(live)
    accepted.venuesServed should contain theSameElementsAs Seq(Multikino, KinoApollo)
    archive.venuesServed should contain theSameElementsAs Seq(Helios, KinoMuza)
  }

  it should "be exactly the listing set the whole-archive read gave" in {
    val expected = wholeArchiveListings(acceptedRows, archiveRows)
    expected.map(_._1) shouldBe Seq(Helios, Multikino)
    expected.toMap.apply(Multikino).map(_.movie.title) shouldBe Seq("Lalka")
    for (pageSize <- 1 to 5)
      intake(new PagedScrapeArchive(acceptedRows, pageSize), new PagedScrapeArchive(archiveRows, pageSize)).listings(live) shouldBe expected
  }

  it should "take an archive it could not read whole as empty, as the whole-archive read did, not as the pages it got" in {
    intake(new PagedScrapeArchive(acceptedRows, 1, completes = false), new PagedScrapeArchive(archiveRows, 1)).listings(live) shouldBe
      wholeArchiveListings(Nil, archiveRows)
    intake(new PagedScrapeArchive(acceptedRows, 1), new PagedScrapeArchive(archiveRows, 1, completes = false)).listings(live) shouldBe
      wholeArchiveListings(acceptedRows, Nil)
  }

  // A projection holds every venue's listing at once while it resolves and writes; a film a feed lists at
  // a hundred venues came back as a hundred copies of its title, cast, synopsis and every instant: the
  // US listing set held ~490 MB live, ~300 MB sharing them.
  it should "hold the values its venues repeat once, and be equal to the listings it read" in {
    def shown(cinema: Cinema) = CinemaMovie(Movie("Diuna", Some(155)), cinema, Some("https://p/diuna.jpg"), None, Some("Pustynia."),
      List("Timothée Chalamet", "Zendaya"), List("Denis Villeneuve"),
      Seq(Showtime(LocalDateTime.of(2026, 9, 27, 18, 0), None, Some("Sala 1"), List("2D", "NAP"))), Map("flicks" -> "1"), None, Some("12"))
    val venues   = Seq(Multikino, Helios)
    val rows     = venues.map(c => ArchivedScrape(c, Cinema.cityOf(c), Some(SuccessfulScrape(clock.instant(), listingComplete = true, Seq(shown(c)))), None))
    val read     = intake(new PagedScrapeArchive(rows.take(1), 1), new PagedScrapeArchive(rows.drop(1), 1)).listings(venues)
    read shouldBe venues.sortBy(_.displayName).map(c => c -> Seq(shown(c)))
    val Seq(a, b) = read.map(_._2.head)
    def same(of: CinemaMovie => AnyRef) = withClue(of(a))(of(a) should be theSameInstanceAs of(b))
    same(_.movie); same(_.posterUrl); same(_.synopsis); same(_.cast); same(_.director); same(_.externalIds); same(_.ageRating)
    same(_.showtimes.head.dateTime); same(_.showtimes.head.room); same(_.showtimes.head.format)
  }
}
