package services.identity

import models.{Cinema, CinemaMovie, Helios, KinoApollo, KinoMuza, Movie, Multikino, Rialto, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer
import services.scrapes.{ArchivedScrape, SuccessfulScrape}

import java.time.{Instant, LocalDateTime}

/** The shadow run's listing set is read from the archive a page at a time — never the whole
 *  archive's parsed rows at once, which on the US corpus alone is ~580 MB of live heap — and is
 *  exactly the listing set the whole archive would have given. */
class ArchiveListingsSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val at         = Instant.parse("2026-09-26T10:00:00Z")
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)

  private def film(cinema: Cinema, title: String, year: Option[Int], url: Option[String] = None): CinemaMovie =
    CinemaMovie(Movie(title, releaseYear = year), cinema, None, url, None, Nil, Nil, Seq(Showtime(start, None)))

  private def row(cinema: Cinema, films: CinemaMovie*): ArchivedScrape =
    ArchivedScrape(cinema, Cinema.cityOf(cinema), Some(SuccessfulScrape(at, listingComplete = true, films)), None)

  private val rows = Seq(
    row(Multikino, film(Multikino, "Lalka", Some(2026)), film(Multikino, "Obcy", Some(1979))),
    // Two spellings under one key: the smaller by the total order is the one kept.
    row(Helios, film(Helios, "Lalka", Some(2026), Some("/lalka")), film(Helios, "Lalka", Some(1968), Some("/lalka")),
      film(Helios, "Diuna", Some(2021))),
    row(KinoApollo, film(KinoApollo, "Lalka", Some(2026))),
    row(Rialto, film(Rialto, "Obcy", Some(1979))),
    row(KinoMuza, film(KinoMuza, "Diuna", Some(2021))))

  private def paged(pageSize: Int, completes: Boolean = true) = new PagedScrapeArchive(rows, pageSize, completes)

  private val live: Cinema => Boolean = _ != Rialto

  "the shadow's listing set" should "be read a page at a time, never the whole archive at once" in {
    val archive = paged(pageSize = 2)
    ArchiveListings.read(archive, live, normalizer) should not be empty
    archive.pagesServed shouldBe 3
  }

  it should "be exactly the listing set of the whole archive's live venues" in {
    val whole = Listing.corpus(rows.filter(r => live(r.cinema)).map(r => r.cinema -> r.films), normalizer)
    ArchiveListings.read(paged(pageSize = 2), live, normalizer) shouldBe whole
    ArchiveListings.read(paged(pageSize = 1), live, normalizer) shouldBe whole
    whole.map(_.venue).toSet should not contain Rialto.displayName
    whole.count(_.venue == Helios.displayName) shouldBe 2
  }

  it should "be empty on an archive read that could not be completed, not the pages it got" in {
    ArchiveListings.read(paged(pageSize = 2, completes = false), live, normalizer) shouldBe empty
  }
}
