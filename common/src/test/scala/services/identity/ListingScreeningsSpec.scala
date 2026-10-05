package services.identity

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.{LocalDate, LocalDateTime}

/** The days a listing screens on, and the season a stage work billed with neither its season nor a year is broadcast in:
 *  a relay airs on its house's published dates, so the days name the production its title does not. */
class ListingScreeningsSpec extends AnyFlatSpec with Matchers {

  private def listing(title: String, at: String*): Listing =
    Listing.of(KinoApollo, CinemaMovie(Movie(title), KinoApollo, None, None, None, Nil, Nil,
      at.map(t => Showtime(LocalDateTime.parse(t), None))), SingleCountryNormalizer.titleNormalizer)

  "A listing" should "carry the days it screens on, each once" in {
    listing("Samson i Dalila", "2026-12-26T18:00", "2026-12-05T18:55", "2026-12-05T21:00").screenings.days shouldBe
      Seq(LocalDate.of(2026, 12, 5), LocalDate.of(2026, 12, 26))
  }

  "A stage work billed with neither its season nor a year" should "be broadcast in the season its first screening falls in" in {
    // PL Kino Amok's "Samson i Dalila" on the Met's 2026/27 broadcast date; Kino Powiśle's "OPERA-MAKBET - retransmisja"
    listing("Samson i Dalila", "2026-12-05T18:55").broadcastSeason shouldBe Some(2026)
    listing("Dziewczyna z Dzikiego Zachodu", "2027-01-23T18:55").broadcastSeason shouldBe Some(2026)
    listing("OPERA-MAKBET - retransmisja", "2026-11-21T18:00").broadcastSeason shouldBe Some(2026)
    listing("Manon", "2026-07-10T18:00").broadcastSeason shouldBe Some(2026)
    // a title naming its season, or no stage work, or nothing screening, is broadcast in none
    listing("Met Opera 2026-27: Manon", "2027-04-03T18:00").broadcastSeason shouldBe None
    listing("Lalka", "2026-12-05T18:00").broadcastSeason shouldBe None
    listing("Samson i Dalila").broadcastSeason shouldBe None
    // nor one stating its running time, or crediting its director, either of which already names the staging
    Listing.of(KinoApollo, CinemaMovie(Movie("Royal Opera House: Otello", runtimeMinutes = Some(190)), KinoApollo, None, None, None, Nil, Nil,
      Seq(Showtime(LocalDateTime.parse("2027-04-24T19:00"), None))), SingleCountryNormalizer.titleNormalizer).broadcastSeason shouldBe None
    Listing.of(KinoApollo, CinemaMovie(Movie("Royal Shakespeare Company: Macbeth"), KinoApollo, None, None, None, Nil, Seq("Polly Findlay"),
      Seq(Showtime(LocalDateTime.parse("2026-10-20T19:00"), None))), SingleCountryNormalizer.titleNormalizer).broadcastSeason shouldBe None
  }

  "A listing's equality" should "hold its broadcast season, never the days it screens on" in {
    // a venue adding a showtime re-resolves nothing; a relay moving to the next season does
    listing("Samson i Dalila", "2026-12-05T18:55") shouldBe listing("Samson i Dalila", "2026-12-05T18:55", "2026-12-09T18:00")
    listing("Lalka", "2026-12-05T18:00") shouldBe listing("Lalka", "2027-12-05T18:00")
    listing("Samson i Dalila", "2026-12-05T18:55") should not be listing("Samson i Dalila", "2027-12-05T18:55")
  }
}
