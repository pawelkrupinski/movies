package clients.bfi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.BfiLondonSouthbank
import services.cinemas.uk.BfiSouthbankClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a 2026-10-12 capture of cinemas.bfi.org.uk (`WriteBfiSouthbank`): its seven
 *  `whats-on/show-list-page.partial?page=N` pages and every film page they link, each
 *  trimmed to the film cards and performance blocks the client reads.
 *
 *  BFI Southbank never had a showtime from Flicks, its only source. */
class BfiSouthbankClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today   = LocalDate.of(2026, 10, 12)
  private def scrape(on: LocalDate) = new BfiSouthbankClient(new FakeHttpFetch("bfi"), BfiLondonSouthbank, on).fetch()
  private val results = scrape(today)
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "BfiSouthbankClient.fetch" should "read the Southbank programme Flicks never listed off the BFI's cinemas site" in {
    results.size shouldBe 106
    results.flatMap(_.showtimes).size shouldBe 325
    results.map(_.cinema).toSet shouldBe Set(BfiLondonSouthbank)
    results.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2026, 11, 30)
    results.flatMap(_.showtimes).flatMap(_.room).toSet shouldBe Set("NFT1", "NFT2", "NFT3", "NFT4")
  }

  it should "read each performance block's time, screen and booking link, placing its yearless day" in {
    val film = byTitle("A Dry White Season")
    film.showtimes.map(s => (s.dateTime, s.room.value)) shouldBe Seq(
      LocalDateTime.of(2026, 10, 23, 20, 40) -> "NFT3",
      LocalDateTime.of(2026, 11, 2, 18, 20)  -> "NFT4",
      LocalDateTime.of(2026, 11, 25, 18, 10) -> "NFT2")
    film.showtimes.head.bookingUrl.value should startWith ("https://whatson.bfi.org.uk/Online/login.asp?targetPage=mapSelect.asp")
  }

  it should "carry the card's runtime, year, certificate, synopsis and original image" in {
    val film = byTitle("A Dry White Season")
    film.movie.runtimeMinutes shouldBe Some(107)
    film.movie.releaseYear shouldBe Some(1989)
    film.ageRating shouldBe Some("15")
    film.synopsis.value should startWith ("An ordinary man’s conscience is ignited")
    film.posterUrl.value should startWith ("https://cms.cinemas.bfi.org.uk/storage/media/")
    film.filmUrl.value shouldBe "https://cinemas.bfi.org.uk/whats-on/a-dry-white-season"
  }

  it should "leave the BFI IMAX's performances to that venue" in {
    // Digger plays only at the IMAX in Waterloo on 2026-10-12.
    byTitle.keySet should not contain "Digger"
  }

  it should "drop performances before today" in {
    scrape(LocalDate.of(2026, 10, 24)).flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 10, 24)
  }

  "BfiSouthbankClient.performances" should "read the blocks of a page that carries no calendar" in {
    BfiSouthbankClient.performances(
      """<div class="performanceDetails" data-ga-vanue="BFI Southbank"><a href="https://whatson.bfi.org.uk/x" aria-label="14:00 Monday 16 November NFT1">14:00</a></div>""",
      today) shouldBe Seq(models.Showtime(LocalDateTime.of(2026, 11, 16, 14, 0), Some("https://whatson.bfi.org.uk/x"), room = Some("NFT1")))
  }

  it should "leave out the talks the site sells in the BFI Reuben Library" in {
    BfiSouthbankClient.performances(
      """<div class="performanceDetails" data-ga-vanue="BFI Southbank"><a href="https://whatson.bfi.org.uk/x" aria-label="18:30 Thursday 5 November BFI Reuben Library">18:30</a></div>""",
      today) shouldBe empty
  }
}
