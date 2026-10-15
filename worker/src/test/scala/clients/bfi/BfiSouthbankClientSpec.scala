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
 *  trimmed to the film cards and the `section#shows` (day headings and performance blocks)
 *  the client reads.
 *
 *  BFI Southbank never had a showtime from Flicks, its only source. */
class BfiSouthbankClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today   = LocalDate.of(2026, 10, 12)
  private def scrape(on: LocalDate) = new BfiSouthbankClient(new FakeHttpFetch("bfi"), BfiLondonSouthbank, on).fetch()
  private val results = scrape(today)
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "BfiSouthbankClient.fetch" should "read the Southbank programme Flicks never listed off the BFI's cinemas site" in {
    results.size shouldBe 118
    results.flatMap(_.showtimes).size shouldBe 350
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

  it should "keep a sold-out performance, whose only date is its day heading" in {
    // Sold out, the block draws a disabled <button>18:00</button> under <h3>Wed 21 Oct</h3>:
    // no booking link, no aria-label. It is the film's only performance.
    byTitle("Ghosts: The Possession of Button House - Preview").showtimes shouldBe Seq(
      models.Showtime(LocalDateTime.of(2026, 10, 21, 18, 0), None, room = Some("NFT1")))
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

  it should "leave out the test listing the box office made to try its site" in {
    // "Z Test Show please ignore": one NFT4 performance, listed beside the films on 2026-10-12.
    byTitle.keySet.filter(_.toLowerCase.contains("test show")) shouldBe empty
  }

  it should "leave the BFI IMAX's performances to that venue" in {
    // Digger plays only at the IMAX in Waterloo on 2026-10-12.
    byTitle.keySet should not contain "Digger"
  }

  it should "drop performances before today" in {
    scrape(LocalDate.of(2026, 10, 24)).flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 10, 24)
  }

  // Each a film page's `section#shows` as cinemas.bfi.org.uk served it on 2026-10-16
  // (https://cinemas.bfi.org.uk/whats-on/<slug>), trimmed of its scripts and icons.
  private def shows(page: String) =
    scala.io.Source.fromFile(s"test/resources/fixtures/bfi/shows-sections/$page.html", "UTF-8").mkString

  "BfiSouthbankClient.performances" should "read the blocks of a page that carries no calendar" in {
    val dryWhite = BfiSouthbankClient.performances(shows("a-dry-white-season"), today)
    dryWhite.map(s => (s.dateTime, s.room)) shouldBe Seq(
      LocalDateTime.of(2026, 10, 23, 20, 40) -> Some("NFT3"),
      LocalDateTime.of(2026, 11, 2, 18, 20)  -> Some("NFT4"),
      LocalDateTime.of(2026, 11, 25, 18, 10) -> Some("NFT2"))
    dryWhite.flatMap(_.bookingUrl).foreach(_ should startWith ("https://whatson.bfi.org.uk/"))
    // sold out: a disabled button under its day heading, no booking link
    BfiSouthbankClient.performances(shows("sold-out-cunk-on-cinema"), today) shouldBe
      Seq(models.Showtime(LocalDateTime.of(2026, 11, 3, 18, 15), None, room = Some("NFT1")))
  }

  it should "leave out the talks the site sells in the BFI Reuben Library" in {
    // UEA Film50's talk on the James Bond franchise, 30 Nov, in the Reuben Library
    BfiSouthbankClient.performances(shows("reuben-library-talk-james-bond-franchise"), today) shouldBe empty
  }
}
