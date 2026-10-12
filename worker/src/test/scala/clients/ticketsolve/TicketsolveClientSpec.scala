package clients.ticketsolve

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.{NerveCentreLondonderry, TheatrMwldanCardigan}
import services.cinemas.uk.TicketsolveClient

import java.time.{LocalDate, LocalDateTime}

/** Replays 2026-10-12 captures of two Ticketsolve feeds (`WriteTicketsolve`):
 *  `mwldan.ticketsolve.com/shows.xml` and `nervecentre.ticketsolve.com/shows.xml`.
 *
 *  Theatr Mwldan's three screens and the Nerve Centre's cinema had no showtime from
 *  Flicks, their only source; both box offices publish the whole programme as XML. */
class TicketsolveClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today = LocalDate.of(2026, 10, 12)
  private def scrape(client: String, cinema: models.Cinema, on: LocalDate = today) =
    new TicketsolveClient(new FakeHttpFetch("ticketsolve"), client, cinema, on).fetch()

  private val mwldan  = scrape("mwldan", TheatrMwldanCardigan)
  private val byTitle = mwldan.map(cm => cm.movie.title -> cm).toMap

  "TicketsolveClient.fetch" should "read Theatr Mwldan's whole cinema programme off the feed" in {
    mwldan.size shouldBe 21
    mwldan.flatMap(_.showtimes).size shouldBe 180
    mwldan.map(_.cinema).toSet shouldBe Set(TheatrMwldanCardigan)
    mwldan.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2027, 3, 14)
  }

  it should "read each screening's local time, screen and booking link, once though the feed repeats it per venue" in {
    val film = byTitle("Forgotten Island")
    film.showtimes.take(3).map(s => (s.dateTime, s.room.value)) shouldBe Seq(
      LocalDateTime.of(2026, 10, 16, 18, 45) -> "Mwldan 1",
      LocalDateTime.of(2026, 10, 17, 13, 30) -> "Mwldan 1",
      LocalDateTime.of(2026, 10, 17, 16, 15) -> "Mwldan 1")
    film.showtimes.head.bookingUrl.value shouldBe "https://mwldan.ticketsolve.com/shows/873688734/events/129693858/seats"
    film.showtimes.map(_.dateTime).distinct should have size film.showtimes.size.toLong
    film.externalIds shouldBe Map("ticketsolve" -> "873688734")
    film.posterUrl.value should startWith ("https://d235gwso45fsgz.cloudfront.net/as-assets/variants/jn91ef78ydozznx28h3xb3gse2qo/")
  }

  it should "lift the certificate the box office appends to the title into the age rating" in {
    val film = byTitle("Forgotten Island")
    film.ageRating shouldBe Some("PG")
    film.movie.rawTitle shouldBe Some("Forgotten Island (PG)")
    // "(12A tbc)" is still a 12A.
    byTitle("Avengers: Doomsday").ageRating shouldBe Some("12A")
  }

  it should "leave out a relay category that also sells live gigs" in {
    // "Live Broadcast Events" sells Royal Ballet relays beside Clwb Mwldan's club nights.
    mwldan.map(_.movie.title).filter(_.startsWith("Royal Ballet")) shouldBe empty
    mwldan.map(_.movie.title).filter(_.startsWith("Clwb Mwldan")) shouldBe empty
  }

  it should "drop screenings before today" in {
    val later = scrape("mwldan", TheatrMwldanCardigan, LocalDate.of(2026, 10, 17))
    later.flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 10, 17)
  }

  "A multi-arts Ticketsolve client" should "keep only what it files as film, not its gigs, comedy or film courses" in {
    val nerve = scrape("nervecentre", NerveCentreLondonderry)
    nerve.size shouldBe 14
    val titles = nerve.map(_.movie.title)
    titles should contain allOf ("Corpse Bride", "The Shining (Extended Cut)")
    titles should not contain "Bell X1 | Good Bones Tour 2026"
    // "BFI Film Academy" files online masterclasses, not screenings.
    titles.filter(_.contains("Masterclass")) shouldBe empty
    nerve.find(_.movie.title == "Corpse Bride").value.showtimes.map(s => (s.dateTime, s.room)) should contain (
      LocalDateTime.of(2026, 10, 30, 14, 0) -> Some("Cinema One"))
  }

  "TicketsolveClient.parse" should "refuse a page that is no feed rather than call it an empty programme" in {
    an[IllegalArgumentException] should be thrownBy
      TicketsolveClient.parse("<html><body>Maintenance</body></html>", TheatrMwldanCardigan, today)
  }
}
