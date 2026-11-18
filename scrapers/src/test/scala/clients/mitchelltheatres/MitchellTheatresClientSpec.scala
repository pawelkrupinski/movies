package clients.mitchelltheatres

import clients.tools.{FakeHttpFetch, ScriptedByUrlHttpFetch}
import models.UsRoster
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.us.MitchellTheatresClient

import java.io.IOException
import java.time.{LocalDate, LocalDateTime, LocalTime}

/** Replays a 2026-11-17 (Kansas evening) capture of Southgate Cinema 6's own site
 *  (`WriteMitchellTheatres`): https://www.southgate6.com/index.
 *
 *  flicks.us's page for the house listed Sequoyah Cinema 9's (Garden City) sessions that week,
 *  every one booked on tickets.sequoyah9.com (DuplicateVenueListing, us, 2026-11-17); the
 *  house's own site sells its own, on tickets.southgate6.com. */
class MitchellTheatresClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Site      = "https://www.southgate6.com"
  private val Southgate = UsRoster.byDisplayName("Southgate Cinema 6 Liberal")
  private val today     = LocalDate.of(2026, 11, 17)

  private def scrape(on: LocalDate = today) =
    new MitchellTheatresClient(new FakeHttpFetch("mitchelltheatres"), Site, Southgate, on).fetch()

  "MitchellTheatresClient.fetch" should "read every film the home page sells, each showing booked on the house's own tickets host" in {
    val films = scrape()
    films.map(_.movie.title).sorted shouldBe Seq("Clayface", "Ebenezer", "Ghost Soldier", "Godzilla Minus Zero",
      "Other Mommy", "The Cat in the Hat", "The Hunger Games: Sunrise On The Reaping")
    films.map(_.cinema).toSet shouldBe Set(Southgate)
    val showtimes = films.flatMap(_.showtimes)
    showtimes.size shouldBe 75
    showtimes.flatMap(_.bookingUrl).map(new java.net.URI(_).getHost).toSet shouldBe Set("tickets.southgate6.com")
    showtimes.map(_.dateTime.toLocalDate).distinct.sorted shouldBe (17 to 24).map(LocalDate.of(2026, 11, _))
  }

  it should "read each day's times off its own row, on the house's wall clock" in {
    val ebenezer = scrape().find(_.movie.title == "Ebenezer").value
    ebenezer.showtimes.filter(_.dateTime.toLocalDate == LocalDate.of(2026, 11, 21)).map(_.dateTime.toLocalTime) shouldBe
      Seq(LocalTime.of(13, 25), LocalTime.of(16, 25), LocalTime.of(19, 25), LocalTime.of(22, 25))
    ebenezer.showtimes.head.dateTime shouldBe LocalDateTime.of(2026, 11, 17, 19, 5)
    ebenezer.showtimes.head.bookingUrl.value shouldBe "https://tickets.southgate6.com/checkout/showing/4327160/"
  }

  it should "carry the poster, synopsis, trailer, rating and runtime off the film's block" in {
    val ebenezer = scrape().find(_.movie.title == "Ebenezer").value
    ebenezer.posterUrl.value should startWith ("https://indy-systems.imgix.net/")
    ebenezer.synopsis.value should startWith ("An elderly miser embarks on a supernatural journey")
    ebenezer.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=A7lyvgnDuDE"
    ebenezer.ageRating.value shouldBe "PG-13"
    ebenezer.movie.runtimeMinutes.value shouldBe 115
  }

  it should "drop the days before today, and a film with none left" in {
    val later = scrape(on = LocalDate.of(2026, 11, 20))
    later.flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 11, 20)
    later.map(_.movie.title) should not contain allOf ("Clayface", "Other Mommy")
  }

  it should "fail the scrape when the home page cannot be read" in {
    val fetch = new ScriptedByUrlHttpFetch(_ => throw new IOException("site refused"))
    an[IOException] should be thrownBy new MitchellTheatresClient(fetch, Site, Southgate, today).fetch()
  }

  "MitchellTheatresClient.timeOf" should "read the site's 12-hour clock" in {
    MitchellTheatresClient.timeOf("7:05pm").value shouldBe LocalTime.of(19, 5)
    MitchellTheatresClient.timeOf("12:20am").value shouldBe LocalTime.of(0, 20)
    MitchellTheatresClient.timeOf("12:30pm").value shouldBe LocalTime.of(12, 30)
    MitchellTheatresClient.timeOf("Sold out") shouldBe None
  }
}
