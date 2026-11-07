package clients.cinemaplus

import clients.tools.{FakeHttpFetch, ScriptedByUrlHttpFetch}
import models.UsRoster
import org.scalatest.{LoneElement, OptionValues}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.ListingReads
import services.cinemas.us.CinemaPlusClient

import java.io.IOException
import java.time.{LocalDate, LocalDateTime, LocalTime}

/** Replays a 2026-10-12 capture of Barry Cinemas' CinemaPlus site (`WriteCinemaPlus`):
 *  https://barrycinemas.com/acme-theater-riverton and the Gem Theatre's page beside it,
 *  with the `?date=` page of every other day the Gem's calendar advertised.
 *
 *  The Acme (Riverton, WY) was retired as closed on 2026-10-12 because its flicks.us page
 *  had 404'd since 2026-08-31 — while barrycinemas.com sold its Tuesday screening. */
class CinemaPlusClientSpec extends AnyFlatSpec with Matchers with OptionValues with LoneElement {

  private val Site  = "https://barrycinemas.com"
  private val Acme  = UsRoster.byDisplayName("Acme Theatre Riverton")
  private val Gem   = UsRoster.byDisplayName("Gem Theatre Riverton")
  private val today = LocalDate.of(2026, 10, 12)

  private def scrape(path: String, cinema: models.Cinema, on: LocalDate = today) =
    new CinemaPlusClient(new FakeHttpFetch("cinemaplus"), Site, path, cinema, on).fetch()

  "CinemaPlusClient.fetch" should "read the Acme's one advertised screening, a day after the page's own date" in {
    val acme = scrape("/acme-theater-riverton", Acme)
    acme.map(_.movie.title) shouldBe Seq("Angel and the Badman")
    acme.map(_.cinema).toSet shouldBe Set(Acme)
    val show = acme.head.showtimes.loneElement
    show.dateTime shouldBe LocalDateTime.of(2026, 10, 13, 18, 0)
    show.bookingUrl.value shouldBe s"$Site/acme-theater-riverton/tickets/decb2faf-c0c1-f111-a6a9-002248ba6662"
  }

  it should "read every day the calendar advertises, each off its own ?date= page" in {
    val gem = scrape("/gem-theater-riverton", Gem)
    gem.map(_.movie.title) shouldBe Seq("Digger", "Heart of the Beast", "Other Mommy", "Verity")
    gem.flatMap(_.showtimes).size shouldBe 16
    gem.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted shouldBe
      (12 to 15).map(LocalDate.of(2026, 10, _))
    gem.find(_.movie.title == "Other Mommy").value.showtimes.map(_.dateTime.toLocalTime).distinct shouldBe
      Seq(LocalTime.of(18, 45))
  }

  it should "carry the film page, poster, synopsis, credits and rating off the card" in {
    val badman = scrape("/acme-theater-riverton", Acme).head
    badman.filmUrl.value shouldBe s"$Site/acme-theater-riverton/movie/27321"
    badman.posterUrl.value should startWith ("https://images.cinemaplus.com/ticketing/posters/")
    badman.synopsis.value should startWith ("When feared outlaw Quirt Evans")
    badman.synopsis.value should not include "&#039;"
    badman.cast shouldBe Seq("Zachary Levi", "Neal McDonough", "Tommy Lee Jones")
    badman.director shouldBe Seq("Julio Quintana")
    badman.ageRating.value shouldBe "PG-13"
  }

  it should "not read a seating class as a presentation format" in {
    val all = (scrape("/acme-theater-riverton", Acme) ++ scrape("/gem-theater-riverton", Gem)).flatMap(_.showtimes)
    all.flatMap(_.format) shouldBe empty
  }

  it should "skip the advertised days before today" in {
    val later = scrape("/gem-theater-riverton", Gem, on = LocalDate.of(2026, 10, 14))
    later.flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 10, 14)
    later.flatMap(_.showtimes).size shouldBe 8
  }

  it should "keep the other days when one day's page fails, and mark the listing incomplete" in {
    val replay = new FakeHttpFetch("cinemaplus")
    val fetch = new ScriptedByUrlHttpFetch(url =>
        if (url.endsWith("?date=2026-10-13")) throw new IOException("day refused") else replay.get(url))
    val (gem, reads) = ListingReads.during(new CinemaPlusClient(fetch, Site, "/gem-theater-riverton", Gem, today).fetch())
    gem.flatMap(_.showtimes).map(_.dateTime.toLocalDate).distinct.sorted shouldBe
      Seq(12, 14, 15).map(LocalDate.of(2026, 10, _))
    reads.complete shouldBe false
  }

  it should "fail the scrape when the venue page itself cannot be read" in {
    val fetch = new ScriptedByUrlHttpFetch(_ => throw new IOException("venue refused"))
    an[IOException] should be thrownBy new CinemaPlusClient(fetch, Site, "/gem-theater-riverton", Gem, today).fetch()
  }

  "CinemaPlusClient.timeOf" should "read the platform's 12-hour clock" in {
    CinemaPlusClient.timeOf("6:00p").value shouldBe LocalTime.of(18, 0)
    CinemaPlusClient.timeOf("12:15p").value shouldBe LocalTime.of(12, 15)
    CinemaPlusClient.timeOf("12:30a").value shouldBe LocalTime.of(0, 30)
    CinemaPlusClient.timeOf("11:05a").value shouldBe LocalTime.of(11, 5)
    CinemaPlusClient.timeOf("Sold out") shouldBe None
  }
}
