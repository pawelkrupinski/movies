package clients.spektrix

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.QuadDerby
import services.cinemas.uk.QuadDerbyClient
import tools.GetOnlyHttpFetch

import java.io.IOException
import java.time.LocalDate

/** Replays 2026-10-18 captures of Derby QUAD: its Spektrix v3 events and instances
 *  (`system.spektrix.com/derbyquad/api/v3/events?instanceStart_from=2026-10-18`, `…/instances?startFrom=2026-10-18`),
 *  its own programme `www.derbyquad.co.uk/whats-on` and one film page, `/whats-on/obsession-pg`.
 *
 *  The box office files a film with no description, image, web link, year or director — "Obsession (PG)", 96 min, is
 *  all it says — so the listing matched nothing. The venue's page says Edward Dmytryk, UK, 1949, with a synopsis and a
 *  poster. */
class QuadDerbyClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val client  = new QuadDerbyClient(new FakeHttpFetch("quad-derby"), LocalDate.of(2026, 10, 18))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "QuadDerbyClient.fetch" should "read Derby QUAD's films off its Spektrix box office" in {
    results.map(_.cinema).toSet shouldBe Set(QuadDerby)
    byTitle("Obsession (PG)").movie.runtimeMinutes shouldBe Some(96)
    byTitle("Obsession (PG)").movie.countries shouldBe Seq("UK") // `attribute_FilmCountryOfOrigin`
    byTitle("Obsession (PG)").externalIds shouldBe Map("spektrix" -> "353802AJBRGGJSJJNKSHVVCDLGBHTTDTG")
  }

  it should "point each film at the page the venue's programme lists it under, by its title and certificate" in {
    byTitle("Obsession (PG)").filmUrl shouldBe Some("https://www.derbyquad.co.uk/whats-on/obsession-pg")
    // The page's slug is not the title's: "night-of-the-demon", not "night-of-the-demon-pg".
    byTitle("Night of the Demon (PG)").filmUrl shouldBe Some("https://www.derbyquad.co.uk/whats-on/night-of-the-demon")
  }

  it should "still list the box office's films when the venue's own programme page fails, only without their pages" in {
    val replay = new FakeHttpFetch("quad-derby")
    val fetch = new GetOnlyHttpFetch {
      def get(url: String): String =
        if (url == QuadDerbyClient.WhatsOnUrl) throw new IOException("programme refused") else replay.get(url)
    }
    val listed = new QuadDerbyClient(fetch, LocalDate.of(2026, 10, 18)).fetch()
    listed.map(_.movie.title) shouldBe results.map(_.movie.title)
    listed.flatMap(_.filmUrl) shouldBe empty
  }

  it should "leave a film the programme bills under another name without a page, never a guessed one" in {
    // The programme bills it "The Blob", in its Geek Bank season.
    byTitle("Geek Bank Presents: The Blob (18)").filmUrl shouldBe None
  }

  "QuadDerbyClient.fetchFilmDetail" should "read the film's facts, synopsis and poster off its page" in {
    val detail = client.fetchFilmDetail("https://www.derbyquad.co.uk/whats-on/obsession-pg").value
    detail.releaseYear shouldBe Some(1949)
    detail.director shouldBe Seq("Edward Dmytryk")
    detail.countries shouldBe Seq("UK")
    detail.genres shouldBe Seq("Thriller")
    detail.runtimeMinutes shouldBe Some(96)
    detail.synopsis.value should startWith ("Revered for his chilling performance as Bill Sykes in David Lean’s *Oliver Twist*")
    detail.synopsis.value should include ("restored by Powerhouse Films")
    detail.posterUrl shouldBe Some("https://ik.imagekit.io/l2o8gnuspz/quad/application/files/6117/9086/6943/OBSESSION-WEB.jpg")
  }
}
