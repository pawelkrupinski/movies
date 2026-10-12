package clients.squarespace

import clients.tools.FakeHttpFetch
import models.{TimeZones, UsRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.us.SquarespaceEventsClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a 2026-10-12 capture of
 *  https://www.acmescreeningroom.org/now-playing?format=json (`WriteSquarespaceEvents`).
 *
 *  ACME Screening Room (Lambertville, NJ) was retired as closed on 2026-10-12 because
 *  its flicks.us page had 404'd since 2026-08-31 — while its own site sold the
 *  Lambertville Halloween Film Festival and screenings into December. */
class SquarespaceEventsClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Site   = "https://www.acmescreeningroom.org"
  private val Acme   = UsRoster.byDisplayName("ACME Screening Room Lambertville")
  private val Zone   = TimeZones.named("America/New_York")
  private val today  = LocalDate.of(2026, 10, 12)
  private def scrape(on: LocalDate) =
    new SquarespaceEventsClient(new FakeHttpFetch("squarespace"), Site, "/now-playing", Acme, Zone, on).fetch()
  private val results = scrape(today)
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private val DeadFunny = "2026 LHFF Dead Funny: Horror/Halloween Comedy & Dark Humor"
  private val Scream    = "2026 LHFF Scream (1996)"

  "SquarespaceEventsClient.fetch" should "read every upcoming screening the venue publishes, to its furthest date" in {
    results.size shouldBe 9
    results.flatMap(_.showtimes).size shouldBe 9
    results.map(_.cinema).toSet shouldBe Set(Acme)
    results.flatMap(_.showtimes).map(_.dateTime).max shouldBe LocalDateTime.of(2026, 12, 4, 19, 0)
  }

  it should "read each start instant as the venue's wall clock, cut to the minute" in {
    byTitle(Scream).showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 10, 18, 13, 0))
    byTitle("2026 LHFF Mind Games: Psychological & Tension-Driven Thrillers").showtimes.map(_.dateTime) shouldBe
      Seq(LocalDateTime.of(2026, 10, 17, 15, 45))
  }

  it should "drop the party, the block pass and the live stage show the collection sells beside its films" in {
    val titles = results.map(_.movie.title.toLowerCase)
    titles.filter(_.contains("monster bash")) shouldBe empty
    titles.filter(_.contains("block pass")) shouldBe empty
    titles.filter(_.contains("clue")) shouldBe empty
  }

  it should "keep a film billed with its live accompaniment or its post-screening Q&A" in {
    byTitle.keySet should contain ("Spotlight on Cinema Double Bill: Frankenstein (1910) + He Who Gets Slapped (1924) with Live Music")
    byTitle.keySet.exists(_.startsWith("Spotlight on Cinema: “The Peasants” with Introduction")) shouldBe true
  }

  it should "unescape the entities and non-breaking spaces Squarespace leaves in titles" in {
    byTitle.keySet should contain (DeadFunny)
    byTitle.keySet.filter(t => t.contains("&amp;") || t.contains("&nbsp;") || t.contains(" ")) shouldBe empty
  }

  it should "book through the event's own OvationTix production, not the festival pass every block links to" in {
    byTitle(DeadFunny).showtimes.head.bookingUrl.value shouldBe "https://ci.ovationtix.com/35991/production/1286523"
    byTitle(Scream).showtimes.head.bookingUrl.value shouldBe "https://ci.ovationtix.com/35991/production/1293867"
  }

  it should "carry the event page, poster and synopsis" in {
    val scream = byTitle(Scream)
    scream.filmUrl.value shouldBe s"$Site/now-playing/scream-1996"
    scream.posterUrl.value should startWith ("https://images.squarespace-cdn.com/")
    scream.synopsis.value should not include "<p"
  }

  it should "drop screenings before today" in {
    val later = scrape(LocalDate.of(2026, 10, 18))
    later.flatMap(_.showtimes).map(_.dateTime.toLocalDate).min shouldBe LocalDate.of(2026, 10, 18)
    later.map(_.movie.title) should not contain "2026 LHFF Vincent Price: What Keeps Us Alive"
  }

  "SquarespaceEventsClient.parse" should "refuse a body that is not an events collection rather than call it an empty programme" in {
    an[IllegalArgumentException] should be thrownBy
      SquarespaceEventsClient.parse("""{"collection":{}}""", Site, Acme, Zone, today)
  }
}
