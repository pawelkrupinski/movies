package clients.the_old_court

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.TheOldCourtWindsor
import services.cinemas.uk.TheOldCourtClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a recorded 2026-07-31 capture of https://oldcourt.org.uk/events.
  *
  * The Old Court was wired to the flicks.co.uk slug `the-screen-cinema-windsor` —
  * a DIFFERENT Windsor venue. The Old Court is absent from Flicks' entire
  * `sitemap-cinemas.xml`, so the venue scraped to zero forever: a white uptime bar
  * caused by our own mapping, not by the venue or the aggregator. Its own site
  * carries the programme, and this reads it. */
class TheOldCourtClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today   = LocalDate.of(2026, 7, 31)
  private val results = new TheOldCourtClient(new FakeHttpFetch("the-old-court"), TheOldCourtWindsor, today).fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  "TheOldCourtClient.fetch" should "find the cinema programme the Flicks mapping never could" in {
    results.size shouldBe 12
    results.flatMap(_.showtimes).size shouldBe 15
    results.map(_.cinema).toSet shouldBe Set(TheOldCourtWindsor)
  }

  it should "keep only events booked under the cinema's own box-office path" in {
    // The listing is one flat stream of 42 events mixing the cinema in with the
    // bar and theatre programme. `/sales/the-old-court-cinema/` is the only
    // reliable discriminator — the titles are not ("Rocky Horror Night" IS a film
    // here, "Alice In Wonderland - The Ballet" is not).
    byTitle.keySet should contain ("Rocky Horror Night")
    byTitle.keySet should not contain "The Old Court Quiz Night"
    byTitle.keySet should not contain "Alice In Wonderland - The Ballet"
    byTitle.keySet should not contain "Bar Beats: DJ Little Kate"
    byTitle.keySet should not contain "Sound Bath Healing"
  }

  it should "read the date, time and booking link off the screening line" in {
    val film = byTitle("Whilst she was gone")
    film.showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2026, 8, 7, 20, 30))
    film.showtimes.head.bookingUrl shouldBe
      Some("https://tickets.oldcourt.org/sales/the-old-court-cinema/whilst-she-was-gone")
    film.filmUrl  shouldBe Some("https://oldcourt.org.uk/event/11283")
    film.posterUrl.value should startWith ("https://tickets.oldcourt.org/Sales/Images/Image.ashx")
  }

  it should "collapse a programme-strand label so one film is one card" in {
    // "Tuner (The Old Courters)" (the seniors' club matinee) and "Tuner" are the
    // same film on two days. Left verbatim they would render as two near-duplicate
    // rows for the same title.
    byTitle.keySet should not contain "Tuner (The Old Courters)"
    byTitle("Tuner").showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 16, 15, 0),
      LocalDateTime.of(2026, 9, 17, 19, 0)
    )
    byTitle("Backrooms").showtimes.size shouldBe 2
    byTitle.keySet should not contain "Whilst she was gone (Independent film)"
  }

  it should "gather a film's separate screening lines under one entry" in {
    // "The End of Oak Street" carries two booking spans in a single event block.
    byTitle("The End of Oak Street").showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 10, 18, 17, 0),
      LocalDateTime.of(2026, 10, 22, 19, 30)
    )
  }

  it should "infer the year from today, since the page prints none" in {
    // Dates read "Fri 7th Aug 20:30-21:15" — no year anywhere on the page. Every
    // date in this capture is later in the same year...
    all(results.flatMap(_.showtimes).map(_.dateTime.getYear)) shouldBe 2026
    // ...but read from December, the same August listing must roll into next year
    // rather than resurrect a date eight months past.
    val fromDecember =
      new TheOldCourtClient(new FakeHttpFetch("the-old-court"), TheOldCourtWindsor, LocalDate.of(2026, 12, 1)).fetch()
    fromDecember.find(_.movie.title == "Whilst she was gone").value
      .showtimes.map(_.dateTime) shouldBe Seq(LocalDateTime.of(2027, 8, 7, 20, 30))
  }
}
