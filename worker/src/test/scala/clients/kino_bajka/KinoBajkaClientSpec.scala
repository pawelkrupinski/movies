package clients.kino_bajka

import clients.tools.FakeHttpFetch
import models.KinoBajka
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoBajkaClient

import java.time.LocalDateTime

/** Replays the recorded Kino Bajka repertoire (06-06-2026 capture of
 *  `kinobajka.pl/repertuar/`) through the client. The WordPress page renders
 *  the whole advance window inline — one `div.screening-day[id=screening-YYYYMMDD]`
 *  per date, each holding `div.screening-item` blocks — so a single fetch carries
 *  every screening. The day id supplies the date that each `span.time` pairs with. */
class KinoBajkaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new FakeHttpFetch("kino-bajka")

  "KinoBajkaClient" should "parse film screenings off the repertoire page" in {
    val movies = new KinoBajkaClient(http, KinoBajka).fetch()

    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoBajka)

    // Every film has at least one showtime; all dates fall in the captured 2026 window.
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "pair each span.time with the enclosing day's date" in {
    val movies = new KinoBajkaClient(http, KinoBajka).fetch()

    // Pinned screening seen in the captured fixture: "Drzewo magii" plays
    // 07-06-2026 at 13:30. The 13:30 lives in the screening-20260607 day block,
    // so this verifies the day-id → date pairing.
    val drzewo = movies.find(_.movie.title == "Drzewo magii").value
    drzewo.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 13, 30))

    // Booking host, runtime and de-slugged production country come off the item.
    val slot = drzewo.showtimes.find(_.dateTime == LocalDateTime.of(2026, 6, 7, 13, 30)).value
    slot.bookingUrl.value shouldBe "https://bajka-lublin.biletpro24.pl"
    drzewo.movie.runtimeMinutes.value shouldBe 110
    drzewo.movie.countries shouldBe Seq("wielka brytania")
  }

  it should "merge a film's screenings across days into one CinemaMovie" in {
    val movies = new KinoBajkaClient(http, KinoBajka).fetch()

    // "Drzewo magii" plays on several days; it collapses to a single row whose
    // showtimes span both 07-06 and 08-06 (11:00).
    val drzewo = movies.filter(_.movie.title == "Drzewo magii")
    drzewo should have size 1
    drzewo.head.showtimes.map(_.dateTime) should contain allOf (
      LocalDateTime.of(2026, 6, 7, 13, 30),
      LocalDateTime.of(2026, 6, 8, 11, 0)
    )
  }
}
