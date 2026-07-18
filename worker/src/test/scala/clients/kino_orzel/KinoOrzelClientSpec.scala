package clients.kino_orzel

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import models.KinoOrzel
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoOrzelClient

import java.time.LocalDateTime

/** Replays the recorded Bilety24 organiser page for MCK Kino Orzeł
 *  (`/kino/organizator/mck-kino-orzel-892`, captured 2026-06-06) through the
 *  client. Every screening is one anchor whose `title` carries the film name,
 *  date and time; the page renders each anchor twice (desktop + mobile), so the
 *  parser must dedup by (dateTime, bookingUrl) — the counts below are the
 *  DISTINCT figures, which is the behaviour under test. */
class KinoOrzelClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("bydgoszcz")
  private def movies = new KinoOrzelClient(http, KinoOrzel).fetch()

  "KinoOrzelClient" should "parse every distinct film off the organiser listing" in {
    movies.map(_.movie.title) should have size 14
    movies.map(_.cinema).toSet shouldBe Set(KinoOrzel)
    all(movies.map(_.showtimes)) should not be empty
    movies.flatMap(_.showtimes).map(_.dateTime.getYear).toSet shouldBe Set(2026)
  }

  it should "fold a film's repeated anchors into one row with deduped showtimes" in {
    // "Milcząca przyjaciółka" appears under three sessions (936210 / 936227 /
    // 936234) each rendered twice on the page — the dedup collapses the six
    // anchors to three distinct showtimes.
    val film = movies.find(_.movie.title == "Milcząca przyjaciółka").value
    film.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 6, 7, 16, 30),
      LocalDateTime.of(2026, 6, 8, 20, 15),
      LocalDateTime.of(2026, 6, 10, 20, 30),
    )
    film.showtimes.head.bookingUrl.value shouldBe
      "https://www.bilety24.pl/kup-bilet-na-892-milczaca-przyjaciolka-157267?id=936210"
  }

  it should "keep dashes inside a title rather than splitting on them" in {
    // The title separator is the dash BEFORE the date; an in-title " - " (here
    // "KONWICKI: pisarz - scenarzysta …") must survive intact.
    movies.map(_.movie.title) should contain ("KONWICKI: pisarz - scenarzysta – reżyser: „Ostatni dzień lata”")
  }
}
