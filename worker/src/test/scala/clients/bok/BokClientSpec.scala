package clients.bok

import clients.tools.FakeHttpFetch
import models.{KinoGlebocka66, KinoNaBoku, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.BokClient

import java.time.{LocalDate, LocalDateTime}

/** Replays a recorded bok.waw.pl listing + its week of day pages
 *  (`/<prefix>,ts:<epoch>`) + the film detail pages through the client. The day
 *  pages are the load-bearing source: the listing alone shows only today's
 *  screenings, and most detail pages no longer carry a `movieshow-list` block,
 *  so the earlier detail-only parser captured ~1 film. Walking the day tabs
 *  recovers the whole week — 8 films / 16 showtimes at na Boku, 6 / 9 at
 *  Głębocka — with booking links merged back in from the detail pages. */
class BokClientSpec extends AnyFlatSpec with Matchers {

  // The recording's "today" — the base `/<prefix>` page is that day; the timestamp day
  // tabs run 07.06–12.06.
  private val today = LocalDate.of(2026, 6, 6)

  // ── Kino na Boku ─────────────────────────────────────────────────────────
  private val naBoku    = new BokClient(new FakeHttpFetch("kino-na-boku"), "kino-na-boku", KinoNaBoku, today)
  private val naBokuRes = naBoku.fetch()
  private val naBokuByT = naBokuRes.map(cm => cm.movie.title -> cm).toMap

  "BokClient (na Boku)" should "walk the day tabs and return the whole week, not just today" in {
    naBokuRes.size shouldBe 8
    naBokuRes.flatMap(_.showtimes).size shouldBe 16
  }

  it should "assign Kino na Boku to every entry" in {
    naBokuRes.map(_.cinema).toSet shouldBe Set(KinoNaBoku)
  }

  it should "aggregate a film's showtimes across multiple days" in {
    // Zawodowcy screens five times across four distinct days (06-11 twice) —
    // proof the client reads the timestamp day pages, not just the listing's single day.
    val zawodowcy = naBokuByT("Zawodowcy")
    zawodowcy.showtimes.size shouldBe 5
    zawodowcy.showtimes.map(_.dateTime.toLocalDate).distinct.size shouldBe 4
  }

  it should "strip the promo suffix, enrich runtime, and merge biletyna booking URLs from the detail page" in {
    val m = naBokuByT("Drzewo magii") // "Drzewo magii | PREMIERA" with the promo tag dropped
    m.movie.runtimeMinutes shouldBe Some(110)
    m.showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 7, 15, 30), Some("https://iframe17.biletyna.pl/event/view/id/665164"), None, Nil)
  }

  it should "keep the recurring-programme banner so distinct films don't collapse onto it" in {
    // Each keeps its banner AND its film — not bare "Kino dla Seniora".
    naBokuByT.keys should contain ("Kino dla Seniora: Tajny agent")
    naBokuByT.keys should contain ("Kinowy Poranek: Wyspa Puffinów. Nowi przyjaciele")
  }

  it should "read the cast list off the detail page" in {
    naBokuByT("Drzewo magii").cast shouldBe
      Seq("Andrew Garfield", "Claire Foy", "Rebecca Ferguson", "Nicola Coughlan", "Jessica Gunning")
  }

  // ── Kino Głębocka 66 (same client, different slug prefix) ─────────────────
  private val gleb    = new BokClient(new FakeHttpFetch("kino-glebocka-66"), "kino-glebocka-66", KinoGlebocka66, today)
  private val glebRes = gleb.fetch()

  "BokClient (Głębocka 66)" should "return 6 films and 9 showtimes, all tagged Głębocka 66" in {
    glebRes.size shouldBe 6
    glebRes.flatMap(_.showtimes).size shouldBe 9
    glebRes.map(_.cinema).toSet shouldBe Set(KinoGlebocka66)
  }
}
