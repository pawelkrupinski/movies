package clients.kino_kreska

import models.KinoKreska
import org.scalatest.OptionValues
import clients.tools.{ConstantHttpFetch, FailingHttpFetch, FakeHttpFetch}
import tools.{HttpFetch, RefusedRedirectException, UnexpectedBodyException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoKreskaClient

import java.net.URI
import java.time.LocalDateTime

/** Replays the recorded SFR/Kino Kreska response (2026-06-21 capture of
 *  `www.sfr.pl/heroapp/terms/rest/load`, category "Repertuar kinowy")
 *  through the client. The endpoint returns a JSON envelope with an `items`
 *  HTML fragment; each `<li>` tile carries ISO date + time, so no year
 *  inference is needed. */
class KinoKreskaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoKreskaClient(new FakeHttpFetch("kino-kreska"), KinoKreska, today = _root_.tools.SpecClock.PinnedDay).fetch()

  "KinoKreskaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKreska)
  }

  it should "produce non-empty titles for every film" in {
    all(movies.map(_.movie.title)) should not be empty
  }

  it should "give every film at least one showtime" in {
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "produce plausible dates and times for all showtimes" in {
    val showtimes = movies.flatMap(_.showtimes)
    all(showtimes.map(_.dateTime.getYear)) shouldBe 2026
    all(showtimes.map(_.dateTime.getHour)) should (be >= 8 and be <= 23)
  }

  it should "pin a concrete film from the captured repertoire" in {
    // "Drzewo magii" (family film) screens on 2026-06-21 at 15:00
    val drewMagii = movies.find(_.movie.title == "Drzewo magii").value
    drewMagii.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 21, 15, 0))
  }

  it should "carry a booking URL for showtimes that have a ticket button" in {
    // The captured repertoire includes bookable screenings
    movies.flatMap(_.showtimes).exists(_.bookingUrl.isDefined) shouldBe true
  }

  // ── The film's /wydarzenie page (recorded 2026-10-06) ─────────────────────

  private val client = new KinoKreskaClient(new FakeHttpFetch("kino-kreska"), KinoKreska, today = _root_.tools.SpecClock.PinnedDay)
  private def detailOf(page: String) = client.fetchFilmDetail(s"${KinoKreskaClient.BaseUrl}/wydarzenie/$page").value

  it should "read the event page's one-line credit — Czarna woda" in {
    // https://www.sfr.pl/wydarzenie/1147/czarna-woda-watch-docs-2026 — "reż. Natxo Leuza, Hiszpania 2025, 85'"
    val detail = detailOf("1147/czarna-woda-watch-docs-2026")
    detail.director shouldBe Seq("Natxo Leuza")
    detail.countries shouldBe Seq("Hiszpania")
    detail.releaseYear.value shouldBe 2025
    detail.runtimeMinutes.value shouldBe 85
    detail.synopsis.value should startWith("Z powodu huraganów")
    detail.synopsis.value should not include "reż."
    // The page's og:image is the site's default cover; the event's own image is the poster.
    detail.posterUrl.value shouldBe "https://bilety.sfr.pl/uploads/event/1147/1789469157.jpg"
  }

  it should "read a credit line the venue wrapped over two source lines — Prawda czy wyzwanie" in {
    // https://www.sfr.pl/wydarzenie/1145/prawda-czy-wyzwanie-watch-docs-2026
    val detail = detailOf("1145/prawda-czy-wyzwanie-watch-docs-2026")
    detail.director shouldBe Seq("Tonislav Hristov")
    detail.countries shouldBe Seq("Finlandia", "Bułgaria", "Szwecja", "Norwegia")
    detail.releaseYear.value shouldBe 2025
    detail.runtimeMinutes.value shouldBe 85
  }

  it should "read the labelled block — Róża" in {
    // https://www.sfr.pl/wydarzenie/1159/roza
    val detail = detailOf("1159/roza")
    detail.director shouldBe Seq("Markus Schleinzer")
    detail.cast shouldBe Seq("Sandra Hüller", "Caro Braun", "Marisa Growaldt", "Godehard Giese", "Augustino Renken")
    detail.countries shouldBe Seq("Austria", "Niemcy")
    detail.releaseYear.value shouldBe 2026
    detail.runtimeMinutes.value shouldBe 93
  }

  it should "state only the programme's running time for a compilation of shorts — Klasyka SFR vol. 1" in {
    // https://www.sfr.pl/wydarzenie/1133/klasyka-sfr-vol1 — a list of episodes, then "czas trwania: ok 30 min."
    val detail = detailOf("1133/klasyka-sfr-vol1")
    detail.director shouldBe empty
    detail.releaseYear shouldBe None
    detail.runtimeMinutes.value shouldBe 30
    detail.synopsis.value should startWith("Prezentowany wybór kreskówek")
  }

  // ── A listing read that teaches nothing fails the scrape; only the venue's own "none" is empty ──

  private def fetchThrough(http: HttpFetch) = new KinoKreskaClient(http, KinoKreska, today = _root_.tools.SpecClock.PinnedDay).fetch()

  it should "read the endpoint's own empty answer as a venue with nothing scheduled" in {
    // Recorded 2026-10-06 from the same endpoint for a category with no events: status 0,
    // empty `items`, `allItemsAmount: 0` — the shape a week with no screenings takes.
    fetchThrough(new FakeHttpFetch("kino-kreska-empty")) shouldBe empty
  }

  it should "fail, not read as no films, when the listing POST answers an empty body" in {
    val thrown = the [UnexpectedBodyException] thrownBy fetchThrough(new ConstantHttpFetch(""))
    thrown.getMessage should include (KinoKreskaClient.TermsUrl)
  }

  it should "fail with the refused redirect as its reason when sfr.pl redirects the POST to 127.0.0.1" in {
    // sfr.pl has answered this POST with `301 Location: http://127.0.0.1/…`; RealHttpFetch refuses
    // that hop (RealHttpFetchSpec), and the refusal must surface as the scrape's failure.
    val redirectedHome = new FailingHttpFetch((method, url) => new RefusedRedirectException(method, URI.create(url), 301,
      URI.create("http://127.0.0.1/heroapp/terms/rest/load"), "127.0.0.1 is a local address"))
    val thrown = the [RefusedRedirectException] thrownBy fetchThrough(redirectedHome)
    thrown.getMessage should include ("127.0.0.1")
  }
}
