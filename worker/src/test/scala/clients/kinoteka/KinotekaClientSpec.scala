package clients.kinoteka

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import models.{Kinoteka, Showtime}
import services.cinemas.common.FilmDetail
import services.cinemas.pl.KinotekaClient

import java.time.LocalDateTime

class KinotekaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinotekaClient(new FakeHttpFetch("kinoteka"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title"))
    ).getOrElse(fail(s"no detail for $title"))

  "KinotekaClient.fetch" should "walk the date nav and return 82 films / 268 showtimes" in {
    results.size shouldBe 82
    results.flatMap(_.showtimes).size shouldBe 268
  }

  it should "assign Kinoteka to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Kinoteka)
  }

  it should "merge a film across days and carry listing fields on the bare fetch result" in {
    val m = byTitle("Zawodowcy")
    m.showtimes.size       shouldBe 28
    m.movie.genres         shouldBe Seq("Akcja", "Dramat")
    m.filmUrl              shouldBe Some("https://kinoteka.pl/film/zawodowcy/")
    byTitle("Diabeł ubiera się u Prady 2").showtimes.size shouldBe 21
  }

  it should "enrich runtime / year / countries / original title from the detail page" in {
    val d = detailFor("Zawodowcy")
    // The detail page lists two durations — "Czas trwania filmu" (the film) and
    // "Czas trwania reklam" (the trailer/ad block). Runtime must be the film's
    // 100 min, never the 15-min ad row.
    d.runtimeMinutes shouldBe Some(100)
    d.releaseYear    shouldBe Some(2026)
    d.countries      shouldBe Seq("USA", "Wielka Brytania")
    d.originalTitle  shouldBe Some("In the Grey")
  }

  it should "take the film poster from the hero <picture>, not the generic site og:image" in {
    detailFor("Zawodowcy").posterUrl shouldBe Some("https://medstore.kinoteka.pl/image001(1).jpg")
  }

  it should "read the cast list and the YouTube trailer off the detail page" in {
    val d = detailFor("Zawodowcy")
    d.cast       shouldBe Seq("Henry Cavill", "Rosamund Pike", "Jake Gyllenhaal")
    d.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=AYq1ljpbNfA")
  }

  it should "read the film runtime even when the ad-block duration row comes first" in {
    val html =
      """<dl class="p-movie-details__general-info">
        |  <dt>Czas trwania reklam:</dt><dd>15 min</dd>
        |  <dt>Czas trwania filmu:</dt><dd>137 min</dd>
        |</dl>""".stripMargin
    KinotekaClient.parseDetail(html).runtime shouldBe Some(137)
  }

  it should "carry the screening booking URL with absolute date" in {
    byTitle("Zawodowcy").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 14, 15),
        Some("https://bilety.kinoteka.pl/#/screen?screeningId=42d45fbe-db10-4ef1-b5e4-0eeb0c149927&cinemaId=9ef78349-db9c-4dfc-85aa-96d030082c0d"),
        None, Nil
      )
  }

  // Regression: event pages append a "Harmonogram wydarzenia: …" agenda + a
  // partner/sponsor list as trailing body paragraphs; keep only the prose.
  it should "drop the trailing event agenda from synopses" in {
    val synopses = results.flatMap(_.filmUrl).flatMap(client.fetchFilmDetail).flatMap(_.synopsis)
    synopses.size should be >= 70 // the fix doesn't gut the corpus of synopses
    synopses.foreach(_ should not include "Harmonogram")
  }

  // Chunked scrape: the day pages are now fanned out as independent ScrapeChunk
  // tasks (one per date) instead of one synchronous scrape, so a slow day can't
  // pin the whole ~2-week scrape. The split must be behaviour-preserving — the
  // assertions above run through the synchronous `fetch()` (= reduce∘fetchChunk∘
  // planChunks) and still see 82 films / 268 showtimes.
  "KinotekaClient.planChunks" should "enumerate the date nav as the chunk keys" in {
    val dates = client.planChunks()
    dates.size should be > 1
    all(dates) should fullyMatch regex """\d{4}-\d{2}-\d{2}"""
    client.fetchChunk(dates.head) should not be empty // a single day yields films
  }
}
