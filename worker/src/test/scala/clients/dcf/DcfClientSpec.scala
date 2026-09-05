package clients.dcf

import org.scalatest.matchers.should.Matchers
import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import models.{DolnoslaskieCentrumFilmowe, Showtime}
import services.cinemas.common.FilmDetail
import services.cinemas.pl.DcfClient

import java.time.LocalDateTime

class DcfClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new DcfClient(new FakeHttpFetch("dcf"))
  private val results  = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle.getOrElse(title, fail(s"no movie for '$title'")).filmUrl.getOrElse(fail(s"no filmUrl for '$title'"))
    ).getOrElse(fail(s"fetchFilmDetail returned None for '$title'"))

  "DcfClient.fetch" should "return 32 films grouped by cleaned title" in {
    results.size shouldBe 32
  }

  it should "return 112 showtimes in total" in {
    results.flatMap(_.showtimes).size shouldBe 112
  }

  it should "assign DCF as the cinema for every entry" in {
    results.map(_.cinema).toSet shouldBe Set(DolnoslaskieCentrumFilmowe)
  }

  it should "merge programme-tagged screenings onto the base title" in {
    val counts = results.map(m => m.movie.title -> m.showtimes.size).toMap
    counts("Diabeł ubiera się u Prady 2") shouldBe 11
    counts("Zawodowcy")                   shouldBe 13
    counts("Znaki Pana Śliwki")           shouldBe 13
    counts("Ojczyzna")                    shouldBe 4
    counts("Obsesja")                     shouldBe 10
  }

  it should "carry the auditorium name on each showtime" in {
    byTitle("Obsesja").showtimes.map(_.room).flatten.toSet shouldBe Set("Sala Lalka", "Sala Polonia")
  }

  it should "build Bilety24 booking URLs" in {
    byTitle("Obsesja").showtimes.flatMap(_.bookingUrl).foreach { u =>
      u should startWith ("https://dcf.bilety24.pl/kup-bilety/?id=")
    }
  }

  it should "enrich metadata from the Bilety24 event page" in {
    val m = byTitle("Obsesja")
    val d = detailFor("Obsesja")
    d.runtimeMinutes shouldBe Some(127)
    d.releaseYear    shouldBe Some(2025)
    d.countries      shouldBe Seq("USA")
    d.genres         shouldBe Seq("Horror")
    d.director       shouldBe Seq("Curry Barker")
    m.posterUrl      shouldBe Some("https://image.bilety24.pl/sf_api_thumb_400/dealer-default/1491/obsesja-plakat-net.jpg")
    m.filmUrl        shouldBe Some("https://dcf.bilety24.pl/wydarzenie/?id=157574")
    d.synopsis.getOrElse("").length should be > 50
    d.trailerUrl     shouldBe Some("https://www.youtube.com/watch?v=C-h48bml6k0")
  }

  it should "return the first Obsesja showtime fully specified" in {
    byTitle("Obsesja").showtimes.head shouldBe
      Showtime(LocalDateTime.of(2026, 6, 5, 15, 30), Some("https://dcf.bilety24.pl/kup-bilety/?id=938261"), Some("Sala Lalka"), Nil)
  }

  /** The detail cache is handed down, not built here. `CachingDetailFetch` is
   *  bounded per instance, so a client that builds its own turns one budget into
   *  one per venue — the multiplicity behind worker-pl's 2026-09-05 heap alert.
   *  Detail pages must go through the injected fetch; the repertoire listing
   *  carries volatile showtimes and must NOT, so it stays on `http`. */
  "DcfClient" should "fetch detail pages through the detail fetch it is given" in {
    var detailUrls = List.empty[String]
    val listing = new FakeHttpFetch("dcf")
    val sharedDetail = new _root_.tools.HttpFetch {
      def get(url: String): String = { detailUrls ::= url; listing.get(url) }
      def post(url: String, body: String, contentType: String): String = listing.post(url, body, contentType)
    }
    val injected = new DcfClient(listing, detailHttp = Some(sharedDetail))
    val film = injected.fetch().flatMap(_.filmUrl).head
    injected.fetchFilmDetail(film).isDefined shouldBe true

    detailUrls shouldBe List(film)                       // the detail page went through the shared fetch
    film.startsWith("https://dcf.bilety24.pl/wydarzenie/") shouldBe true
  }

  "DcfClient.normalizeTitle" should "strip a trailing programme label" in {
    DcfClient.normalizeTitle("Ojczyzna | pokaz przedpremierowy") shouldBe "Ojczyzna"
    DcfClient.normalizeTitle("Znaki Pana Śliwki | FKS")          shouldBe "Znaki Pana Śliwki"
    DcfClient.normalizeTitle("Fargo")                            shouldBe "Fargo"
  }

  // Regression: some descriptions end with an organiser footer ("Więcej:
  // www.<film>.pl"); it must not leak into the synopsis.
  "DcfClient synopsis" should "drop the organiser URL footer" in {
    val synopses = results.flatMap(_.filmUrl).flatMap(client.fetchFilmDetail).flatMap(_.synopsis)
    synopses.exists(_.contains("Kultowa produkcja w reżyserii Bartosza Walaszka")) shouldBe true
    synopses.foreach { s =>
      s should not include "www."
      s should not include "najlepszeznajgorszych"
    }
  }
}
