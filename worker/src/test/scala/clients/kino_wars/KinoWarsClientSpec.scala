package clients.kino_wars

import clients.tools.{FailingHttpFetch, FakeHttpFetch, RequestLogHttpFetch}
import models.KinoWars
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.KinoWarsClient
import tools.HttpStatusException

import java.time.LocalDateTime

/** Replays the 2026-09-27 capture of Kino Wars' own repertoire
 *  (`kino.wysokiemazowieckie.pl/repertuar` + its `?start=15` second page) — one
 *  Joomla blog post per film with "DD.MM.YYYY r. - godz. HH:MM" date lines.
 *
 *  Fixture directory: test/resources/fixtures/kino-wars/ (recorded with
 *  RecordingHttpFetch over RealHttpFetch). */
class KinoWarsClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http = new RequestLogHttpFetch(new FakeHttpFetch("kino-wars"))
  private val movies = new KinoWarsClient(http).fetch()

  private def film(title: String) = movies.find(_.movie.title == title).value

  "KinoWarsClient" should "read every page of the paginated repertoire" in {
    http.gets shouldBe Seq(KinoWarsClient.RepertoireUrl, s"${KinoWarsClient.RepertoireUrl}?start=15")
    movies.map(_.cinema).toSet shouldBe Set(KinoWars)
    movies.size shouldBe 12
    movies.flatMap(_.showtimes).size shouldBe 60
  }

  it should "pin a film's exact showtimes, each booking through its iKsoris ticket page" in {
    val gwiazdozbior = film("Gwiazdozbiór psa")
    gwiazdozbior.showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 9, 25, 17, 0),
      LocalDateTime.of(2026, 9, 26, 20, 0),
      LocalDateTime.of(2026, 9, 27, 17, 0),
      LocalDateTime.of(2026, 9, 30, 20, 0),
      LocalDateTime.of(2026, 10, 1, 17, 0)
    )
    gwiazdozbior.showtimes.flatMap(_.bookingUrl).distinct shouldBe
      Seq("http://bilety.kino.wysokiemazowieckie.pl/rezerwacja/termin.html?idl=0&idg=0&idw=847&d=3")
    all(gwiazdozbior.showtimes.map(_.format)) shouldBe List("NAP")
  }

  it should "emit the runtime, genres, age rating, poster, trailer and synopsis the post carries" in {
    val gwiazdozbior = film("Gwiazdozbiór psa")
    gwiazdozbior.movie.runtimeMinutes.value shouldBe 119
    gwiazdozbior.movie.genres shouldBe Seq("Akcja", "przygodowy", "sci-fi", "thriller")
    gwiazdozbior.ageRating.value shouldBe "14+"   // the "N+" every other PL venue badges
    gwiazdozbior.posterUrl.value shouldBe
      "https://kino.wysokiemazowieckie.pl/images/filmy/2026/Gwiazdozbiór_Psa_-_plakat_główny_net_zmniejszony.jpg"
    gwiazdozbior.filmUrl.value shouldBe "https://kino.wysokiemazowieckie.pl/repertuar/gwiazdozbior-psa"
    gwiazdozbior.trailerUrl.value shouldBe "https://www.youtube.com/watch?v=WHlue-wpHcE"
    gwiazdozbior.synopsis.value should startWith("Gwiazdozbiór psa w reżyserii Ridleya Scotta")
    gwiazdozbior.synopsis.value should not include "UWAGA"
  }

  it should "strip the '/ PL' Polish-film suffix, keeping the raw title" in {
    val zeus = film("100 dni: Misja Zeus")
    zeus.movie.rawTitle.value shouldBe "100 dni: Misja Zeus / PL"
    zeus.movie.runtimeMinutes.value shouldBe 113
    zeus.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 27, 20, 0))
  }

  it should "read two shows on one date line ('godz. 17:00 i 20:00')" in {
    film("Królowe życia - spektakl").showtimes.map(_.dateTime) shouldBe Seq(
      LocalDateTime.of(2026, 10, 2, 17, 0),
      LocalDateTime.of(2026, 10, 2, 20, 0)
    )
  }

  it should "drop an announced film with no screening dates yet" in {
    movies.map(_.movie.title) should not contain "Luna i rozgadana świnka"
  }

  // ── Per-film page (deferred detail) ────────────────────────────────────────
  // The listing shows only a post's intro; the credits sit below the fold on the
  // per-film page, as plain-text lines on foreign titles:
  //   "Występują: Austin Abrams, Zach Cherry, Kali Reis i Paul Walter Hauser."
  //   "Reżyseria: Zach Cregger"
  private val client = new KinoWarsClient(new FakeHttpFetch("kino-wars"))

  it should "read the director and cast lines off a foreign film's own page" in {
    val detail = client.fetchFilmDetail(film("Resident Evil").filmUrl.value).value
    detail.director shouldBe Seq("Zach Cregger")
    detail.cast     shouldBe Seq("Austin Abrams", "Zach Cherry", "Kali Reis", "Paul Walter Hauser")
  }

  it should "leave director and cast empty on a page without those lines" in {
    val polish = client.fetchFilmDetail(film("100 dni: Misja Zeus").filmUrl.value).value
    polish.director shouldBe empty
    polish.cast     shouldBe empty
    val foreignWithout = client.fetchFilmDetail(film("Gwiazdozbiór psa").filmUrl.value).value
    foreignWithout.director shouldBe empty
    foreignWithout.cast     shouldBe empty
  }

  it should "propagate a fetch failure instead of reporting an empty (white) scrape" in {
    a[HttpStatusException] should be thrownBy new KinoWarsClient(new FailingHttpFetch(503)).fetch()
  }

  it should "fail rather than truncate when the pagination never ends" in {
    // Every page links a next one: a runaway that must read red, not as a
    // programme cut at the backstop with the rest pruned.
    val endless = new _root_.tools.HttpFetch {
      def get(url: String): String =
        s"""<div class="pagination-next"><a href="${KinoWarsClient.RepertoireUrl}?start=${url.length}">dalej</a></div>"""
      def post(url: String, body: String, contentType: String): String = ""
    }
    an[IllegalStateException] should be thrownBy new KinoWarsClient(endless).fetch()
  }
}
