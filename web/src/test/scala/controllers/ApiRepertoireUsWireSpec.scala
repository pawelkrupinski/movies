package controllers

import models.{MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/**
 * THE US LISTING AS THE APPS RECEIVE IT. The US web prints showtimes on a 12-hour
 * clock ("7:30 PM"), but both apps sort, bucket and prune on the API's `HH:mm`
 * (`Film.earliestShowing`, the from-hour filter, past-showtime pruning), so the
 * web's format must never leak into `/api/repertoire`. Pinned here on a New York
 * film with evening, past-midnight and noon slots — the three a 12-hour clock
 * spells differently.
 *
 * The same response, carrying what only a non-Polish listing carries (an
 * age-rating certificate, English day labels, US rating sites, a booking link),
 * is checked in as the iOS and Android decoder fixtures — rewritten here when
 * the wire moves (re-run to confirm, then commit both), so the apps' decoding
 * tests read what the server actually writes rather than a hand-typed copy.
 */
class ApiRepertoireUsWireSpec extends AnyFlatSpec with Matchers {

  private val newYork = models.City.bySlug("new-york").getOrElse(fail("no new-york city"))
  private val venue   = newYork.cinemas.head
  private val today   = java.time.LocalDate.of(2026, 6, 10) // TestMovieController.clock's date

  private lazy val controller: MovieController = {
    val record = MovieRecord(
      imdbId            = Some("tt1234567"),
      imdbRating        = Some(7.4),
      metascore         = Some(68),
      rottenTomatoes    = Some(91),
      // A Filmweb score a US row should never have, so the spec sees it withheld.
      filmwebRating     = Some(6.9),
      metacriticUrl     = Some("https://www.metacritic.com/movie/wire-test/"),
      rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/wire_test"),
      data = Map[Source, SourceData](
        venue -> SourceData(
          title     = Some("Wire Test"),
          showtimes = Seq(
            Showtime(today.atTime(19, 30), Some("https://tickets.example.com/b/1930"), Some("Theater 4"), List("IMAX")),
            Showtime(today.plusDays(1).atTime(0, 5), Some("https://tickets.example.com/b/0005"), None, Nil),
            Showtime(today.plusDays(1).atTime(12, 30), None, None, Nil),
          )
        ),
        Tmdb -> SourceData(ageRating = Some("PG-13"), releaseYear = Some(2026), runtimeMinutes = Some(118),
          genres = Seq("Drama"), countries = Seq("United States"), director = Seq("Jane Doe"), cast = Seq("John Roe"))
      )
    )
    TestMovieController.build(Seq(("Wire Test", Some(2026), record)), servingCountry = models.Country.UnitedStates)._1
  }

  private lazy val body: String = {
    val result = controller.apiRepertoire(newYork.slug)(FakeRequest())
    status(result) shouldBe OK
    contentAsString(result)
  }

  private def times(json: JsValue): Seq[String] =
    (json \\ "showtimes").flatMap(_.as[Seq[JsValue]]).map(s => (s \ "time").as[String]).toSeq

  "A US city's /api/repertoire" should "carry every showtime as 24-hour HH:mm, not the web's 12-hour clock" in {
    times(Json.parse(body)) shouldBe Seq("19:30", "00:05", "12:30")
  }

  it should "carry the non-Polish fields the apps decode" in {
    val film = Json.parse(body).as[Seq[JsValue]].head
    (film \ "ageRating").as[String] shouldBe "PG-13"
    (film \ "showings" \ 0 \ "label").as[String] should include ("June")
  }

  // Filmweb is a Polish site: a US listing neither links nor scores it, on the API,
  // the listing page or its JSON-LD — even for a row that carries a Filmweb rating.
  it should "offer no Filmweb link or score" in {
    val ratings = Json.parse(body).as[Seq[JsValue]].head \ "ratings"
    (ratings \ "filmwebURL").toOption shouldBe None
    (ratings \ "filmweb").toOption shouldBe None
  }

  "A US city's listing page" should "offer no Filmweb link or score, in the cards or the JSON-LD" in {
    val html = contentAsString(controller.index(newYork.slug)(FakeRequest("GET", s"/${newYork.slug}/")))
    html should include ("Wire Test")
    // Links and pills only: the page also embeds the Polish language pack, whose copy names Filmweb.
    // Links and pills only: the page also carries the pill's CSS and the Polish language
    // pack, whose copy names Filmweb.
    html should (not include "filmweb.pl" and not include "class=\"rating-fw\"")
  }

  private val fixtures = Seq(
    "ios/Tests/KinowoCoreTests/Fixtures/api_repertoire_us.json",
    "android/app/src/test/resources/fixtures/repertoire_us.json",
  )

  fixtures.foreach { rel =>
    "A US city's /api/repertoire body" should s"be what the decoder fixture $rel holds" in {
      // web's Test JVM is forked in `web/` (guarded by TestJvmSpec), so the repo root is its parent.
      val file    = new java.io.File("..", rel)
      val current = if (file.exists()) new String(Files.readAllBytes(file.toPath), StandardCharsets.UTF_8) else null
      if (current != body) {
        Files.write(file.toPath, body.getBytes(StandardCharsets.UTF_8))
        fail(s"Decoder fixture $rel was missing/stale — rewrote it. Re-run to confirm, then commit it.")
      }
    }
  }
}
