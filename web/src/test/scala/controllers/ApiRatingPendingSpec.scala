package controllers

import models.{Helios, MovieRecord, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsValue, Json}
import play.api.test.FakeRequest
import play.api.test.Helpers._

/**
 * A source we link but cannot rate yet (a new release) reaches the apps additively: `imdbURL` is
 * already present without `imdb` (it derives from the IMDb id alone), and `filmwebRatingPending`
 * is the one new optional key, because `filmwebURL` is a search page whenever no stored link exists.
 * No existing key changes meaning or nullability, so builds that predate it decode as before.
 */
class ApiRatingPendingSpec extends AnyFlatSpec with Matchers {

  private val pendingRecord =
    MovieRecord(imdbId = Some("tt7654321"), filmwebUrl = Some("https://www.filmweb.pl/film/Przepis+na+swieta-2026-1"))

  private def bodyOf(record: MovieRecord): String = {
    val showtime = models.Showtime(TestCityControllers.now.plusHours(2), None, None, Nil)
    val row = record.copy(data = record.data + (Helios -> SourceData(title = Some("Pending Film"), releaseYear = Some(2026), showtimes = Seq(showtime))))
    val controller = TestCityControllers.build(Seq(("Pending Film", Some(2026), row))).api
    contentAsString(controller.apiRepertoire("poznan")(FakeRequest()))
  }

  private def ratingsOf(record: MovieRecord): JsValue =
    (Json.parse(bodyOf(record)).as[Seq[JsValue]].head \ "ratings").get

  "/api/repertoire ratings" should "flag a stored Filmweb page without a rating, and carry the IMDb link without a score" in {
    val ratings = ratingsOf(pendingRecord)
    (ratings \ "filmwebRatingPending").as[Boolean] shouldBe true
    (ratings \ "filmwebURL").as[String] shouldBe "https://www.filmweb.pl/film/Przepis+na+swieta-2026-1"
    (ratings \ "filmweb").toOption shouldBe None
    (ratings \ "imdbURL").as[String] shouldBe "https://www.imdb.com/title/tt7654321/"
    (ratings \ "imdb").toOption shouldBe None
  }

  it should "not flag a Filmweb search-page fallback, nor a rated film" in {
    (ratingsOf(MovieRecord()) \ "filmwebRatingPending").toOption shouldBe None
    val rated = ratingsOf(MovieRecord(filmwebUrl = Some("https://www.filmweb.pl/film/X-1"), filmwebRating = Some(6.5)))
    (rated \ "filmwebRatingPending").toOption shouldBe None
    (rated \ "filmweb").as[Double] shouldBe 6.5
  }

  // The apps' decoder fixtures: this response with the new key, so the iOS and Android decoding tests
  // read what the server writes. Rewritten here when the wire moves (re-run, then commit both).
  Seq(
    "ios/Tests/KinowoCoreTests/Fixtures/api_repertoire_pending.json",
    "android/app/src/test/resources/fixtures/repertoire_pending.json",
  ).foreach { rel =>
    it should s"be what the decoder fixture $rel holds" in {
      val body    = bodyOf(pendingRecord)
      val file    = new java.io.File("..", rel)
      val current = if (file.exists()) new String(java.nio.file.Files.readAllBytes(file.toPath), java.nio.charset.StandardCharsets.UTF_8) else null
      if (current != body) {
        java.nio.file.Files.write(file.toPath, body.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        fail(s"Decoder fixture $rel was missing/stale — rewrote it. Re-run to confirm, then commit it.")
      }
    }
  }
}
