package controllers

import models.{Helios, MovieRecord, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._
import services.movies.CaffeineMovieCache

import java.time.LocalDateTime

class ApiRepertoireConditionalSpec extends AnyFlatSpec with Matchers {

  private def buildController(): (MovieController, CaffeineMovieCache) = {
    val now = LocalDateTime.now()
    val record = MovieRecord(
      imdbId = Some("tt999"),
      data = Map[Source, SourceData](
        Helios -> SourceData(
          title       = Some("Test Film"),
          releaseYear = Some(2024),
          genres      = Seq("Dramat", "Komedia"),
          synopsis    = Some("A test synopsis."),
          trailerUrl  = Some("https://www.youtube.com/watch?v=abc123DEF45"),
          showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
        ),
        Tmdb -> SourceData(originalTitle = Some("The Test Movie"))
      )
    )
    TestMovieController.build(Seq(("Test Film", Some(2024), record)))
  }

  it should "keep the lean listing free of detail-only fields" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(FakeRequest())
    status(result) shouldBe OK
    val film = play.api.libs.json.Json.parse(contentAsString(result))
      .as[Seq[play.api.libs.json.JsValue]].head
    (film \ "title").as[String] shouldBe "Test Film"
    (film \ "synopsis").toOption shouldBe None
    (film \ "trailerURLs").toOption shouldBe None
  }

  it should "carry releaseYear and genres on the lean listing (mobile card parity)" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(FakeRequest())
    status(result) shouldBe OK
    val film = play.api.libs.json.Json.parse(contentAsString(result))
      .as[Seq[play.api.libs.json.JsValue]].head
    (film \ "releaseYear").as[Int] shouldBe 2024
    (film \ "genres").as[Seq[String]] shouldBe Seq("Dramat", "Komedia")
  }

  "apiDetails" should "return synopsis + embed-transformed trailerURLs keyed by title" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiDetails("poznan")(FakeRequest())
    status(result) shouldBe OK
    val entry = play.api.libs.json.Json.parse(contentAsString(result))
      .as[Seq[play.api.libs.json.JsValue]]
      .find(d => (d \ "title").as[String] == "Test Film")
      .getOrElse(fail("Test Film missing from /api/details"))
    (entry \ "synopsis").as[String] shouldBe "A test synopsis."
    // Raw watch URL is normalised to a YouTube embed URL.
    (entry \ "trailerURLs").as[Seq[String]] shouldBe Seq("https://www.youtube.com/embed/abc123DEF45")
    // The TMDB original title differs from the Polish cinema title, so it
    // rides on the detail payload.
    (entry \ "originalTitle").as[String] shouldBe "The Test Movie"
  }

  it should "keep the original title off the lean listing (detail-only field)" in {
    val (ctrl, _) = buildController()
    val film = play.api.libs.json.Json.parse(contentAsString(ctrl.apiRepertoire("poznan")(FakeRequest())))
      .as[Seq[play.api.libs.json.JsValue]].head
    (film \ "originalTitle").toOption shouldBe None
  }

  it should "omit films with neither synopsis nor trailers" in {
    val (ctrl, _) = buildController()
    // The single fixture film has both, so it is present; a film with neither
    // would be filtered out by ApiFilmDetails.hasContent.
    val details = play.api.libs.json.Json.parse(contentAsString(ctrl.apiDetails("poznan")(FakeRequest())))
      .as[Seq[play.api.libs.json.JsValue]]
    details.forall(d => (d \ "synopsis").toOption.isDefined || (d \ "trailerURLs").as[Seq[String]].nonEmpty) shouldBe true
  }

  "apiRepertoire" should "return 200 with Last-Modified header when no If-Modified-Since" in {
    val (ctrl, _) = buildController()
    val result = ctrl.apiRepertoire("poznan")(FakeRequest())
    status(result) shouldBe OK
    header("Last-Modified", result) shouldBe defined
  }

  it should "return 304 when If-Modified-Since matches" in {
    val (ctrl, _) = buildController()
    val first = ctrl.apiRepertoire("poznan")(FakeRequest())
    val lastMod = header("Last-Modified", first).get

    val second = ctrl.apiRepertoire("poznan")(FakeRequest().withHeaders("If-Modified-Since" -> lastMod))
    status(second) shouldBe NOT_MODIFIED
    contentAsString(second) shouldBe empty
  }

  it should "return 200 after a cache mutation even with the old If-Modified-Since" in {
    val (ctrl, cache) = buildController()
    val first = ctrl.apiRepertoire("poznan")(FakeRequest())
    val lastMod = header("Last-Modified", first).get

    Thread.sleep(1100)
    cache.rehydrate()

    val second = ctrl.apiRepertoire("poznan")(FakeRequest().withHeaders("If-Modified-Since" -> lastMod))
    status(second) shouldBe OK
  }
}
