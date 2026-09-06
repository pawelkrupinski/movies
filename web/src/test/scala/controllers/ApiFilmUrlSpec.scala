package controllers

import models.{Helios, Movie, MovieRecord, Poznan, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsArray, JsObject, JsString, JsValue, Json}
import services.readmodel.TestReadModel

import java.net.URI
import java.time.{LocalDate, LocalDateTime}

/**
 * EVERY URL THE JSON API EMITS PARSES UNDER A STRICT URL GRAMMAR. The mobile
 * models decode these fields as `URL`, and on Linux — and any iPhone before iOS
 * 17 — Swift's `URL(string:)` rejects a non-ASCII character outright, at which
 * point `Codable` fails the WHOLE `[Film]` listing: a city with a thousand films
 * goes blank over one cinema's poster filename. That is exactly how
 * `LocalServerRepertoireTests` went red on 2026-09-06, on `posterURL` of the
 * fifteenth film, a WordPress upload called `Milcząca-przyjaciółka_…jpg`.
 *
 * The strict parser here is "pure ASCII, and `java.net.URI` takes it" — Java's
 * URI alone lets a Unicode letter through where Swift does not, see AsciiUrlSpec.
 */
class ApiFilmUrlSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = Poznan

  private val ScrapedPoster = "http://kinobulgarska19.pl/wp-content/uploads/2026/05/Milcząca-przyjaciółka_plakat-PL_LQ.jpg"

  private val film: FilmSchedule =
    FilmSchedule(
      movie          = Movie("Milcząca przyjaciółka", runtimeMinutes = Some(120), releaseYear = Some(2025), genres = Nil),
      posterUrl      = Some(ScrapedPoster),
      synopsis       = Some("…"),
      cast           = Nil,
      director       = Nil,
      cinemaFilmUrls = Seq(Helios -> "https://helios.pl/film/Milcząca-przyjaciółka"),
      showings       = Seq(LocalDate.of(2026, 6, 4) -> Seq(CinemaShowtimes(Helios, Seq(
        Showtime(LocalDateTime.of(2026, 6, 4, 18, 0), Some("https://helios.pl/bilet?film=Milcząca przyjaciółka"), Some("Sala 1"), List("2D"))
      )))),
      resolved       = TestReadModel.resolved("Milcząca przyjaciółka", Some(2025),
        MovieRecord(imdbId = Some("tt0000001"), data = Map[Source, SourceData](
          Helios -> SourceData(posterUrl = Some(ScrapedPoster)),
          Tmdb   -> SourceData(posterUrl = Some("https://image.tmdb.org/t/p/original/ok.jpg"))))),
      slug           = FilmHref.slugOf("Milcząca przyjaciółka")
    )

  /** Every string under a key that names a URL, anywhere in the tree. */
  private def urlFields(json: JsValue, path: String = ""): Seq[(String, String)] = json match {
    case o: JsObject => o.fields.flatMap { case (k, v) => urlFields(v, s"$path.$k") }.toSeq
    case a: JsArray  => a.value.zipWithIndex.flatMap { case (v, i) => urlFields(v, s"$path[$i]") }.toSeq
    case JsString(s) if path.toLowerCase.contains("url") => Seq(path -> s)
    case _ => Nil
  }

  "the repertoire film" should "carry a poster URL a strict parser accepts, not the raw scraped link" in {
    val json = Json.toJson(ApiFilm.from(film))
    (json \ "posterURL").as[String] shouldBe
      "http://kinobulgarska19.pl/wp-content/uploads/2026/05/Milcz%C4%85ca-przyjaci%C3%B3%C5%82ka_plakat-PL_LQ.jpg"
  }

  it should "emit only URLs a strict parser accepts, in every URL-bearing field" in {
    val fields = urlFields(Json.toJson(ApiFilm.from(film)))
    withClue("the sample no longer reaches the fields it is meant to guard: ") {
      fields.map(_._1) should contain allOf (".posterURL", ".fallbackPosterURLs[0]", ".showings[0].cinemas[0].cinemaURL",
        ".showings[0].cinemas[0].showtimes[0].bookingURL", ".ratings.imdbURL")
    }
    fields.foreach { case (path, url) =>
      withClue(s"$path = $url: ") {
        url.forall(_ < 0x80) shouldBe true
        noException should be thrownBy URI.create(url)
      }
    }
  }

  it should "leave a URL that was already valid exactly as it was" in {
    val json = Json.toJson(ApiFilm.from(film))
    (json \ "fallbackPosterURLs").as[Seq[String]] should contain("https://image.tmdb.org/t/p/original/ok.jpg")
  }
}
