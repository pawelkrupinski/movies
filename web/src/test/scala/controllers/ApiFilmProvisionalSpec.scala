package controllers

import models.{Helios, Movie, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsBoolean, Json}
import services.readmodel.TestReadModel

import java.time.LocalDate

/** Step 6 of the identity structural plan (the owner, 2026-10-31): a film whose identity take is Pending -- decided on the
 *  inputs as they stand while an answer it reads is not filed yet -- is served as decided, marked `provisional` in the
 *  repertoire API. A flag, never an earlier take; omitted for a decided film, so the apps' payload is unchanged there. */
class ApiFilmProvisionalSpec extends AnyFlatSpec with Matchers {

  private def film(record: MovieRecord): FilmSchedule =
    FilmSchedule(
      movie          = Movie("Rose", runtimeMinutes = Some(106), releaseYear = Some(2022), genres = Nil),
      posterUrl      = None,
      synopsis       = None,
      cast           = Nil,
      director       = Nil,
      cinemaFilmUrls = Seq(Helios -> "https://helios.pl/film/rose"),
      showings       = Nil,
      resolved       = TestReadModel.resolved("Rose", Some(2022), record),
      slug           = FilmHref.slugOf("Rose"),
      asOf           = LocalDate.of(2026, 10, 31)
    )

  "the repertoire film" should "say provisional while its identity take is Pending" in {
    val json = Json.toJson(ApiFilm.from(film(MovieRecord(tmdbId = Some(901251), tmdbProvisional = Some(true))), java.util.Locale.ENGLISH))
    (json \ "provisional").toOption shouldBe Some(JsBoolean(true))
  }

  it should "carry no provisional field once its take is decided" in {
    val json = Json.toJson(ApiFilm.from(film(MovieRecord(tmdbId = Some(901251))), java.util.Locale.ENGLISH))
    (json \ "provisional").toOption shouldBe None
  }
}
