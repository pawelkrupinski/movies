package controllers

import deploy.RepoFile
import models.{Helios, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.json.{JsObject, Json}

import java.time.{LocalDate, LocalDateTime}

/** [[FilmFormat.tokensToStrip]] held to the vectors iOS's and Android's `FormatTokenFilter` tests read too
 *  (`test/resources/format-token-strip.json`), so the three platforms drop the same tokens from a pill. */
class FilmFormatSpec extends AnyFlatSpec with Matchers {

  private val table = Json.parse(RepoFile.read(RepoFile.locate("test/resources/format-token-strip.json"))).as[JsObject]

  private val day = LocalDate.of(2026, 11, 22)

  private def film(formats: Seq[String]): FilmSchedule = {
    val slots = formats.zipWithIndex.map { (format, i) =>
      Showtime(LocalDateTime.of(day, java.time.LocalTime.of(10 + i, 0)), None, None, format.split(' ').filter(_.nonEmpty).toList)
    }
    FilmSchedules.of(Movie("Test movie", None), day, showings = Seq(day -> Seq(CinemaShowtimes(Helios, slots))))
  }

  "FilmFormat" should "name the dimensions and the implied one the shared vectors do" in {
    FilmFormat.DimensionTokens shouldBe (table \ "dimensionTokens").as[Set[String]]
    FilmFormat.ImpliedDimension shouldBe (table \ "impliedDimension").as[String]
  }

  for (row <- (table \ "cases").as[Seq[JsObject]]) {
    val name = (row \ "name").as[String]
    it should s"strip as the shared vectors say: $name" in {
      FilmFormat.tokensToStrip(film((row \ "formats").as[Seq[String]])) shouldBe (row \ "strip").as[Set[String]]
    }
  }
}
