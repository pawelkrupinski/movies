package services.readmodel

import models.{ResolvedMovie, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilmTitlesSpec extends AnyFlatSpec with Matchers {

  private def movie(id: String, title: String, year: Option[Int]): ResolvedMovie =
    ResolvedMovie(
      _id = id, title = title, originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
      runtimeMinutes = None, releaseYear = year, genres = Seq.empty, countries = Seq.empty,
      directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
      ratings = ResolvedRatings(None, None, None, "", None, "", None, ""), weightedRating = 0.0
    )

  private val rocky      = movie("rocky2|1979", "Rocky 2", Some(1979))
  private val invite     = movie("zaproszenie|2026", "Zaproszenie", Some(2026))
  private val jakubowska = movie("zaproszenie|1986", "Zaproszenie", Some(1986))

  "FilmTitles" should "match a Roman-numeral spelling to the Arabic display title" in {
    val titles = FilmTitles(Seq(rocky))
    titles.idsFor("Rocky II") shouldBe Seq("rocky2|1979")
    titles.idsFor("Rocky 2")  shouldBe Seq("rocky2|1979")
  }

  it should "list a same-title pair newest first, the order the bare slug goes by" in {
    FilmTitles(Seq(jakubowska, invite)).idsFor("Zaproszenie") shouldBe Seq("zaproszenie|2026", "zaproszenie|1986")
  }

  it should "know nothing about a title the corpus does not hold" in {
    FilmTitles(Seq(rocky)).idsFor("Nie ma takiego filmu") shouldBe Nil
  }
}
