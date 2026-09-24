package controllers

import models.{Movie, MovieRecord, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

/** Pins the film page's og:description text ([[FilmPreviewText]]). (The share
 *  card's director label, which this spec used to cover, is the worker's now —
 *  `ShareCardInputsSpec`, held to the web's own word by `ShareCardTextSpec`.) */
class FilmPreviewTextSpec extends AnyFlatSpec with Matchers {

  private def ratedSched(title: String, ratings: ResolvedRatings, synopsis: Option[String] = None): FilmSchedule =
    FilmSchedule(Movie(title), posterUrl = None, synopsis = synopsis, cast = Nil, director = Nil,
      cinemaFilmUrls = Nil, showings = Nil,
      resolved = TestReadModel.resolved(title, None, MovieRecord()).copy(ratings = ratings),
      slug = FilmHref.slugOf(title), asOf = java.time.LocalDate.of(2026, 6, 4))

  private def ratings(imdb: Option[Double] = None, metascore: Option[Int] = None,
                      rt: Option[Int] = None, filmweb: Option[Double] = None): ResolvedRatings =
    ResolvedRatings(
      imdb = imdb, imdbUrl = None,
      metascore = metascore, metacriticUrl = "https://www.metacritic.com/",
      rottenTomatoes = rt, rottenTomatoesUrl = "https://www.rottentomatoes.com/",
      filmweb = filmweb, filmwebUrl = "https://www.filmweb.pl/"
    )

  "previewDescription" should "carry only the synopsis, never the ratings" in {
    FilmPreviewText.previewDescription(
      ratedSched("X", ratings(imdb = Some(8.8), rt = Some(87)), synopsis = Some("Sen w śnie."))) shouldBe
      "Sen w śnie."
    FilmPreviewText.previewDescription(ratedSched("X", ratings(imdb = Some(8.8)))) shouldBe ""
    FilmPreviewText.previewDescription(
      ratedSched("X", ratings(), synopsis = Some("Tylko opis."))) shouldBe "Tylko opis."
  }
}
