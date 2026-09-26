package services.identity

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.StoredMovieRecord
import services.readmodel.ReadModelProjection

/** The withheld card: no ratings and no direct rating links, rather than possibly another film's.
 *  Which cards the gate withholds is [[StoredIdentityConfidenceSpec]]. */
class RatingGateSpec extends AnyFlatSpec with Matchers {

  private val slot = SourceData(title = Some("Foo"), rawTitle = Some("Foo 2D"), releaseYear = Some(2024),
    filmUrl = Some("https://mk/foo"))
  private val stored = StoredMovieRecord.synthesised("Foo", Some(2024), MovieRecord(
    imdbRating = Some(7.5), metascore = Some(80), rottenTomatoes = Some(91), filmwebRating = Some(7.1),
    imdbId = Some("tt1"), metacriticUrl = Some("https://www.metacritic.com/movie/foo"),
    rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/foo"), filmwebUrl = Some("https://www.filmweb.pl/film/Foo-2024-1"),
    tmdbId = Some(1), data = Map[Source, SourceData](Multikino -> slot)), titleNormalizer)
  private val movie = ReadModelProjection.resolve(stored, titleNormalizer)

  "a withheld card" should "carry no rating, no IMDb link and only search links, and sort as unrated" in {
    movie.ratings.imdb shouldBe Some(7.5)
    val w = RatingGate.withheld(movie)
    w.ratings.imdb shouldBe None
    w.ratings.imdbUrl shouldBe None
    w.ratings.metascore shouldBe None
    w.ratings.rottenTomatoes shouldBe None
    w.ratings.filmweb shouldBe None
    w.ratings.metacriticUrl should startWith("https://www.metacritic.com/search/")
    w.ratings.rottenTomatoesUrl should startWith("https://www.rottentomatoes.com/search")
    w.ratings.filmwebUrl should startWith("https://www.filmweb.pl/search")
    w.weightedRating shouldBe 0.0
    w.copy(ratings = movie.ratings, weightedRating = movie.weightedRating) shouldBe movie
  }

  "the off gate" should "serve every card untouched" in {
    RatingGate.off(stored, movie) shouldBe movie
  }
}
