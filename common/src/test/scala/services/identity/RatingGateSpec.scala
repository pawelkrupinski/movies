package services.identity

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.ConfidenceCalibration.{Calibration, Sample}
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{ListingKey, StoredMovieRecord}
import services.readmodel.ReadModelProjection

/** The confidence gate: a film whose identity decision is below the calibrated threshold is
 *  served with no ratings and no direct rating links, rather than possibly another film's. */
class RatingGateSpec extends AnyFlatSpec with Matchers {

  private val slot = SourceData(title = Some("Foo"), rawTitle = Some("Foo 2D"), releaseYear = Some(2024),
    filmUrl = Some("https://mk/foo"))
  private val stored = StoredMovieRecord.synthesised("Foo", Some(2024), MovieRecord(
    imdbRating = Some(7.5), metascore = Some(80), rottenTomatoes = Some(91), filmwebRating = Some(7.1),
    imdbId = Some("tt1"), metacriticUrl = Some("https://www.metacritic.com/movie/foo"),
    rottenTomatoesUrl = Some("https://www.rottentomatoes.com/m/foo"), filmwebUrl = Some("https://www.filmweb.pl/film/Foo-2024-1"),
    tmdbId = Some(1), data = Map[Source, SourceData](Multikino -> slot)), titleNormalizer)
  private val movie = ReadModelProjection.resolve(stored, titleNormalizer)
  private val listing = ListingKey.Native(Multikino.displayName, "https://mk/foo", "Foo 2D")

  private final case class D(listings: Set[ListingKey], confidence: Double) extends Decision {
    def tmdbId = Some(1); def explanation = Nil; def contradictions = Nil
  }

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

  "the gate" should "withhold below the threshold and serve the card untouched at or above it" in {
    val cal = Some(Calibration(0.6, 0, 0, 0, 0))
    RatingGate.gate(movie, Some(0.59), cal) shouldBe RatingGate.withheld(movie)
    RatingGate.gate(movie, Some(0.6), cal) shouldBe movie
  }

  it should "withhold nothing for a film the resolver has not decided, or without a calibration" in {
    RatingGate.gate(movie, None, Some(Calibration(0.6, 0, 0, 0, 0))) shouldBe movie
    RatingGate.gate(movie, Some(0.1), None) shouldBe movie
  }

  "a film's confidence" should "be its least confident decision's, found through its listings" in {
    val other = ListingKey.Published("Kino X", "Foo", None, Nil)
    val decisions = Seq(D(Set(listing), 0.9), D(Set(other), 0.3), D(Set(ListingKey.Published("Kino Y", "Bar", None, Nil)), 0.0))
    RatingGate.confidenceOf(Set(listing), decisions) shouldBe Some(0.9)
    RatingGate.confidenceOf(Set(listing, other), decisions) shouldBe Some(0.3)
    RatingGate.confidenceOf(Set(ListingKey.Published("Kino Z", "Baz", None, Nil)), decisions) shouldBe None
  }

  "the shadow-backed gate" should "read the row's listings off its venue slots and gate by the calibrated shadow" in {
    def shadow(conf: Double) = new ShadowDecisions {
      def latest()   = Seq(D(Set(listing), conf))
      def verdicts() = Seq(Sample(0.2, correct = false), Sample(0.8, correct = true))
    }
    RatingGate.fromShadow(shadow(0.1))(stored, movie) shouldBe RatingGate.withheld(movie)
    RatingGate.fromShadow(shadow(0.9))(stored, movie) shouldBe movie
    RatingGate.fromShadow(ShadowDecisions.none)(stored, movie) shouldBe movie
    RatingGate.off(stored, movie) shouldBe movie
  }

  it should "change its version when a verdict could change, and the off gate never" in {
    def shadow(conf: Double) = new ShadowDecisions {
      def latest() = Seq(D(Set(listing), conf)); def verdicts() = Seq(Sample(0.5, correct = true))
    }
    RatingGate.fromShadow(shadow(0.1)).version should not be RatingGate.fromShadow(shadow(0.9)).version
    RatingGate.off.version shouldBe RatingGate.off.version
  }
}
