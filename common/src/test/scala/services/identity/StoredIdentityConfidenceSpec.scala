package services.identity

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.StoredMovieRecord
import services.readmodel.ReadModelProjection

/** The rating gate's confidence in a STORED film's TMDB identity, read off the row's own
 *  evidence by the production calibration artefact (`identity-weights.json`). The rows are the
 *  shapes production stores for real title-only matches, right and wrong. */
class StoredIdentityConfidenceSpec extends AnyFlatSpec with Matchers {

  private val calibration = IdentityCalibration.default
  private val gate        = RatingGate.fromEvidence(calibration)

  private def tmdb(title: String, original: String, year: Int, runtime: Int, director: String, country: String,
                   english: Option[String] = None) =
    SourceData(title = Some(title), originalTitle = Some(original), englishTitle = english, releaseYear = Some(year),
      runtimeMinutes = Some(runtime), director = Seq(director), countries = Seq(country))

  private def row(tmdbSlot: SourceData, venues: (Cinema, SourceData)*): StoredMovieRecord =
    StoredMovieRecord.synthesised(tmdbSlot.title.get, tmdbSlot.releaseYear, MovieRecord(
      imdbRating = Some(6.8), imdbId = Some("tt1"), filmwebRating = Some(6.7), rottenTomatoes = Some(60), tmdbId = Some(1),
      data = venues.toMap[Source, SourceData] + (Tmdb -> tmdbSlot)), titleNormalizer)

  private def confidence(stored: StoredMovieRecord): Double =
    StoredIdentityConfidence.of(stored.record, calibration).get

  private def shown(stored: StoredMovieRecord): Boolean = {
    val movie = ReadModelProjection.resolve(stored, titleNormalizer)
    gate(stored, movie) match {
      case same if same == movie                     => true
      case gated if gated == RatingGate.withheld(movie) => false
      case other                                     => fail(s"neither the card nor its withheld form: $other")
    }
  }

  // Kino 1410's Met Opera relay, filed under Cecil B. DeMille's 1949 film by a unique title hit.
  private val samson = row(tmdb("Samson i Dalia", "Samson and Delilah", 1949, 131, "Cecil B. DeMille", "USA",
      english = Some("Samson and Delilah")),
    Kino1410 -> SourceData(title = Some("Samson i dalila | metropolitan opera: live in hd 2026/27"),
      rawTitle = Some("Samson i dalila | metropolitan opera: live in hd 2026/27")))

  "a title-only match no venue's facts back" should "have its ratings withheld" in {
    confidence(samson) should be < 0.36
    calibration.showsRatings(confidence(samson)) shouldBe false
    shown(samson) shouldBe false
  }

  private val backToTheFuture3 = {
    def venue = SourceData(title = Some("Zurück in die Zukunft III"), rawTitle = Some("Zurück in die Zukunft III"),
      originalTitle = Some("Back to the Future Part III"), releaseYear = Some(1990), runtimeMinutes = Some(118),
      director = Seq("Robert Zemeckis"))
    row(tmdb("Zurück in die Zukunft III", "Back to the Future Part III", 1990, 118, "Robert Zemeckis", "Vereinigte Staaten"),
      KinoIluzjon -> venue, KinoAmondo -> venue)
  }

  "a title-only match its venues corroborate by year, runtime and director" should "keep its ratings" in {
    calibration.showsRatings(confidence(backToTheFuture3)) shouldBe true
    shown(backToTheFuture3) shouldBe true
  }

  private val donnieDarko = tmdb("Donnie Darko", "Donnie Darko", 2001, 113, "Richard Kelly", "USA", english = Some("Donnie Darko"))
  private def replay = SourceData(title = Some("Donnie Darko"), rawTitle = Some("Donnie Darko w Helios RePlay"),
    originalTitle = Some("Donnie Darko"), runtimeMinutes = Some(117))

  it should "keep them when one venue states the year and director and the rest only a runtime" in {
    val stored = row(donnieDarko, HeliosBiala -> replay, HeliosBlueCity -> replay,
      KinoIluzjon -> SourceData(title = Some("Donnie Darko"), originalTitle = Some("Donnie Darko"), releaseYear = Some(2001),
        runtimeMinutes = Some(117), director = Seq("Richard Kelly"), countries = Seq("USA", "Kanada")))
    shown(stored) shouldBe true
  }

  it should "keep them on a venue's runtime and original title alone (a detail page's facts)" in {
    shown(row(donnieDarko, HeliosBiala -> replay)) shouldBe true
  }

  "a film one venue lists by its exact title and nothing else" should "have its ratings withheld: an exact title alone is what a namesake also has" in {
    // Neither corroborated nor contradicted. Without the search that concluded it (not stored),
    // the calibration's probability for an exact title and no other fact is below the display
    // threshold — the same evidence the Samson relay had against a namesake.
    val bare = row(tmdb("Zawieście czerwone latarnie", "大红灯笼高高挂", 1991, 125, "张艺谋", "Chiny"),
      KinoAmondo -> SourceData(title = Some("Zawieście czerwone latarnie"), rawTitle = Some("Zawieście czerwone latarnie")))
    calibration.showsRatings(confidence(bare)) shouldBe false
    shown(bare) shouldBe false
  }

  it should "keep them once a second venue publishes the year and runtime, even under a different director script" in {
    val corroborated = row(tmdb("Zawieście czerwone latarnie", "大红灯笼高高挂", 1991, 125, "张艺谋", "Chiny"),
      KinoAmondo -> SourceData(title = Some("Zawieście czerwone latarnie")),
      KinoPort -> SourceData(title = Some("Zawieście czerwone latarnie"), originalTitle = Some("Da hong deng long gao gao gua"),
        releaseYear = Some(1991), runtimeMinutes = Some(125), director = Seq("Zhang Yimou")))
    shown(corroborated) shouldBe true
  }

  "a row with no TMDB record" should "have no confidence, and keep whatever ratings it has" in {
    val noTmdb = samson.copy(record = samson.record.copy(tmdbId = None, data = samson.record.data - Tmdb))
    StoredIdentityConfidence.of(noTmdb.record, calibration) shouldBe None
    shown(noTmdb) shouldBe true
  }

  "the stored-evidence measures" should "leave out what only the concluding search knew" in {
    val m = StoredIdentityConfidence.measures(StoredIdentityConfidence.film(samson.record.data(Tmdb)), "Kino 1410",
      StoredIdentityConfidence.listing(samson.record.data(Kino1410)), Nil)
    m.keySet intersect IdentityMeasures.RankingPriors shouldBe empty
    m.keySet should contain allOf ("title", "year.distance", "director", "runtime.delta", "venues.corroborating")
  }

  "the gate's version" should "follow the calibration artefact" in {
    RatingGate.fromEvidence(calibration).version shouldBe RatingGate.fromEvidence(calibration).version
    RatingGate.fromEvidence(calibration.copy(version = "other")).version should not be RatingGate.fromEvidence(calibration).version
    RatingGate.fromEvidence(calibration).version should not be RatingGate.off.version
  }
}
