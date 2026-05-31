package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MovieRecord.weightedRating` — the sort key behind the grid's "Ocena"
 * order. Each of the four external ratings is normalised to a 0–10 scale
 * (Metacritic + Rotten Tomatoes are 0–100, so they're ÷10) and the present
 * ones are averaged with equal weight. Missing sources are skipped; a record
 * with no ratings at all scores exactly 0.
 */
class MovieRecordWeightedRatingSpec extends AnyFlatSpec with Matchers {

  "weightedRating" should "average all four sources on a common 0–10 scale" in {
    // imdb 8.0, filmweb 7.0, metascore 80→8.0, RT 90→9.0 ⇒ 32/4 = 8.0
    val rec = MovieRecord(
      imdbRating     = Some(8.0),
      filmwebRating  = Some(7.0),
      metascore      = Some(80),
      rottenTomatoes = Some(90)
    )
    rec.weightedRating shouldBe 8.0 +- 1e-9
  }

  it should "skip missing sources and average only the present ones" in {
    // imdb 8.0 + RT 60→6.0 ⇒ 14/2 = 7.0 (filmweb + metascore absent)
    val rec = MovieRecord(imdbRating = Some(8.0), rottenTomatoes = Some(60))
    rec.weightedRating shouldBe 7.0 +- 1e-9
  }

  it should "normalise a lone Metacritic score by dividing by 10" in {
    MovieRecord(metascore = Some(50)).weightedRating shouldBe 5.0 +- 1e-9
  }

  it should "normalise a lone Rotten Tomatoes percentage by dividing by 10" in {
    MovieRecord(rottenTomatoes = Some(73)).weightedRating shouldBe 7.3 +- 1e-9
  }

  it should "pass an IMDb / Filmweb rating through unscaled" in {
    MovieRecord(imdbRating = Some(6.4)).weightedRating shouldBe 6.4 +- 1e-9
    MovieRecord(filmwebRating = Some(7.8)).weightedRating shouldBe 7.8 +- 1e-9
  }

  it should "score 0 when the record has no ratings at all" in {
    MovieRecord().weightedRating shouldBe 0.0
  }
}
