package services.movies

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The determinism specs compare whole records, so their failure message has to
 * be able to name ANY field. The printer this replaces named three, and a real
 * failure on 2026-08-31 diverged in none of them — reporting "9 field diffs"
 * whose every printed pair was identical, which said nothing about the cause.
 */
class MovieRecordDiffSpec extends AnyFlatSpec with Matchers {

  "the record diff" should "name a field the old three-field printer never looked at" in {
    val a = MovieRecord(imdbRating = Some(7.1))
    val b = MovieRecord(imdbRating = Some(7.4))

    val described = MovieRecordDiff.describe(a, b, 2)
    described should include ("imdbRating")
    described should include ("0=Some(7.1)")
    described should include ("2=Some(7.4)")
  }

  it should "name every differing field, not just the first" in {
    val a = MovieRecord(tmdbId = Some(1), tmdbNoMatch = false, searchTitle = Some("a"))
    val b = MovieRecord(tmdbId = Some(2), tmdbNoMatch = true, searchTitle = Some("b"))

    val described = MovieRecordDiff.describe(a, b, 1)
    described should include ("tmdbId")
    described should include ("tmdbNoMatch")
    described should include ("searchTitle")
  }

  it should "stay silent about the fields that match" in {
    val a = MovieRecord(imdbId = Some("tt1"), metascore = Some(60))
    val b = MovieRecord(imdbId = Some("tt1"), metascore = Some(61))

    val described = MovieRecordDiff.describe(a, b, 1)
    described should include ("metascore")
    // The noise that made the old message unreadable: nine lines of fields whose
    // two sides were equal.
    described should not include ("imdbId")
  }

  it should "say so plainly when the records differ outside their fields" in {
    val same = MovieRecord(tmdbId = Some(7))
    MovieRecordDiff.describe(same, same, 1) should include ("outside the constructor")
  }
}
