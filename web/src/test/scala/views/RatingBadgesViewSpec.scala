package views

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `_ratingBadges` score formatting. The bug this guards: a whole-number IMDb /
 * Filmweb score (7.0) must render "7.0", not "7" — keeping the web in step with
 * the iOS + Android pills. The template uses `f"$r%.1f"`, which already pins one
 * decimal; this locks it so a refactor to `r.toString` can't regress it.
 */
class RatingBadgesViewSpec extends AnyFlatSpec with Matchers {

  "_ratingBadges" should "render a whole-number IMDb score with one decimal" in {
    val rendered = views.html._ratingBadges(Some(MovieRecord(imdbRating = Some(7.0))), "Some Film").body
    rendered should include ("rating-imdb-value\">7.0<")
  }

  it should "render a whole-number Filmweb score with one decimal" in {
    val rendered = views.html._ratingBadges(Some(MovieRecord(filmwebRating = Some(7.0))), "Some Film").body
    rendered should include ("rating-fw-value\">7.0<")
  }

  it should "render a fractional score verbatim to one decimal" in {
    val rendered = views.html._ratingBadges(Some(MovieRecord(imdbRating = Some(7.7))), "Some Film").body
    rendered should include ("rating-imdb-value\">7.7<")
  }

  it should "round a raw rating to one decimal, keeping the tenths place" in {
    // A real filmweb value rounds to 7.0 and must still show the ".0".
    val rendered = views.html._ratingBadges(Some(MovieRecord(filmwebRating = Some(6.97571))), "Some Film").body
    rendered should include ("rating-fw-value\">7.0<")
  }
}
