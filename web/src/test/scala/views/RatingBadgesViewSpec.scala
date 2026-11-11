package views

import models.MovieRecord
import services.readmodel.TestReadModel
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
    val rendered = views.html._ratingBadges(Some(TestReadModel.ratings("Some Film", MovieRecord(imdbRating = Some(7.0))))).body
    rendered should include ("rating-imdb-value\">7.0<")
  }

  it should "render a whole-number Filmweb score with one decimal" in {
    val rendered = views.html._ratingBadges(Some(TestReadModel.ratings("Some Film", MovieRecord(filmwebRating = Some(7.0))))).body
    rendered should include ("rating-fw-value\">7.0<")
  }

  it should "render a fractional score verbatim to one decimal" in {
    val rendered = views.html._ratingBadges(Some(TestReadModel.ratings("Some Film", MovieRecord(imdbRating = Some(7.7))))).body
    rendered should include ("rating-imdb-value\">7.7<")
  }

  it should "round a raw rating to one decimal, keeping the tenths place" in {
    // A real filmweb value rounds to 7.0 and must still show the ".0".
    val rendered = views.html._ratingBadges(Some(TestReadModel.ratings("Some Film", MovieRecord(filmwebRating = Some(6.97571))))).body
    rendered should include ("rating-fw-value\">7.0<")
  }

  // A source we hold a link for but no rating yet still gets its pill (owner, 2026-11-06).
  private def render(record: MovieRecord) =
    views.html._ratingBadges(Some(TestReadModel.ratings("Some Film", record))).body

  it should "render a rating-less Filmweb pill linking the stored page, labelled for assistive tech" in {
    val rendered = render(MovieRecord(filmwebUrl = Some("https://www.filmweb.pl/film/Przepis+na+swieta-2026-1")))
    rendered should include ("""href="https://www.filmweb.pl/film/Przepis+na+swieta-2026-1"""")
    rendered should include ("""title="Filmweb, no rating yet" aria-label="Filmweb, no rating yet"""")
    rendered should include ("<span class=\"rating-fw-label\">FW</span>")
    rendered should not include "rating-fw-value"
  }

  it should "render no Filmweb pill when only the derived search-page fallback exists" in {
    render(MovieRecord()) should not include "rating-fw"
  }

  it should "translate a rating-less pill's title and aria-label per language, leaving the visible text" in {
    val expected = Seq(
      "pl" -> ("IMDb – brak oceny", "Filmweb – brak oceny"),
      "de" -> ("IMDb – noch keine Bewertung", "Filmweb – noch keine Bewertung"),
      "en" -> ("IMDb, no rating yet", "Filmweb, no rating yet"),
      "es" -> ("IMDb – sin valoración todavía", "Filmweb – sin valoración todavía"))
    for ((lang, (imdb, filmweb)) <- expected) {
      val messages = testsupport.TestMessages.forLang(lang)
      val ratings  = TestReadModel.ratings("Some Film", MovieRecord(imdbId = Some("tt1"), filmwebUrl = Some("https://www.filmweb.pl/film/X-1")))
      val rendered = views.html._ratingBadges(Some(ratings), key => messages(key)).body
      rendered should include (s"""title="$imdb" aria-label="$imdb"""")
      rendered should include (s"""title="$filmweb" aria-label="$filmweb"""")
      rendered should include ("""data-i18n-attr="aria-label:rating.imdb.pending;title:rating.imdb.pending"""")
      rendered should not include "rating-imdb-value"
      rendered should not include "rating-fw-value"
    }
  }

  it should "render a rating-less IMDb pill linking the title page" in {
    val rendered = render(MovieRecord(imdbId = Some("tt1234567")))
    rendered should include ("""href="https://www.imdb.com/title/tt1234567/"""")
    rendered should include ("""title="IMDb, no rating yet" aria-label="IMDb, no rating yet"""")
    rendered should include ("<span class=\"rating-imdb-label\">IMDb</span>")
    rendered should not include "rating-imdb-value"
  }

  it should "leave a rated pill without the no-rating label" in {
    val rendered = render(MovieRecord(imdbId = Some("tt1"), imdbRating = Some(7.0), filmwebUrl = Some("https://www.filmweb.pl/film/X-1"), filmwebRating = Some(6.5)))
    rendered should not include "no rating yet"
    rendered should include ("rating-imdb-value\">7.0<")
    rendered should include ("rating-fw-value\">6.5<")
  }
}
