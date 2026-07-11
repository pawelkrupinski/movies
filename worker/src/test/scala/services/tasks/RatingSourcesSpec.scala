package services.tasks

import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Eligibility of the Filmweb rating source. Filmweb is un-gated for `tmdbId`-less
 * rows that carry a Filmweb URL: such a row can RESOLVE its tmdbId via
 * Filmweb→Wikidata (see [[services.movies.MovieService]]) and its rating refresh
 * reads that known URL directly (no fuzzy match, so no bogus rating). A
 * tmdbId-less row WITHOUT a Filmweb URL stays ineligible — nothing to refresh
 * from and nothing to resolve through — so the ~190 event/opera/NT-Live rows
 * aren't enqueued.
 */
class RatingSourcesSpec extends AnyFlatSpec with Matchers {

  private def filmwebEligible(row: MovieRecord): Boolean =
    RatingSources.all.find(_.taskType == TaskType.FilmwebRating).get.eligible(row)

  "the Filmweb rating source" should "be eligible for a tmdbId-less row that carries a Filmweb URL" in {
    filmwebEligible(MovieRecord(filmwebUrl = Some("https://www.filmweb.pl/film/X-2007-469205"))) shouldBe true
  }

  it should "stay eligible for a resolved row (unchanged)" in {
    filmwebEligible(MovieRecord(tmdbId = Some(603))) shouldBe true
  }

  it should "NOT be eligible for a tmdbId-less row with no Filmweb URL" in {
    filmwebEligible(MovieRecord()) shouldBe false
  }

  "forCountry" should "keep all four sources for a filmweb-enabled country (Poland)" in {
    RatingSources.forCountry(filmwebEnabled = true).map(_.taskType) should contain(TaskType.FilmwebRating)
    RatingSources.forCountry(filmwebEnabled = true) shouldBe RatingSources.all
  }

  it should "drop FilmwebRating for a filmweb-disabled country (UK/Germany)" in {
    val sources = RatingSources.forCountry(filmwebEnabled = false)
    sources.map(_.taskType) should not contain TaskType.FilmwebRating
    sources.map(_.taskType) should contain allOf (TaskType.ImdbRating, TaskType.RtRating, TaskType.McRating)
    sources should have size 3
  }
}
