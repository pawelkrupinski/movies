package services.tasks

import models.{Country, MovieRecord}
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

  private def taskTypes(country: Country): Set[TaskType] =
    RatingSources.forCountry(country).map(_.taskType).toSet

  "RatingSources.forCountry" should "include Filmweb for a Filmweb-enabled country (Poland)" in {
    taskTypes(Country.Poland) should contain(TaskType.FilmwebRating)
  }

  it should "DROP Filmweb for a country that does not enable it (UK) — its handler is unwired there, so an enqueued task would pile up handler-less forever" in {
    taskTypes(Country.UnitedKingdom) should not contain TaskType.FilmwebRating
  }

  it should "keep the global rating sources for every country" in {
    val global = Set[TaskType](TaskType.ImdbRating, TaskType.RtRating, TaskType.McRating)
    taskTypes(Country.Poland) should contain allElementsOf global
    taskTypes(Country.UnitedKingdom) should contain allElementsOf global
  }
}
