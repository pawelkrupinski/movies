package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The settle's containment edge folds an unresolved spelling that decorates a resolved film's
 * title onto that film. It read the film's KEY spelling and TMDB aliases only, so whether it fired
 * depended on which of the film's own venue spellings happened to key its row: PL's "Rolling Loud.
 * Film" (1336166) was keyed by Cinema City's "Unlimited show - Rolling Loud. Film" on one day and by
 * "Rolling Loud. Film" the next, and Jaworzyna's "Rolling Loud. Film 2026" stood apart, then folded,
 * with its listings unchanged (PL convergence, recording 36584135207).
 */
class ContainmentBySlotSpellingSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.Poland)
  private def venue(name: String) = Cinema.byDisplayName(name)

  private def film(keyTitle: String) = CacheKey(keyTitle, Some(2026), normalizer) -> MovieRecord(tmdbId = Some(1336166), data = Map[Source, SourceData](
    Tmdb -> SourceData(title = Some("Rolling Loud: The Movie"), originalTitle = Some("Rolling Loud: The Movie"), releaseYear = Some(2026)),
    CinemaShowing.keyFor(venue("Cinema City Arkadia"), "Unlimited show - Rolling Loud. Film", normalizer) ->
      SourceData(title = Some("Unlimited show - Rolling Loud. Film"), releaseYear = Some(2026)),
    CinemaShowing.keyFor(venue("Kino Kijów"), "Rolling Loud. Film", normalizer) -> SourceData(title = Some("Rolling Loud. Film"))))

  private val jaworzyna = CacheKey("Rolling Loud. Film 2026", None, normalizer) -> MovieRecord(data = Map[Source, SourceData](
    CinemaShowing.keyFor(venue("Jaworzyna"), "Rolling Loud. Film 2026", normalizer) -> SourceData(title = Some("Rolling Loud. Film 2026"))))

  private def componentOf(rows: Seq[(CacheKey, MovieRecord)]) =
    FilmCanonicalizer.groupByFilm(rows, normalizer).find(_.exists(_._1 == jaworzyna._1)).map(_.map(_._1.cleanTitle).toSet)

  "a spelling that decorates a resolved film's venue spelling" should "join that film whichever spelling keys its row" in {
    componentOf(Seq(film("Rolling Loud. Film"), jaworzyna)) shouldBe Some(Set("Rolling Loud. Film", "Rolling Loud. Film 2026"))
    componentOf(Seq(film("Unlimited show - Rolling Loud. Film"), jaworzyna)) shouldBe
      Some(Set("Unlimited show - Rolling Loud. Film", "Rolling Loud. Film 2026"))
  }
}
