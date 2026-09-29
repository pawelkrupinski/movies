package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A yearless straggler folds onto a group's one resolved film unless its own evidence
 * contradicts that film (`FilmCanonicalizer`'s rule 4). The year it brackets in its TITLE is
 * such evidence — the only year a repertory listing like Cultplex's "It (1990)" gives. Read as
 * slot `releaseYear`s alone, Tommy Lee Wallace's 168-minute "It (1990)" folded onto
 * Muschietti's 2017 film, whose 135 minutes are within the runtime check's plausible range
 * (UK convergence on recording 36584135207, 2026-09-29).
 */
class StragglerBracketYearSpec extends AnyFlatSpec with Matchers {

  private val normalizer = TitleNormalizer.forCountry(Country.UnitedKingdom)
  private val cultplex   = Cinema.byDisplayName("Cultplex Manchester")
  private val showcase   = Cinema.byDisplayName("Showcase Bristol Avonmeads")

  private val muschietti = CacheKey("It", Some(2017), normalizer) -> MovieRecord(tmdbId = Some(346364), data = Map[Source, SourceData](
    Tmdb -> SourceData(title = Some("It"), releaseYear = Some(2017), runtimeMinutes = Some(135), director = Seq("Andy Muschietti")),
    CinemaShowing.keyFor(showcase, "It", normalizer) -> SourceData(title = Some("It"), rawTitle = Some("It"))))

  private def wallace(runtime: Int) = CacheKey("It (1990)", None, normalizer) -> MovieRecord(data = Map[Source, SourceData](
    CinemaShowing.keyFor(cultplex, "It (1990)", normalizer) -> SourceData(title = Some("It (1990)"), rawTitle = Some("It (1990)"),
      runtimeMinutes = Some(runtime), director = Seq("Tommy Lee Wallace"))))

  private def clusters(rows: Seq[(CacheKey, MovieRecord)]) =
    FilmCanonicalizer.groupByFilm(rows, normalizer).flatMap(FilmCanonicalizer.clusterByFilm(_, normalizer)).map(_.map(_._1.cleanTitle).toSet)

  "a yearless straggler whose title brackets another year" should "stay apart from the group's resolved film" in {
    clusters(Seq(muschietti, wallace(168))) should contain allOf (Set("It"), Set("It (1990)"))
  }

  it should "still fold when its runtime agrees with the film: a rerelease bracketing its screening year" in {
    clusters(Seq(muschietti, wallace(135).copy(_2 = wallace(135)._2.copy(data = wallace(135)._2.data.view.mapValues(_.copy(director = Nil)).toMap))))
      .filter(_.contains("It (1990)")) shouldBe Seq(Set("It", "It (1990)"))
  }

  // Once Cultplex's slot had sat on the 2017 row — in one arrival order — the row RETAINED its
  // synopsis, and every later Cultplex "It (1990)" row shared that "duplicate listing" (same venue,
  // same blurb) and was reclaimed onto 2017 for good, whatever its own year and director said.
  "a year-bearing orphan sharing a retained synopsis with a resolved film" should
    "stay apart when its own year and director deny that film" in {
    val blurb    = "A 1990 miniseries: seven outcasts confront the evil that terrorises Derry."
    val retained = muschietti.copy(_2 = muschietti._2.copy(retainedSynopses = Map[Source, String](CinemaShowing.keyFor(cultplex, "It (1990)", normalizer) -> blurb)))
    val orphan   = CacheKey("It (1990)", Some(1990), normalizer) -> MovieRecord(data = Map[Source, SourceData](
      CinemaShowing.keyFor(cultplex, "It (1990)", normalizer) -> SourceData(title = Some("It (1990)"), rawTitle = Some("It (1990)"),
        runtimeMinutes = Some(168), director = Seq("Tommy Lee Wallace"), synopsis = Some(blurb))))
    clusters(Seq(retained, orphan)) should contain allOf (Set("It"), Set("It (1990)"))
  }
}
