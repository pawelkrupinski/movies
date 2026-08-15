package scripts

import models.{Cinema, CinemaCityKinepolis, CinemaShowing, Helios, KinoMuza, Multikino, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ConsolidateSplitFilms.{Candidate, Move}

/**
 * The decision that re-files a split film's venues. Pinned here rather than exercised
 * through Mongo because this is the function that says which of two rows a cinema's
 * screenings end up under — the one thing in the script that can lose data.
 *
 * Shapes taken from prod, 2026-08-14: `tylkojednanoc|1961` (tmdbId 41050, "La notte",
 * 121 min) and `tylkojednanoc|2026` (tmdbId 1433367, 102 min), holding 67 of the same
 * venue slots.
 */
class ConsolidateSplitFilmsSpec extends AnyFlatSpec with Matchers {

  // Keyed as production keys a slot — `CinemaShowing(cinema, titleKey)`, never the bare
  // `Cinema`. A repair that keys on the venue removes nothing at all from a real row.
  private def venue(cinema: Cinema, runtime: Option[Int]): (Source, SourceData) =
    CinemaShowing(cinema, "tylkojednanoc") -> SourceData(title = Some("Tylko jedna noc"), runtimeMinutes = runtime)

  private def row(key: String, tmdbId: Int, ownRuntime: Int, venues: (Source, SourceData)*): Candidate =
    Candidate(key, Some(tmdbId), Some(ownRuntime), venues.toMap)

  private def slotOf(cinema: Cinema): Source = CinemaShowing(cinema, "tylkojednanoc")

  private val Romcom  = 1433367
  private val LaNotte = 41050

  "a venue held by both rows" should "move to the film its published runtime is nearest" in {
    val moves = ConsolidateSplitFilms.movesFor(Seq(
      row("tylkojednanoc|1961", LaNotte, 121, venue(Multikino, Some(105)), venue(KinoMuza, Some(121))),
      row("tylkojednanoc|2026", Romcom,  102, venue(Multikino, Some(105)))
    ))
    moves should contain(Move(slotOf(Multikino), "tylkojednanoc|2026", Seq("tylkojednanoc|1961")))
  }

  it should "leave a venue alone when only one row holds it" in {
    // Kino Muza sits on the 1961 row only, so there is nothing to de-duplicate there —
    // and its 121 minutes say it really is screening Antonioni.
    val moves = ConsolidateSplitFilms.movesFor(Seq(
      row("tylkojednanoc|1961", LaNotte, 121, venue(Multikino, Some(105)), venue(KinoMuza, Some(121))),
      row("tylkojednanoc|2026", Romcom,  102, venue(Multikino, Some(105)))
    ))
    moves.map(_.slot) should not contain slotOf(KinoMuza)
  }

  "a venue that published no runtime" should "go to the film more venues are screening" in {
    val moves = ConsolidateSplitFilms.movesFor(Seq(
      row("tylkojednanoc|1961", LaNotte, 121, venue(Multikino, None)),
      row("tylkojednanoc|2026", Romcom,  102, venue(Multikino, None), venue(Helios, Some(102)), venue(CinemaCityKinepolis, Some(102)))
    ))
    moves should contain(Move(slotOf(Multikino), "tylkojednanoc|2026", Seq("tylkojednanoc|1961")))
  }

  "two same-titled films that share no venue" should "be left completely alone" in {
    // A genuine remake pair. Nothing is duplicated, so nothing may move.
    ConsolidateSplitFilms.movesFor(Seq(
      row("diuna|1984", 100, 137, venue(KinoMuza, Some(137))),
      row("diuna|2021", 200, 155, venue(Multikino, Some(155)))
    )) shouldBe empty
  }

  "a group whose rows are all one film" should "produce no moves" in {
    ConsolidateSplitFilms.movesFor(Seq(
      row("tylkojednanoc|2026", Romcom, 102, venue(Multikino, Some(105))),
      row("tylkojednanoc|",     Romcom, 102, venue(Multikino, Some(105)))
    )) shouldBe empty
  }

  "a venue neither film's runtime fits, with the venue counts tied" should "not be moved at all" in {
    // No evidence either way: refuse rather than guess, and leave the corpus as it is.
    ConsolidateSplitFilms.movesFor(Seq(
      row("tylkojednanoc|1961", LaNotte, 121, venue(Multikino, None)),
      row("tylkojednanoc|2026", Romcom,  102, venue(Multikino, None))
    )) shouldBe empty
  }
}
