package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDate, Month}

/** When a seasonal venue is expected to be dark, and which venues the rosters say are seasonal. */
class OpeningSpec extends AnyFlatSpec with Matchers {

  private def at(date: String) = Instant.parse(s"${date}T12:00:00Z")

  "A season window" should "hold its months, both ends included, and wrap the new year when it must" in {
    val summer = SeasonWindow(Month.MAY, Month.SEPTEMBER)
    Month.values.filter(summer.contains).toSeq shouldBe Seq(Month.MAY, Month.JUNE, Month.JULY, Month.AUGUST, Month.SEPTEMBER)
    val winter = SeasonWindow(Month.NOVEMBER, Month.FEBRUARY)
    Month.values.filter(winter.contains).toSeq shouldBe Seq(Month.JANUARY, Month.FEBRUARY, Month.NOVEMBER, Month.DECEMBER)
  }

  it should "say when the season a day falls in opened" in {
    SeasonWindow.Germany.openedOn(LocalDate.of(2026, 7, 14)) shouldBe Some(LocalDate.of(2026, 5, 1))
    SeasonWindow.Germany.openedOn(LocalDate.of(2026, 10, 1)) shouldBe None
    SeasonWindow(Month.NOVEMBER, Month.FEBRUARY).openedOn(LocalDate.of(2027, 1, 10)) shouldBe Some(LocalDate.of(2026, 11, 1))
  }

  "Each country's typical season" should "be the one agreed for its open-air and drive-in screens" in {
    SeasonWindow.Germany shouldBe SeasonWindow(Month.MAY, Month.SEPTEMBER)
    SeasonWindow.Spain shouldBe SeasonWindow(Month.JUNE, Month.SEPTEMBER)
    SeasonWindow.UnitedStates shouldBe SeasonWindow(Month.APRIL, Month.OCTOBER)
    SeasonWindow.UnitedKingdom shouldBe SeasonWindow(Month.MAY, Month.SEPTEMBER)
    SeasonWindow.Poland shouldBe SeasonWindow(Month.MAY, Month.SEPTEMBER)
  }

  "A seasonal venue" should "be off season only outside its window; a year-round one never" in {
    val openAir = Opening.Seasonal(SeasonWindow.Germany)
    openAir.offSeasonAt(at("2026-04-30")) shouldBe true
    openAir.offSeasonAt(at("2026-05-01")) shouldBe false
    openAir.offSeasonAt(at("2026-09-30")) shouldBe false
    openAir.offSeasonAt(at("2026-10-01")) shouldBe true
    Opening.YearRound.offSeasonAt(at("2027-01-15")) shouldBe false
    openAir.seasonOpenedAt(at("2026-08-01")) shouldBe Some(Instant.parse("2026-05-01T00:00:00Z"))
    openAir.seasonOpenedAt(at("2026-12-01")) shouldBe None
    Opening.YearRound.seasonOpenedAt(at("2026-08-01")) shouldBe None
  }

  // Stratford's rooftop ended its programme on 09-27 with the window running to 09-30: a typical
  // window cannot say which day of its last month a venue closes, so the whole month may be dark.
  it should "count as possibly dark off its season and through its window's final month; a year-round one never" in {
    val openAir = Opening.Seasonal(SeasonWindow.Germany)                 // May..September
    openAir.mayBeDarkAt(at("2026-08-31")) shouldBe false
    openAir.mayBeDarkAt(at("2026-09-01")) shouldBe true
    openAir.mayBeDarkAt(at("2026-09-30")) shouldBe true
    openAir.mayBeDarkAt(at("2026-12-01")) shouldBe true
    openAir.mayBeDarkAt(at("2026-05-01")) shouldBe false
    Opening.Seasonal(SeasonWindow(Month.NOVEMBER, Month.FEBRUARY)).mayBeDarkAt(at("2027-02-10")) shouldBe true
    Opening.YearRound.mayBeDarkAt(at("2026-09-15")) shouldBe false
  }

  it should "put its season's halfway point midway between the window's first day and the day after it closes" in {
    val openAir = Opening.Seasonal(SeasonWindow.Germany)                 // 05-01 .. 10-01: 153 days
    openAir.seasonHalfwayAt(at("2026-06-10")) shouldBe Some(Instant.parse("2026-07-16T00:00:00Z"))
    openAir.seasonHalfwayAt(at("2026-12-01")) shouldBe None
    val winter  = Opening.Seasonal(SeasonWindow(Month.NOVEMBER, Month.FEBRUARY))  // 11-01 .. 03-01: 120 days
    winter.seasonHalfwayAt(at("2027-01-15")) shouldBe Some(Instant.parse("2026-12-31T00:00:00Z"))
    Opening.YearRound.seasonHalfwayAt(at("2026-08-01")) shouldBe None
  }

  private def venue(name: String): Cinema = Cinema.byDisplayName(name)

  "The rosters" should "make each country's open-air, summer and drive-in screens seasonal, in its own window" in {
    venue("Freiluftkino Kreuzberg").opening shouldBe Opening.Seasonal(SeasonWindow.Germany)
    venue("Autocine Getxo").opening shouldBe Opening.Seasonal(SeasonWindow.Spain)
    venue("Admiral Twin Drive-in Tulsa").opening shouldBe Opening.Seasonal(SeasonWindow.UnitedStates)
    RooftopFilmClubStratfordRoofEast.opening shouldBe Opening.Seasonal(SeasonWindow.UnitedKingdom)
  }

  it should "leave a regular cinema, and a drive-in that screens all year, year-round" in {
    venue("Kino Central Berlin").opening shouldBe Opening.YearRound
    venue("Drive In Autokino Essen").opening shouldBe Opening.YearRound
    venue("West Wind Glendale Drive-In").opening shouldBe Opening.YearRound
    RooftopFilmClubPeckhamBusseyBuilding.opening shouldBe Opening.YearRound
    KinoMuza.opening shouldBe Opening.YearRound
  }

  // A generated `seasonal` name no venue carries is one the generator and the roster disagree on.
  it should "find a rostered venue behind every generated seasonal name" in {
    val seasonal = Cinema.all.filter(_.opening != Opening.YearRound)
    seasonal.size shouldBe (GermanRosterData.seasonal.size + SpanishRosterData.seasonal.size + UsRosterData.seasonal.size + 1)
  }
}
