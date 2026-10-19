package services.identity

import org.scalatest.funsuite.AnyFunSuite

class FilmNessSpec extends AnyFunSuite {
  private def listing(raw: String, year: Option[Int] = None) = IdentityMeasures.Listing(raw, year = year)

  test("a factless gig billing reads its event words and its other words") {
    val tanzwut = listing("Tanzwut - Live Tour 2026 | Special Guest: Sagenbringer")
    assert(tanzwut.eventWords == Seq("live", "tour"))
    assert(tanzwut.nonEventWords == Set("tanzwut", "special", "guest", "sagenbringer"))
  }

  test("a billing relaying a broadcast reads none") {
    assert(listing("NT Live: Hay Fever").eventWords.isEmpty)
  }
}

class FilmNessAcceptanceSpec extends AnyFunSuite {
  import IdentityMeasures.{Film, ListingFilm}
  private val calibration = IdentityCalibration.resolver
  private val acceptance  = new Acceptance(calibration)

  test("a gig's record found by its event words alone is not taken") {
    val gig  = IdentityMeasures.Listing("Tanzwut - Live Tour 2026 | Special Guest: Sagenbringer", originalTitle = Some("Number_i LIVE TOUR 2024 No.I 特別編集版"))
    val film = Film("Number_i LIVE TOUR 2024 No.I 特別編集版", Some("Number_i LIVE TOUR 2024 No.I 特別編集版"), year = Some(2025), runtime = Some(108))
    val measures = IdentityMeasures.listingFilm(gig, film, Some(1), 0, 0)
    val scored = Scored(Candidate(1419814, film), calibration.probability(ListingFilm, measures), measures, denial = None, gig, Some(1))
    info(s"eventWords=${gig.eventWords} other=${gig.nonEventWords} filmWords=${film.titles.flatMap(IdentityMeasures.countedWords)}")
    info(s"refusals=${acceptance.refusals(Seq(scored))}")
    assert(acceptance.aloneNamed(Seq(scored)).isEmpty)
  }
}
