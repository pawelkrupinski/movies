package services.identity

import org.scalatest.funsuite.AnyFunSuite

class FilmNessSpec extends AnyFunSuite {
  private def listing(raw: String, year: Option[Int] = None) = IdentityMeasures.Listing(raw, year = year)

  test("a factless gig billing reads its event words and its other words") {
    val tanzwut = listing("Tanzwut - Live Tour 2026 | Special Guest: Sagenbringer")
    assert(tanzwut.eventWords == Seq("live", "tour"))
    assert(tanzwut.nonEventWords == Set("tanzwut", "special", "guest", "sagenbringer"))
  }

  test("a billing stating a film's fact, or relaying a broadcast, reads none") {
    assert(listing("Tanzwut - Live Tour 2026", year = Some(2026)).eventWords.isEmpty)
    assert(listing("NT Live: Hay Fever").eventWords.isEmpty)
  }
}
