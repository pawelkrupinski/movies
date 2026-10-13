package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.JudgeEvidence.{Side, suggest}

/** The judge cards' suggestions ([[JudgeEvidence.suggest]]): a verdict only where the evidence decides it. */
class JudgeEvidenceSpec extends AnyFlatSpec with Matchers {

  private def side(year: Option[Int] = None, runtime: Option[Int] = None, directors: Seq[String] = Nil) = Side("t", None, year, runtime, directors)
  private val taken = Set("tmdb:1434460")

  "a judge card" should "suggest WRONG for a 2025 short taken for the extended Return of the King (DE CineStar, 10-13)" in {
    val s = suggest(side(Some(2003), Some(263), Seq("Peter Jackson")), Some(side(Some(2025), Some(14), Seq("Someone Else"))), Nil, taken)
    s.verdict shouldBe Some(false)
    s.reasons shouldBe Seq("a short against a feature: runtime 263 vs 14 min")
  }

  it should "suggest RIGHT when the year agrees within one and a director is shared, however the name is written" in {
    suggest(side(Some(2003), Some(201), Seq("JACKSON, Peter")), Some(side(Some(2003), Some(201), Seq("Peter Jackson"))), Nil, taken).verdict shouldBe Some(true)
    suggest(side(Some(2024), None, Seq("Agnieszka Holland")), Some(side(Some(2023), None, Seq("Agnieszka Holland"))), Nil, taken).verdict shouldBe Some(true)
  }

  it should "suggest WRONG for a record running more than 30% longer than the venue's slot, and keep an extended cut's 25% open" in {
    suggest(side(runtime = Some(90)), Some(side(runtime = Some(140))), Nil, taken).verdict shouldBe Some(false)
    suggest(side(runtime = Some(160)), Some(side(runtime = Some(201))), Nil, taken).verdict shouldBe None
  }

  it should "leave a slot longer than the film open: an event's extras (UK Purbeck Film Festival - Little Amelie, 150 vs 77, 10-13)" in {
    suggest(side(runtime = Some(150)), Some(side(runtime = Some(77))), Nil, taken).verdict shouldBe None
  }

  it should "only lean wrong on a record dated after the year billed, and not at all for a stage relay" in {
    // DE "Pettersson und Findus Mitmachkino 2": billed 2024, the 2026 event's record labelled right
    suggest(side(Some(2024)), Some(side(Some(2026))), Nil, taken) shouldBe
      JudgeEvidence.Suggestion(None, Seq("leans wrong: the record (2026) is dated after the year billed (2024)"))
    // UK MetOpera: La Fanciulla del West: the 2018 staging billed, the 2027 relay's record labelled right
    suggest(side(Some(2018)), Some(side(Some(2027))), Nil, taken, broadcast = true) shouldBe JudgeEvidence.Suggestion(None, Nil)
  }

  it should "leave a re-release open: a venue billing this year's revival of an old film contradicts nothing" in {
    suggest(side(Some(2026)), Some(side(Some(1968), directors = Seq("Wojciech Has"))), Nil, taken).verdict shouldBe None
  }

  it should "suggest WRONG when both the year and the directors disagree, and leave a director mismatch alone open" in {
    suggest(side(Some(2010), directors = Seq("A Smith")), Some(side(Some(1990), directors = Seq("B Jones"))), Nil, taken).verdict shouldBe Some(false)
    suggest(side(Some(2010), directors = Seq("A Smith")), Some(side(Some(2010), directors = Seq("B Jones"))), Nil, taken).verdict shouldBe None
  }

  it should "read the venue's own catalogue id: its film is right, another of the same database wrong, another database's neither" in {
    suggest(side(), None, Seq("tmdb:1434460"), taken).verdict shouldBe Some(true)
    suggest(side(), None, Seq("tmdb:122"), taken).verdict shouldBe Some(false)
    suggest(side(), None, Seq("filmweb:1"), taken).verdict shouldBe None
  }

  it should "stay OPEN when a reason for and one against meet, or there is nothing to compare" in {
    val conflict = suggest(side(Some(2003), Some(263), Seq("Peter Jackson")), Some(side(Some(2003), Some(14), Seq("Peter Jackson"))), Nil, taken)
    conflict.verdict shouldBe None
    conflict.reasons.forall(_.startsWith("conflict: ")) shouldBe true
    suggest(side(Some(2003)), None, Nil, taken) shouldBe JudgeEvidence.Suggestion(None, Seq("no record to compare"))
  }

  "a judge card" should "show why the suspect-take audit suspects its take, and the record beside the listing" in {
    val take = tools.UnmatchedClusters.Take("de", "CineStar Augsburg", "Herr der Ringe - Rückkehr des Königs (ext.)", Some(1434460), None, "Filled",
      "Herr der Ringe (2025)", explanation = Seq("filled by venues.current: 'Herr der Ringe (2025)'"))
    val card = IdentityJudgeCards.Card("de", take.rawTitle, "tmdb:1434460", Seq(take), side(runtime = Some(263)),
      Some(Side("Herr der Ringe", None, Some(2025), Some(14), Nil)), None, Nil, None, everyVenue = true,
      suspicions = Seq("runtime: a 14-min short billed at feature length"))
    val page = IdentityJudgeCards.markdown(card)
    page should include("**suggest WRONG** — a short against a feature: runtime 263 vs 14 min")
    page should include("suspect — runtime: a 14-min short billed at feature length")
    page should include("| runtime | 263 | 14 |")
    page should include("why taken: filled by venues.current")
  }
}
