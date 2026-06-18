package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.GeneratedTitleRules
import services.titlerules.TitleRuleSet

/** Exercises the auto-dumped [[GeneratedTitleRules]] — the code mirror of the
 *  live prod `titleRules` set — so the suite runs EXACTLY what prod runs, not
 *  just the frozen seed. The scheduled `sync-title-rules` workflow refreshes the
 *  file and opens a PR when prod drifts; this spec then runs against the new
 *  rules automatically, so a banner that silently stops matching shows up here.
 *
 *  (The emitter that produces the file is pinned separately by
 *  [[scripts.DumpTitleRulesSpec]].) */
class GeneratedTitleRulesSpec extends AnyFlatSpec with Matchers {

  private val prod = TitleRuleSet(GeneratedTitleRules.all)

  "GeneratedTitleRules" should "have unique rule ids" in {
    val ids = GeneratedTitleRules.all.map(_.id)
    ids.diff(ids.distinct) shouldBe empty
  }

  it should "carry only valid regex patterns (a broken dump can't compile a bad regex away)" in {
    // Validity is judged on the placeholder-EXPANDED pattern — a rule may carry a
    // raw `{{SEP}}` token that only compiles once the placeholder is substituted,
    // and an unresolved/typo'd token is reported here too (see TitleRuleSet).
    prod.invalidRules.map(r => s"${r.id}: ${r.pattern}") shouldBe empty
  }

  it should "be a non-trivial set (guards against an empty/failed dump landing)" in {
    GeneratedTitleRules.all.size should be > 20
  }

  // The real prod rules on real decorated titles — strips to the bare film for
  // the external-API query while the screening keeps its banner row. Spans the
  // breadth of the prod set: seed programme prefix + accessibility, the curated
  // ExtraTitleRules cycles (Konesera / DKF / senior / przedpremiera), the
  // festival banners (WTF Fest / 6 razy Pedro / Kino cyrkularne / Fellini), and
  // the global trailing-year strip (the bare film, not the cinema's "(1957)").
  private val cases = Seq(
    "Kino bez barier: Freak Show (AD + CC + PJM)"           -> "Freak Show",
    "Klub Konesera: Ojczyzna"                               -> "Ojczyzna",
    "DKF Kropka: Riefenstahl"                               -> "Riefenstahl",
    "Pora dla Seniora: Ojczyzna"                            -> "Ojczyzna",
    "Filmoterapia z Inspirą: Drugie życie"                  -> "Drugie życie",
    "Ojczyzna | PRZEDPREMIERA"                              -> "Ojczyzna",
    "WTF Fest | Crash"                                      -> "Crash",
    "Ból i blask | 6 razy Pedro"                            -> "Ból i blask",
    "Lawrence z Arabii | Kino cyrkularne EXTRA"             -> "Lawrence z Arabii",
    "Federico Fellini: ciao a tutti! – Osiem i pół"         -> "Osiem i pół",
    // Fellini-suffix strips the banner, then the global year rule drops " (1957)".
    "Noce Cabirii (1957) | FEDERICO FELLINI: ciao a tutti!" -> "Noce Cabirii"
  )

  it should "strip every known prod banner for the external-API query" in {
    cases.foreach { case (in, film) =>
      withClue(s"search('$in'): ")(prod.search(in) shouldBe film)
    }
  }

  it should "leave a plain colon/word title untouched" in {
    Seq("Top Gun: Maverick", "Ojczyzna", "2001: Odyseja kosmiczna").foreach { t =>
      withClue(s"search('$t'): ")(prod.search(t) shouldBe t)
    }
  }
}
