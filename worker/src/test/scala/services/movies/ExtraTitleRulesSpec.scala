package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ExtraTitleRules
import services.titlerules.{TitleRuleDefaults, TitleRuleSet}

/** Locks the behaviour of the post-baseline [[ExtraTitleRules]] before they're
 *  applied to prod. The "load-bearing" pair on each case is the gate: the seed
 *  rules ALONE must NOT strip the banner (fail-before), and seed + extras MUST
 *  (pass-after) — so a rule that silently stops matching is caught here, not in
 *  prod.
 *
 *  `withExtras` is what prod runs once `ApplyExtraTitleRules` lands (seed rules
 *  + the additions); `seedOnly` is today's behaviour. */
class ExtraTitleRulesSpec extends AnyFlatSpec with Matchers {

  private val withExtras = TitleRuleSet(TitleRuleDefaults.all ++ ExtraTitleRules.all)
  private val seedOnly   = TitleRuleDefaults.ruleSet

  // ── programme prefixes: extracted as own row (display), stripped for query ──

  private val programmeCases = Seq(
    "Klub Konesera: Ojczyzna"                         -> ("Klub Konesera: ",                  "Ojczyzna"),
    "Kino Konesera: Drugie życie"                     -> ("Kino Konesera: ",                  "Drugie życie"),
    "KINO SENIORA | Ojczyzna"                         -> ("KINO SENIORA | ",                  "Ojczyzna"),
    "Pora dla Seniora: Ojczyzna"                      -> ("Pora dla Seniora: ",               "Ojczyzna"),
    "Wtorki dla Seniora: Młode matki"                 -> ("Wtorki dla Seniora: ",             "Młode matki"),
    "Filmowy Klub Seniora i Seniorki: OJCZYZNA"       -> ("Filmowy Klub Seniora i Seniorki: ", "OJCZYZNA"),
    "DKF Kropka: Riefenstahl"                         -> ("DKF Kropka: ",                     "Riefenstahl"),
    "DKF Człowiek w Zagrożeniu: Pociągi"              -> ("DKF Człowiek w Zagrożeniu: ",      "Pociągi"),
    "Klub Filmowy Urania: Młode matki"               -> ("Klub Filmowy Urania: ",            "Młode matki"),
    "Klub Filmowy Żółty Fotel: Chronologia wody"     -> ("Klub Filmowy Żółty Fotel: ",       "Chronologia wody"),
    "Kino Dostępne: Drugie życie"                     -> ("Kino Dostępne: ",                  "Drugie życie"),
    "Filmoterapia z Inspirą: Drugie życie"            -> ("Filmoterapia z Inspirą: ",         "Drugie życie"),
    "Portret Kobiety: Takie jest życie"               -> ("Portret Kobiety: ",               "Takie jest życie")
  )

  "ExtraTitleRules programme prefixes" should "extract the banner for the display row" in {
    programmeCases.foreach { case (in, (banner, _)) =>
      withClue(s"programmePrefix('$in'): ")(withExtras.programmePrefix(in) shouldBe Some(banner))
    }
  }

  it should "strip the banner for the external-API query" in {
    programmeCases.foreach { case (in, (_, query)) =>
      withClue(s"search('$in'): ")(withExtras.search(in) shouldBe query)
    }
  }

  it should "be load-bearing — the seed rules alone leave the banner in place" in {
    programmeCases.foreach { case (in, _) =>
      withClue(s"seedOnly.search('$in') should be unchanged: ")(seedOnly.search(in) shouldBe in)
      withClue(s"seedOnly.programmePrefix('$in'): ")(seedOnly.programmePrefix(in) shouldBe None)
    }
  }

  // ── search-only strips: row kept, query fixed ──────────────────────────────

  private val searchStripCases = Seq(
    "Ojczyzna_DKF"                     -> "Ojczyzna",
    "Czytając Lolitę w Teheranie | DKF" -> "Czytając Lolitę w Teheranie",
    "OJCZYZNA - DKF KOT"               -> "OJCZYZNA",
    "DRUGIE ŻYCIE - DKF III W"         -> "DRUGIE ŻYCIE",
    "Pociągi - dyskusyjny klub filmowy" -> "Pociągi",
    "Ojczyzna | PRZEDPREMIERA"         -> "Ojczyzna",
    "Takie jest życie - przedpremiera" -> "Takie jest życie",
    "Drugie życie | przedpremierowo"   -> "Drugie życie",
    "Przedpremiera | Ojczyzna"         -> "Ojczyzna",
    "PRZEDPREMIERA: Wielki łuk"        -> "Wielki łuk",
    "Młode matki *AD"                  -> "Młode matki"
  )

  "ExtraTitleRules search strips" should "strip the marker for the external-API query" in {
    searchStripCases.foreach { case (in, query) =>
      withClue(s"search('$in'): ")(withExtras.search(in) shouldBe query)
    }
  }

  it should "be load-bearing — the seed rules alone leave the marker in place" in {
    searchStripCases.foreach { case (in, _) =>
      withClue(s"seedOnly.search('$in') should be unchanged: ")(seedOnly.search(in) shouldBe in)
    }
  }

  // ── negative controls: real titles must survive untouched ──────────────────

  private val untouched = Seq(
    "Top Gun: Maverick",
    "Mufasa: Król Lew",
    "Diabeł ubiera się u Prady 2",
    "Ojczyzna",
    "2001: Odyseja kosmiczna"
  )

  it should "never touch a plain colon/word title" in {
    untouched.foreach { t =>
      withClue(s"search('$t'): ")(withExtras.search(t) shouldBe t)
      withClue(s"structural('$t'): ")(withExtras.structural(t) shouldBe t)
      withClue(s"programmePrefix('$t'): ")(withExtras.programmePrefix(t) shouldBe None)
    }
  }
}
