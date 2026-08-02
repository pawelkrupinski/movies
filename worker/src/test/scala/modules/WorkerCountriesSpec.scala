package modules

import models.Country
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.TitleNormalizer
import services.titlerules.TitleRuleSet

/**
 * The boot guard that keeps a worker off a country set it cannot normalise.
 *
 * `WorkerMain` builds one wiring per `KINOWO_COUNTRIES` entry and shares a
 * budget, a Mongo client and a metrics registry between them — so running
 * `pl,de,uk` in one JVM looks like a config flip. It isn't, because
 * `TitleNormalizer` resolves ONE rule set per process. The second half of this
 * spec proves what that fallback actually does to a German title, so the guard
 * reads as a recorded consequence rather than caution.
 */
class WorkerCountriesSpec extends AnyFlatSpec with Matchers with OptionValues {

  "unsupportedCountries" should "allow the single-country deploys we actually run" in {
    Seq(Country.Poland, Country.Germany, Country.UnitedKingdom).foreach { c =>
      withClue(s"${c.code}: ")(WorkerMain.unsupportedCountries(Seq(c)) shouldBe None)
    }
  }

  it should "refuse a worker asked to run several countries at once" in {
    WorkerMain.unsupportedCountries(Seq(Country.Poland, Country.Germany, Country.UnitedKingdom)) shouldBe defined
  }

  it should "refuse even a two-country pairing" in {
    WorkerMain.unsupportedCountries(Seq(Country.Poland, Country.Germany)) shouldBe defined
  }

  it should "name the offending countries so the log says which config was rejected" in {
    val why = WorkerMain.unsupportedCountries(Seq(Country.Poland, Country.Germany)).value
    why should include("KINOWO_COUNTRIES")
    why should include(Country.Poland.code)
    why should include(Country.Germany.code)
  }

  // ── why the guard exists ──────────────────────────────────────────────────
  //
  // `resolveCountries()` never returns empty, so the interesting input is >1.
  // These two pin the damage that would follow if it booted anyway.

  "a multi-country worker" should "have no sole country, so the normalizer falls back to Poland" in {
    Country.soleFrom(country = None, countries = Some("pl,de,uk")) shouldBe None
  }

  it should "key a German title with Poland's rules under that fallback" in {
    val fallback = TitleRuleSet.forCountry(
      Country.soleFrom(None, Some("pl,de,uk")).getOrElse(Country.default))
    // The exact 2026 incident: CinemaxX Würzburg's "Minions & Monster" stored and
    // served as "Minions i Monster". No German cinema slot can produce this key.
    TitleNormalizer.withRules(fallback) {
      TitleNormalizer.sanitize("Minions & Monster") shouldBe "minionsimonster"
    }
    // What the DE web tier computes for the same title — the disagreement that
    // makes this a permanent re-key rather than a cosmetic misspelling.
    TitleNormalizer.withRules(TitleRuleSet.forCountry(Country.Germany)) {
      TitleNormalizer.sanitize("Minions & Monster") shouldBe "minionsmonster"
    }
  }
}
