package services.movies

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The consequence of an ambiguous country config (see `Country.ambiguousFrom`):
 * a process that cannot name ONE country is refused a rule set rather than
 * quietly handed Poland's. That silent fallback is what stored CinemaxX
 * Würzburg's "Minions & Monster" as "Minions i Monster" and served it to German
 * users under `minionsimonster` — a key no German cinema slot can produce.
 */
class DeploymentRulesSpec extends AnyFlatSpec with Matchers {

  "the deployment rule set" should "serve Poland when nothing is configured" in {
    // A dev box or a spec: not an ambiguity, so the default still applies.
    TitleNormalizer.rulesFor(Nil, None).sanitize("Minions & Monster") shouldBe "minionsimonster"
  }

  it should "serve the sole country's rules when exactly one is configured" in {
    TitleNormalizer.rulesFor(Nil, Some(Country.Germany)).sanitize("Minions & Monster") shouldBe "minionsmonster"
  }

  it should "refuse a process configured for several countries, naming them" in {
    // Fail-before/pass-after: this used to hand back Poland's rules and key the
    // German title `minionsimonster`.
    val why = the[RuntimeException] thrownBy
      TitleNormalizer.rulesFor(List(Country.Poland, Country.Germany, Country.UnitedKingdom), None)
    why.getMessage should include("KINOWO_COUNTRIES")
    Seq("pl", "de", "uk").foreach(code => why.getMessage should include(code))
  }
}
