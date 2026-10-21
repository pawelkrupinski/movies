package services.identity

import models.{KinoApollo, Multikino, Rialto}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

/** A rule the ablation harness switches off stops deciding, and the next rule in line decides instead. */
class RuleAblationSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  import FilmTable.{F, listing}

  private val lynch = Seq(F(1018, "Mulholland Drive", 2001, "David Lynch", 146), F(2, "Mulholland Falls", 1996, "Lee Tamahori", 107, 4))
  private def resolve(listings: Seq[Listing], films: Seq[F] = lynch) =
    IdentityResolver.resolve(listings, new FilmTable(films, normalizer), normalizer, IdentityCalibration.resolver)

  "a rule name" should "switch off that rule alone, or a family of them by a trailing *" in {
    RuleAblation.disabling(Set("veto:a-b", "accept:c*")) {
      RuleAblation.disables("veto:a-b") shouldBe true
      RuleAblation.disables("veto:a-b-c") shouldBe false
      RuleAblation.disables("accept:c-d") shouldBe true
    }
    RuleAblation.disables("veto:a-b") shouldBe false
  }

  "a veto switched off" should "no longer deny the film it vetoed" in {
    val wrongDirector = listing(Multikino, "Mulholland Drive", Some(1961), Some("Lee Tamahori"), Some(62))
    val vetoes = resolve(Seq(wrongDirector), lynch.take(1)).decisionOf(wrongDirector.key).trace.rulesOf(wrongDirector.key).filter(_.startsWith("veto:"))
    vetoes should not be empty
    val ablated = RuleAblation.disabling(vetoes.toSet)(resolve(Seq(wrongDirector), lynch.take(1))).decisionOf(wrongDirector.key)
    withClue(ablated.render)(ablated.trace.rulesOf(wrongDirector.key).filter(vetoes.contains) shouldBe empty)
  }

  "an acceptance rule switched off" should "leave the listing to the next rule" in {
    val credited = listing(Rialto, "Mulholland Drive", Some(2001), Some("David Lynch"), Some(146))
    val accepted = resolve(Seq(credited)).decisionOf(credited.key).trace.rulesOf(credited.key).filter(_.startsWith("accept:"))
    accepted should have size 1
    val ablated = RuleAblation.disabling(accepted.toSet)(resolve(Seq(credited))).decisionOf(credited.key)
    withClue(ablated.render)(ablated.trace.rulesOf(credited.key) should not contain accepted.head)
  }

  "a must-link switched off" should "no longer join two listings by it" in {
    val credited = listing(Rialto, "Mulholland Drive", Some(2001), Some("David Lynch"), Some(146))
    val bare     = listing(KinoApollo, "Mulholland Drive")
    resolve(Seq(credited, bare)).decisionOf(bare.key).trace.rulesOf(bare.key) should contain ("join:same-film")
    val ablated = RuleAblation.disabling(Set("join:same-film"))(resolve(Seq(credited, bare))).decisionOf(bare.key)
    withClue(ablated.render)(ablated.trace.rulesOf(bare.key) should not contain "join:same-film")
  }
}
