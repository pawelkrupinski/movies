package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rule contract as the population walk holds it along one listing's populations
 *  ([[PopulationMonotonicitySpec.contractBreaches]]): only a non-monotone rule may go back. */
class RuleContractWalkSpec extends AnyFlatSpec with Matchers {
  import PopulationMonotonicitySpec.{Decided, contractBreaches}

  private val alongChain = (held: Seq[(Int, String)]) => PopulationMonotonicity.returns(held.map(h => Some(h._2)))
  private def breaches(decided: (String, String)*) =
    contractBreaches("Kino Muza | Titans", "model", decided.zipWithIndex.map { case ((take, rule), at) =>
      (at + 1) -> Decided(Option.when(take != "pending")(take), rule) }, alongChain)

  "A listing's populations" should "pass a non-monotone rule going back" in {
    breaches("tmdb:1" -> "accept:exact-top-hit", "tmdb:2" -> "accept:sole-work", "tmdb:1" -> "accept:exact-top-hit") shouldBe empty
  }

  it should "fail a rule that declares no kind" in {
    breaches("tmdb:1" -> "accept:a-rule-nobody-declared") shouldBe
      Seq("Kino Muza | Titans: rule accept:a-rule-nobody-declared declares no kind [model]")
  }

  it should "fail a population-independent rule deciding it two ways" in {
    breaches("none" -> "veto:seasonsapart", "tmdb:1" -> "veto:seasonsapart") shouldBe
      Seq("Kino Muza | Titans: population-independent rule veto:seasonsapart took none and tmdb:1 [model]")
  }

  it should "fail an A -> B -> A over the populations kind (i) and (ii) rules decided, whatever decided between" in {
    breaches("tmdb:1" -> "pin", "none" -> "search:found-nothing", "tmdb:2" -> "accept:sole-work", "tmdb:1" -> "pin") shouldBe
      Seq("Kino Muza | Titans: went back under kind (i)/(ii) rules pin, search:found-nothing [model]")
  }

  it should "read a Pending take as no take" in {
    breaches("tmdb:1" -> "pin", "pending" -> "search:found-nothing", "tmdb:1" -> "pin") shouldBe empty
  }
}
