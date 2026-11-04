package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.{Correction, VoterFamily}
import services.movies.ListingKey

/** Every rule a closed list names declares its kind in [[RuleContract]]: a new rule with none fails here (the population
 *  walk, PopulationMonotonicitySpec, fails on one it sees decide a real take). */
class RuleContractSpec extends AnyFlatSpec with Matchers {
  private val muza = ListingKey.Published("Kino Muza", "Ojciec chrzestny '72", Some(1972), Nil)

  private def undeclared(ids: Iterable[String]): Seq[String] = ids.filter(RuleContract.kindOf(_).isEmpty).toSeq.distinct.sorted

  "Every acceptance rule" should "declare its kind, alone and pooled" in {
    val acceptance = new Acceptance(IdentityCalibration.resolver)
    undeclared(acceptance.aloneRules.map("accept:" + _.name) ++ acceptance.pooledOrder.map("pooled:" + _)) shouldBe empty
  }

  "Every basis a decision is taken by" should "name a rule of a declared kind when its trace names none" in {
    val ids = ResolverDecision.Basis.values.toSeq.map(basis =>
      DecidedBy.of(ResolverDecision(Seq(muza), Some(1), 0.5, basis, Nil)(DecisionTrace.Empty), muza))
    undeclared(ids) shouldBe empty
  }

  "Every veto the model decides members apart by, and every blocker of a listing no rule took" should "declare its kind" in {
    undeclared(DecisionTrace.Apart.map("veto:" + DecisionTrace.id(_)) ++
      Seq("search:found-nothing", "search:unanswered", "rule:none", "veto:imdb-suggested-only")) shouldBe empty
  }

  "Every learned cannot-link" should "be declared population-independent, whatever a refit names it" in {
    val learned = IdentityCalibration.resolver.cannotLinks
      .map(rule => "veto:" + DecisionTrace.id(s"Learned(${rule.name})"))
    learned should not be empty
    learned.map(RuleContract.kindOf).distinct shouldBe Seq(Some(RuleKind.PopulationIndependent))
  }

  "Every agreement of film-database families, correction and withdrawal" should "declare its kind" in {
    val families = VoterFamily.values.toSeq.map(_.label)
    val agreed   = (1 to families.size).flatMap(families.combinations).map(c => "agreed:" + c.sorted.mkString("+"))
    val kinds    = Seq(Correction.Families, Correction.Filmweb, Correction.Poster)
    val against  = (1 to kinds.size).flatMap(kinds.combinations).map(_.map(kind => s"$kind: says so").mkString("; ")) ++ Seq("relay: newer")
    val corrected = for (says <- against; film <- Seq(Some(1), None)) yield Correction.ruleOf(Correction.Outcome(film, s"x — $says"))
    undeclared(agreed ++ corrected) shouldBe empty
    RuleContract.kindOf("withdrawal:judged-wrong") shouldBe Some(RuleKind.PopulationIndependent)
    RuleContract.kindOf("withdrawal:filmweb") shouldBe Some(RuleKind.NonMonotone)
  }

  "The declarations" should "name each id once, and no id two patterns" in {
    val all = RuleContract.declared.map(_._1)
    all.map(_.pattern).diff(all.map(_.pattern).distinct) shouldBe empty
    val patterns = all.filterNot(_.exact)
    patterns.foreach { pattern =>
      val example = pattern.pattern.replace("*", "x")
      withClue(s"$example: ")(patterns.filter(_.matches(example)).map(_.pattern) shouldBe Seq(pattern.pattern))
    }
  }

  "An undeclared rule" should "have no kind, and decide no namesake-ambiguous listing" in {
    RuleContract.kindOf("accept:a-rule-nobody-declared") shouldBe None
    RuleContract.mayDecideNamesakeAmbiguous("accept:a-rule-nobody-declared") shouldBe false
  }

  "A namesake-ambiguous listing" should "be decided only by a population-independent or monotone rule" in {
    RuleContract.mayDecideNamesakeAmbiguous("pin") shouldBe true                     // (ii)
    RuleContract.mayDecideNamesakeAmbiguous("veto:seasonsapart") shouldBe true       // (i)
    RuleContract.mayDecideNamesakeAmbiguous("accept:exact-top-hit") shouldBe false   // (iii)
    RuleContract.mayDecideNamesakeAmbiguous("fill:family.facts") shouldBe false      // (iii)
  }
}
