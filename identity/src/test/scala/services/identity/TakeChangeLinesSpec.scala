package services.identity

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey
import settings.CommitSha

/** A take change's line names why it moved — the rules, the runner-up, what blocked a listing no rule took — and a
 *  command that replays it; the tick's summary counts every take by kind and basis, and how many went unlogged. */
class TakeChangeLinesSpec extends AnyFlatSpec with Matchers {
  private val lines = new TakeChangeLines(Country.Poland, CommitSha("0e41a4812"))
  private val muza  = ListingKey.Published("Kino Muza", "Ojciec chrzestny '72", Some(1972), Nil)

  "A take change's line" should "carry the decision's rules, runner-up and blocker, and a repro that quotes the title for the shell" in {
    val trace = DecisionTrace(None, None, Map(muza -> DecisionTrace.Node(accepted = None, joins = Nil, apart = Nil,
      refusals = Seq(DecisionTrace.Refusal("favoured-calibrated", "below the cut")), candidates = Seq("238 31.0% rank 1 'The Godfather' (1972)"),
      blocker = Some("rule:below-the-cut"))))
    val decision = ResolverDecision(Seq(muza), None, 0.62, ResolverDecision.Basis.BelowThreshold, Seq("first why", "last why"))(trace)
    val line     = lines.of(TakeChange(muza, "f-12", Some(238), None), Some("Ojciec chrzestny"), None, Some(decision), None, None)
    val fields   = line.fields.toMap
    fields("rules") shouldBe Some("refused:favoured-calibrated:below-the-cut")
    fields("runner_up") shouldBe Some("238 31.0% rank 1 'The Godfather' (1972)")
    fields("blocker") shouldBe Some("rule:below-the-cut")
    fields("reason_first") shouldBe Some("first why")
    fields("previous_basis") shouldBe None
    fields("repro") shouldBe "scripts/identity-slice pl --key 'Kino Muza|Ojciec chrzestny '\\''72' --ref 0e41a4812"
    line.message should startWith("identity take change: lost Kino Muza | Ojciec chrzestny '72: tmdb 238 (Ojciec chrzestny) -> none (f-12); " +
      "reason BelowThreshold: last why: first why; explain: scripts/identity-slice pl")
  }

  "A tick's summary" should "count every take by kind and basis, and say how many were not logged a line each" in {
    val takes = Seq(TakeChange(muza, "f-1", Some(1), None), TakeChange(muza.copy(venue = "Helios"), "f-1", Some(1), Some(2)),
      TakeChange(muza.copy(venue = "Rialto"), "f-3", None, Some(4)))
    val summary = lines.summary(takes, t => Option.when(t.after.isDefined)(ResolverDecision.Basis.Agreed), logged = 2)
    summary.message shouldBe "identity take change: summary 3 take changes (changed 1, gained 1, lost 1); by basis Agreed 2, no decision 1; " +
      "1 more not logged"
    summary.fields.toMap shouldBe Map("take_changes" -> 3, "logged" -> 2, "by_basis" -> "Agreed 2, no decision 1", "lost" -> 1, "changed" -> 1,
      "gained" -> 1)
  }
}
