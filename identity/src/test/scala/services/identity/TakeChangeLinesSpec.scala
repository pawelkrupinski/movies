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

  it should "name the stage and rule that decided it, the score and runner-up, the trigger and the title rules, as ids" in {
    val withRules = new TakeChangeLines(Country.Poland, CommitSha("0e41a4812"), listing => Seq("title:search-programme-prefix", "format:3d"))
    val trace = DecisionTrace(None, None, Map(muza -> DecisionTrace.Node(accepted = Some("sole-work"), joins = Nil, apart = Nil,
      candidate = Some(238), score = Some(0.8123), runnerUp = Some(DecisionTrace.Rival(240, 0.1)))))
    val decision = ResolverDecision(Seq(muza), Some(238), 0.9, ResolverDecision.Basis.OwnMatch, Seq("why"))(trace)
    val listing  = Listing(models.KinoMuza, muza, "Ojciec chrzestny '72", "Ojciec chrzestny", "ojciec chrzestny", Some(1972), Nil, None, None, None)
    // the take before beat 238 with 240: the runner-up won
    val prior    = TakeChange.Prior(ResolverDecision.Basis.OwnMatch, 0.7, None, Some("accept:exact-top-hit"), Some(238))
    val fields   = withRules.of(TakeChange(muza, "f-12", Some(240), Some(238)), None, Some("f-12" -> "Ojciec chrzestny"), Some(decision),
      Some(listing), Some(prior), Some("scrape")).fields.toMap
    fields("stage") shouldBe Some("model")
    fields("decided_by") shouldBe Some("accept:sole-work")
    fields("score") shouldBe Some(0.812)
    fields("runner_up_tmdb") shouldBe Some(240)
    fields("runner_up_score") shouldBe Some(0.1)
    fields("margin") shouldBe Some(0.712)
    fields("trigger") shouldBe Some("scrape")
    fields("title_rules") shouldBe Some("title:search-programme-prefix format:3d")
    fields("previous_decided_by") shouldBe Some("accept:exact-top-hit")
    fields("previous_runner_up_tmdb") shouldBe Some(238)
    fields("runner_up_won") shouldBe Some(true)

    // a relay's own rule is the relay stage's; a pin on either side is a review's doing, whatever ran the projection
    val relay = ResolverDecision(Seq(muza), Some(238), 0.9, ResolverDecision.Basis.OwnMatch, Nil)(
      DecisionTrace(None, None, Map(muza -> DecisionTrace.Node(Some("relay-record"), Nil, Nil))))
    DecidedBy.stage(relay, muza) shouldBe "relay"
    val pinned = ResolverDecision(Seq(muza), Some(238), 1.0, ResolverDecision.Basis.Pinned, Nil)()
    val byPin  = lines.of(TakeChange(muza, "f-12", Some(240), Some(238)), None, None, Some(pinned), None, None, Some("deploy")).fields.toMap
    (byPin("stage"), byPin("decided_by"), byPin("trigger")) shouldBe ((Some("pin"), Some("pin"), Some("review")))
    // a take no runner-up was weighed beside names none, and says nothing of one winning
    val bare = lines.of(TakeChange(muza, "f-12", Some(240), Some(238)), None, None, Some(pinned), None,
      Some(TakeChange.Prior(ResolverDecision.Basis.OwnMatch, 0.7, None)), None).fields.toMap
    (bare("runner_up_tmdb"), bare("margin"), bare("runner_up_won")) shouldBe ((None, None, None))
  }

  "A decision's rule id" should "name the stage after the model that took it, and the model's rule otherwise" in {
    def decided(basis: ResolverDecision.Basis, trace: DecisionTrace = DecisionTrace.Empty, agreed: Map[String, String] = Map.empty) =
      DecidedBy.of(ResolverDecision(Seq(muza), Some(1), 0.5, basis, Nil, agreed = agreed)(trace), muza)
    decided(ResolverDecision.Basis.Agreed, agreed = Map("wiki" -> "Q1", "imdb" -> "tt1")) shouldBe "agreed:imdb+wiki"
    decided(ResolverDecision.Basis.PooledMatch, DecisionTrace(Some("unrivalled-calibrated"), None, Map.empty)) shouldBe "pooled:unrivalled-calibrated"
    decided(ResolverDecision.Basis.Vetoed, DecisionTrace(None, Some(DecisionTrace.Veto("Learned(runtime.delta >= 11)", None)), Map.empty)) shouldBe
      "veto:learned-runtime-delta-11"
    decided(ResolverDecision.Basis.BelowThreshold, DecisionTrace(None, None, Map(muza -> DecisionTrace.Node(None, Nil, Nil,
      blocker = Some("search:found-nothing"))))) shouldBe "search:found-nothing"
    decided(ResolverDecision.Basis.Filled, DecisionTrace(None, None, Map.empty, stage = Some("fill:venues.current"))) shouldBe "fill:venues.current"
    decided(ResolverDecision.Basis.Poster) shouldBe "poster:vote"
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
