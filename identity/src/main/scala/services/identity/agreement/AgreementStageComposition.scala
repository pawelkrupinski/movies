package services.identity.agreement

import services.identity.vocabulary.Question


/**
 * THE ONE WAY AN AGREEMENT STAGE IS BUILT: the worker's (`AgreementWiring.newAgreementStage`) and every harness's
 * — the convergence harness and the live drift check (`scripts.IdentityDrift`), the capture (`tools.UnmatchedClusters`),
 * the dumps — each handing it its [[Sources]], the worker's live, a harness's recorded. Every role is filled the same
 * way whoever builds it: the venues' pages and TMDB apart (a harness handing one lookups for both hid that the worker's
 * venue lookups answer no film, and prod kept 26 takes a whole resolve made, 2026-10-27), the families, the finds,
 * the posters, the catalogue, the attempts, what is given up on and the attempts' marks.
 *
 * What a harness may build otherwise is an [[Override]], each named here with the reason it changes no take; nothing
 * else differs. `AgreementStageCompositionSpec` holds both: no stage is built but through [[apply]] (the spec names the
 * one exception, the slice tool that compiles into refs older than this), and no override but these exists.
 */
object AgreementStageComposition {

  /** Everything a stage reads ([[AgreementSources]]), by the name its builders have always used. */
  type Sources = AgreementSources
  val Sources: AgreementSources.type = AgreementSources

  /** What a stage sets off beside its takes — the worker's queue and inline asks, its metrics, the staging screen, the
   *  heap's pressure. A harness's own: it answers what the stage `wanted` between applies, as the queue would. */
  final case class Effects(ask: Set[Question] => Unit = _ => (), metrics: AgreementStage.Metrics = AgreementStage.Metrics.Silent,
                           askNow: AgreementStage.AskNow = AgreementStage.AskNow.Never,
                           staging: Option[Seq[services.identity.staging.StagingEvent] => Unit] = None,
                           pressed: () => Boolean = () => false)

  /** The ways a harness's stage may differ from the worker's, each with why no take moves by it. */
  enum Override(val reason: String) {
    /** Budgets spread the corrections' posters, the corrections and the backfill over applies; a harness settling a whole
     *  corpus at once applies until nothing is left, so it reaches the takes the budgeted worker reaches over applies. */
    case Unbudgeted extends Override("a corpus settled at once reaches what the budgets spread over applies")
    /** A measure of the corrections' budget itself (`integration.ModelTakeCorrectionCost`): how many an apply corrects,
     *  every correction reached all the same over applies. */
    case CorrectionsPerApply(perApply: Int) extends Override("the corrections' budget, measured: it spreads them over applies")
  }

  def apply(sources: Sources, effects: Effects = Effects(), overrides: Set[Override] = Set.empty): AgreementStage = {
    val unbudgeted = overrides(Override.Unbudgeted)
    val pipeline   = new AgreementPipeline(sources, sources.identities,
      services.identity.agreement.UnifiedRules.resolver, sources.listedOn, effects.askNow, sources.clock)
    new AgreementStage(pipeline, ask = effects.ask, metrics = effects.metrics,
      correctionPostersAtOnce = if (unbudgeted) Int.MaxValue else AgreementStage.CorrectionPostersAtOnce,
      correctionsPerApply = overrides.collectFirst { case Override.CorrectionsPerApply(n) => n }
        .getOrElse(if (unbudgeted) Int.MaxValue else AgreementStage.CorrectionsPerApply),
      backfillPerApply = if (unbudgeted) Int.MaxValue else AgreementStage.BackfillPerApply,
      givenUp = sources.givenUp, attemptsMarked = sources.attemptsMarked,
      staging = effects.staging, clusterKeys = new ClusterKeys(sources.keys), pressed = effects.pressed)
  }
}
