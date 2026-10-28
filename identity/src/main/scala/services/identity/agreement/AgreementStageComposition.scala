package services.identity.agreement

import services.identity.{Answer, CatalogueAnswers, IdentityCalibration, IdentityLookups, PosterAnswers, Question}
import services.movies.TitleNormalizer

/**
 * THE ONE WAY AN AGREEMENT STAGE IS BUILT: the worker's (`IdentityProjectionWiring.newAgreementStage`) and every harness's
 * — the convergence harness and the live drift check (`scripts.IdentityDrift`), the capture (`tools.UnmatchedClusters`),
 * the dumps — each handing it its [[Sources]], the worker's live, a harness's recorded. Every role is filled the same
 * way whoever builds it: the venues' pages and TMDB apart (a harness handing one lookups for both hid that the worker's
 * venue lookups answer no film, and prod kept 26 takes a whole resolve made, 2026-10-27), the families, the finds,
 * the posters, the catalogue, the attempts and their marks, and the TMDB filings its answers' changes count ([[apply]]).
 *
 * What a harness may build otherwise is an [[Override]], each named here with the reason it changes no take; nothing
 * else differs. `AgreementStageCompositionSpec` holds both: no stage is built but through [[apply]] (the spec names the
 * one exception, the slice tool that compiles into refs older than this), and no override but these exists.
 */
object AgreementStageComposition {

  /** Everything a stage reads: the worker's live stores and lookups, or a harness's recorded ones. */
  final case class Sources(families: Map[VoterFamily, FamilyAnswers], venuePages: IdentityLookups, tmdb: IdentityLookups,
                           tmdbOf: String => Answer[Option[Int]], stored: AgreementVerdicts, clock: java.time.Clock,
                           normalizer: TitleNormalizer, identities: Seq[VoterFamily], listedOn: Option[VoterFamily],
                           changes: AnswerChanges, posters: PosterAnswers, catalogue: CatalogueAnswers,
                           attempted: Question => Boolean, attemptsMarked: Option[() => Long], keys: AgreementKeys,
                           calibration: IdentityCalibration = IdentityCalibration.resolver)

  /** What a stage sets off beside its takes — the worker's queue and inline asks, its metrics, the
   *  heap's pressure. A harness's own: it answers what the stage `wanted` between applies, as the queue would. */
  final case class Effects(ask: Set[Question] => Unit = _ => (), metrics: AgreementStage.Metrics = AgreementStage.Metrics.Silent,
                           askNow: AgreementStage.AskNow = AgreementStage.AskNow.Never,
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

  /** The stage over `sources`, its `changes` following it ([[AnswerChanges.follow]]): a TMDB record or search it waits on
   *  or read moves its answers' version when filed, the fill's filings too — the worker's wiring did that by hand, and
   *  the drift replay's stage, built without it, heard none of them: a take waiting on one stood until a scrape or a deploy
   *  read it again, a flip of the replay's own (UK "The Ties That Bind Us", US "NT Live: Hamlet", run 3509). */
  def apply(sources: Sources, effects: Effects = Effects(), overrides: Set[Override] = Set.empty): AgreementStage = {
    val unbudgeted = overrides(Override.Unbudgeted)
    val stage = new AgreementStage(sources.families, sources.venuePages, sources.normalizer, sources.calibration, sources.tmdbOf, sources.stored,
      ask = effects.ask, metrics = effects.metrics, clock = sources.clock, changes = sources.changes, posters = sources.posters,
      tmdb = Some(sources.tmdb), identities = sources.identities, catalogue = sources.catalogue, listedOn = sources.listedOn,
      correctionPostersAtOnce = if (unbudgeted) Int.MaxValue else AgreementStage.CorrectionPostersAtOnce,
      correctionsPerApply = overrides.collectFirst { case Override.CorrectionsPerApply(n) => n }
        .getOrElse(if (unbudgeted) Int.MaxValue else AgreementStage.CorrectionsPerApply),
      backfillPerApply = if (unbudgeted) Int.MaxValue else AgreementStage.BackfillPerApply,
      askNow = effects.askNow, attempted = sources.attempted, attemptsMarked = sources.attemptsMarked,
      keyStore = sources.keys, pressed = effects.pressed)
    sources.changes.follow(question => stage.wanted.contains(question), stage.reads)
    stage
  }
}
