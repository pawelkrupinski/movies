package services.identity.agreement

import services.identity.{IdentityCalibration, IdentityLookups}
import services.identity.vocabulary.{Answer, CatalogueAnswers, PosterAnswers, Question, VoterFamily}
import services.titles.TitleNormalizer

/**
 * The specs' one way to build an [[AgreementStage]] over hand-made sources: what the stage reads becomes its
 * [[AgreementPipeline]], and the rest is the stage's own, every role defaulted as a spec that leaves it out expects.
 * The worker and its harnesses build theirs through [[AgreementStageComposition]] instead (AgreementStageCompositionSpec).
 *
 * The parameters keep the order the stage's own constructor had before the pipeline was handed to it whole: the
 * identity-slice tool reaches the stage at every ref it replays through that one positional list
 * (`services.identity.OlderRefs.agreementStage`), this builder at a ref that has it, the stage's constructor at one before.
 */
object AgreementStageFixture {
  def apply(families: Map[VoterFamily, FamilyAnswers], venuePages: IdentityLookups, normalizer: TitleNormalizer,
            calibration: IdentityCalibration, tmdbOf: String => Answer[Option[Int]], stored: AgreementVerdicts,
            ask: Set[Question] => Unit = _ => (), metrics: AgreementStage.Metrics = AgreementStage.Metrics.Silent,
            clock: java.time.Clock, changes: AnswerChanges = AnswerChanges.Unknown, posters: PosterAnswers = PosterAnswers.Silent,
            tmdb: Option[IdentityLookups] = None, identities: Seq[VoterFamily] = Nil,
            rules: services.identity.agreement.UnifiedRules = services.identity.agreement.UnifiedRules.resolver,
            catalogue: CatalogueAnswers = CatalogueAnswers.Silent, listedOn: Option[VoterFamily] = None,
            correctionPostersAtOnce: Int = AgreementStage.CorrectionPostersAtOnce,
            correctionsPerApply: Int = AgreementStage.CorrectionsPerApply,
            backfillPerApply: Int = AgreementStage.BackfillPerApply,
            askNow: AgreementStage.AskNow = AgreementStage.AskNow.Never,
            attempted: Question => Boolean = _ => false,
            givenUp: Question => Boolean = _ => true,
            attemptsMarked: Option[() => Long] = None,
            staging: Option[Seq[services.identity.staging.StagingEvent] => Unit] = None,
            clusterKeys: ClusterKeys = new ClusterKeys(new InMemoryAgreementKeys),
            tmdbReads: TmdbReadTracking = new TmdbReadTracking,
            pressed: () => Boolean = () => false,
            fullValidationEvery: Int = AgreementStage.FullValidationEvery): AgreementStage =
    new AgreementStage(
      new AgreementPipeline(families, venuePages, normalizer, calibration, tmdbOf, stored, changes, tmdb, identities, rules,
        catalogue, listedOn, askNow, attempted, posters, clock, tmdbReads),
      ask = ask, metrics = metrics, correctionPostersAtOnce = correctionPostersAtOnce, correctionsPerApply = correctionsPerApply,
      backfillPerApply = backfillPerApply, givenUp = givenUp, attemptsMarked = attemptsMarked, staging = staging,
      clusterKeys = clusterKeys, pressed = pressed, fullValidationEvery = fullValidationEvery)
}
