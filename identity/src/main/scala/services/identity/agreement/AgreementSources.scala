package services.identity.agreement

import services.identity.vocabulary.{Answer, CatalogueAnswers, PosterAnswers, Question, VoterFamily}
import services.identity.{IdentityCalibration, IdentityLookups}
import services.titles.TitleNormalizer

/** Everything an agreement stage reads ([[AgreementStageComposition.Sources]]): the worker's live stores and lookups, or a
 *  harness's recorded ones — each stage handed the role it reads ([[AgreementSources.StageSources]]), never these sources whole. */
final case class AgreementSources(families: Map[VoterFamily, FamilyAnswers], venuePages: IdentityLookups, tmdb: IdentityLookups,
                                  tmdbOf: String => Answer[Option[Int]], stored: AgreementVerdicts, clock: java.time.Clock,
                                  normalizer: TitleNormalizer, identities: Seq[VoterFamily], listedOn: Option[VoterFamily],
                                  changes: AnswerChanges, posters: PosterAnswers, catalogue: CatalogueAnswers,
                                  attempted: Question => Boolean,
                                  /** Answered or given up on, as the settle rule reads a question it settled past: one whose ask
                                   *  failed and sits in its retry backoff is not, and leaves a take settled past it Pending (the
                                   *  owner, 2026-10-31). The worker's: the question memory's (`QuestionMemory.givenUp`). */
                                  givenUp: Question => Boolean,
                                  attemptsMarked: Option[() => Long], keys: AgreementKeys,
                                  calibration: IdentityCalibration = IdentityCalibration.resolver) extends AgreementSources.StageSources {
  def tmdbLookups: Option[IdentityLookups] = Some(tmdb)
}

/**
 * THE ROLES THE AGREEMENT STAGES READ: each stage is handed the role it reads ([[AgreementEvidence]] its evidence,
 * [[VerdictIndex]] the verdicts, [[AgreementCandidates]] the changes and posters, [[AgreementTakes]] the catalogue, an
 * apply's [[AgreementPass.Read]] the posters and the families), never the whole sources: the pipeline holds them as
 * [[StageSources]] and hands each stage the same object, typed as its role. The worker's and every harness's sources
 * (the case class above) are one; the specs' (`AgreementStageFixture`) another.
 */
object AgreementSources {

  /** Each voter family's answers. */
  trait FamilySource {
    def families: Map[VoterFamily, FamilyAnswers]
    /** The families asked: what a stored verdict was decided under ([[AgreementStage.configured]]). */
    final def voters: Set[VoterFamily] = families.keySet
  }

  /** What the evidence reads: the families' answers, the venues' pages and TMDB's lookups kept apart (a harness handing
   *  one lookups for both hid that the worker's venue lookups answer no film), and the title text. */
  trait EvidenceSource extends FamilySource {
    def venuePages: IdentityLookups
    def tmdbLookups: Option[IdentityLookups]
    def normalizer: TitleNormalizer
    /** The calibration the families' resolves and the model's evidence are scored under. */
    def calibration: IdentityCalibration
    /** The TMDB film TMDB finds by an IMDb id, as filed. */
    def tmdbOf: String => Answer[Option[Int]]
    /** Whether a question was asked and its answer given up on or not given yet (the settle rule). */
    def attempted: Question => Boolean
  }

  /** What was filed since a verdict was read. */
  trait ChangeSource { def changes: AnswerChanges }

  /** The verdicts stored, and what was filed since one was read. */
  trait VerdictSource extends ChangeSource { def stored: AgreementVerdicts }

  /** The posters' hashes. */
  trait PosterSource { def posters: PosterAnswers }

  /** What the candidates read: what was filed since, and the posters' hashes. */
  trait CandidateSource extends ChangeSource with PosterSource

  /** The catalogue's ids. */
  trait CatalogueSource { def catalogue: CatalogueAnswers }

  /** Every role the pipeline hands its stages. */
  trait StageSources extends EvidenceSource with VerdictSource with CandidateSource with CatalogueSource
}
