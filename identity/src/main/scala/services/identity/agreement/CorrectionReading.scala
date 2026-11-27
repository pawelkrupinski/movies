package services.identity.agreement

import scala.collection.mutable

import services.identity.vocabulary.{Dependency, Question, VoterFamily}

/** The evidence against a film the MODEL took, read now ([[Correction]]): the relay its listings bill
 *  ([[RelayCorrection]]), else the venue posters ([[PosterCorrection]]) and — once one questions the take — the venues'
 *  programmes on `listedOn`'s site ([[ProgrammeCorrection]]) and the families ([[FamilyCorrection]]), every film they name by its TMDB record
 *  ([[CorrectionRecords]]). Read through `evidence` ([[AgreementEvidence]]) and the cluster's `candidates`; what it
 *  read noted in the receipt [[AgreementCorrections]] files it with. */
private[agreement] final class CorrectionReading(evidence: AgreementEvidence, candidates: AgreementCandidates, listedOn: Option[VoterFamily]) {
  import evidence.tried

  private val records  = new CorrectionRecords(evidence, candidates)
  private val relays   = new RelayCorrection(candidates, records)
  private val posters  = new PosterCorrection(evidence, candidates)
  private val programmes = new ProgrammeCorrection(records)
  private val families = new FamilyCorrection(evidence, candidates, records, listedOn)

  /** The evidence against the take `model` of the cluster `digested`: the venue posters and — once one names another
   *  film — the venues' programmes and the families. A part still waiting on an answer decides nothing yet, but a part
   *  known may: a programme's contradiction withdraws the take while the families wait. With what it waits on. */
  def correctionOf(model: Int, digested: AgreementEvidence.Cluster, reading: AgreementPass.Read): (StoredCorrection, Option[Set[Question]]) = {
    val reads = mutable.Map.empty[Dependency, Long]
    val asked = mutable.Set.empty[Question]
    // a search or a record TMDB has not answered yet decides nothing: the correction is read again, not kept as final —
    // kept, a take decided while the model's own record or its candidates' searches were gaps stood uncorrected until its
    // listings moved or a deploy read it again (UK Chichester's "The Ties That Bind Us", identity-slice --drift on 573)
    val blindBefore = records.blindReads
    val outcome =
      if (!records.known(model)) None
      else relays.against(digested, model, asked).orElse(againstTake(digested, model, reading, reads, asked))
    // asked on every pass, the attempted ones too; waiting only on what was not attempted yet (the settle rule)
    val waits = Option.when(asked.nonEmpty)(asked.toSet)
    // in the receipt being made: what it read, and each question asked, read again when its answer is filed
    evidence.noted(reads ++ asked.iterator.map(question => Dependency.Asked(question) -> 0L))
    (StoredCorrection(outcome.flatMap(_.film), outcome.map(_.line), asked.exists(question => !tried(question)) || records.blindReads != blindBefore), waits)
  }

  /** What the evidence against the take `model` decides ([[Correction.decide]]): (a) a venue poster matching another
   *  candidate, (e) the venues' programmes and (b) the families — the last two asked only of a take a poster questions. */
  private def againstTake(digested: AgreementEvidence.Cluster, model: Int, reading: AgreementPass.Read,
                          reads: mutable.Map[Dependency, Long], asked: mutable.Set[Question]): Option[Correction.Outcome] = {
    val (vetoed, questioned) = posters.against(digested, model, reads, asked, reading)
    val programme = programmes.listed(digested, model, questioned, reading, reads, asked)
    families.decided(digested, model, vetoed, questioned, programme, reads, asked)
  }
}
