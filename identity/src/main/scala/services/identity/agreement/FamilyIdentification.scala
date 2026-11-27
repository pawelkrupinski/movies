package services.identity.agreement

import services.identity.vocabulary.toAnswer
import services.identity.vocabulary.Answer
import services.identity.measures.Listing
import services.identity.{Acceptance, IdentityCalibration, IdentityLookups, IdentityResolver}
import services.titles.TitleNormalizer

/** One family's own identification of a cluster: the resolver itself, under the stage's `normalizer` and the family's
 *  `calibration`, over that family's answers ([[verdict]]), and the film its evidence leans to where it took none
 *  ([[leaningOf]]) -- what the families' agreement ([[Agreement]]) then weighs, family by family. */
final class FamilyIdentification(normalizer: TitleNormalizer, calibration: IdentityCalibration) {

  /** The film `family` identifies `listings` as — the resolver itself over the family's answers, as the experiment ran
   *  it — or none, with every record it weighed; `Unknown` while a question it asked is not answered yet. */
  def verdict(listings: Seq[Listing], answers: FamilyAnswers, venues: IdentityLookups): Answer[FamilyVerdict] = {
    val lookups    = new FamilyLookups(answers, venues,
      listings.flatMap(l => Seq(l.title, l.rawTitle, l.cleanTitle) ++ l.originalTitle ++ l.searchTitle).distinct)
    val scored     = IdentityResolver.resolveScored(listings, lookups, normalizer, calibration)
    val resolution = scored.resolution
    if (resolution.unknownQueries > 0 || resolution.unknownFilms > 0) Answer.Unknown
    else resolution.decisions.flatMap(_.film).distinct match {
      case Seq(number) =>
        (for { id <- lookups.idOf(number); answered <- answers.record(id).answered; record <- answered }
          yield FamilyVerdict(answers.family, Some(FamilyPick(answers.family, id, record)), lookups.weighed))
          .toAnswer
      case _ => // none, or the family splits the cluster: no one film
        Answer.Known(FamilyVerdict(answers.family, None, lookups.weighed, leaningOf(scored.evidence, lookups, answers)))
    }
  }

  /** The film a family's evidence LEANS to though it took none: on every listing, its best undenied candidate, at
   *  [[services.identity.Acceptance.LeanMargin]] times the runner-up's probability — the model's own lean
   *  (`Acceptance.leaning`) over the family's search. Never a pick: leans complete a quorum beside a taker, and
   *  a lean to another film is what makes a family's having weighed the film turning it down (PL "Sukienka": RT weighed
   *  "The Dress" at 6.0%, its runner-up at 2.7%; Tempo's IMDb weighed "Tempo" at 2.9% under "Old" at 33.0%). */
  private def leaningOf(nodes: Seq[IdentityResolver.NodeEvidence], lookups: FamilyLookups, answers: FamilyAnswers): Option[SourceRecord] =
    nodes.map { node =>
      val eligible = node.candidates.filterNot(_.denied).sortBy(-_.probability)
      eligible.headOption.filter(best => eligible.lift(1).forall(runnerUp => best.probability >= Acceptance.LeanMargin * runnerUp.probability)).map(_.tmdbId)
    }.distinct match {
      case Seq(Some(number)) => lookups.idOf(number).flatMap(id => answers.record(id).answered.flatten)
      case _                 => None
    }
}
