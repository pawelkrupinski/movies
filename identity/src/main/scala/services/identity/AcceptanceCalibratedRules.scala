package services.identity

import services.identity.AcceptanceRule.Weighing
import services.identity.measures.{Film, IdentityMeasures, ListingShape, Scored, TitleTextMeasures}

/** What the calibrated rules ask beyond the calibration's cut ([[AcceptanceFactRows]]: favoured and unrivalled) — whether
 *  the runner-up's facts beat the best, whether the title sits inside a closer record, whether a film it names alike fits
 *  better — weighed with the evidence `weights`. Functions of the candidates and the listing they score;
 *  [[AcceptanceWiring]] makes the one the rules ask. */
private[identity] final class AcceptanceCalibratedRules(weights: EvidenceWeights) {

  /** The runner-up `best`'s own facts must outweigh and do not ([[AcceptanceRuleTable.FavouredCalibrated]]): another
   *  film the billing could name, one the listing's own evidence reached. */
  private[identity] def runnerUpItsFactsDoNotBeat(weighing: Weighing, best: Scored): Option[Scored] =
    weighing.eligible.filterNot(r => (r eq best) || !r.ownReach || IdentityMeasures.eventNamed(r.listing, r.candidate.film)).headOption
      .filterNot(weights.favours(best, _))

  /** `best` and the eligible records the listing's own evidence reached: a sibling's search alone brings a record and
   *  takes it away. */
  private[identity] def ownReached(weighing: Weighing, best: Scored): Seq[Scored] = weighing.eligible.filter(r => (r eq best) || r.ownReach)

  /** The eligible record the listing's title sits inside ([[titledCloser]]) whose answered facts
   *  `best`'s do not beat, if any: then `best`, which the title only overlaps, is not the listing's
   *  film on that evidence — on its own or pooled. */
  private[identity] def closerThan(best: Scored, eligible: Seq[Scored]): Option[Scored] =
    eligible.find(closer => (closer ne best) && titledCloser(closer, best) && weights.factsAnswered(best, closer) <= weights.factsAnswered(closer, closer))

  /** Does `closer`'s record hold the listing's title, or name a whole piece of it
   *  ([[IdentityMeasures.ContainingRelations]]), while `other`'s only overlaps it, names it by no
   *  original or alternative title (PL "Following" is Nolan's "Śledząc" by its original title), and
   *  does not hold `closer`'s title itself ("English National Ballet presents The Sleeping Beauty"
   *  holds "The Sleeping Beauty")? Then `other` is taken over it only when the facts `closer`'s record
   *  answers favour `other` ([[EvidenceWeights.factsAnswered]]). ES "BTS World Tour 'ARIRANG' In
   *  Buenos Aires: Live" ×82 took the Busan concert, and PL Kinoteka's double bill "Basia. Humor w
   *  paski mam + Kocia Szajka…" took "Basia. Radzę sobie!", on a credit and a runtime the record their
   *  title names merely does not state. */
  private[identity] def titledCloser(closer: Scored, other: Scored): Boolean = {
    def containing(scored: Scored) = scored.category("title").exists(IdentityMeasures.ContainingRelations)
    def words(scored: Scored) = services.titles.TitleContainment.tokens(scored.candidate.film.title).toSet
    containing(closer) && !containing(other) && !other.titleNamesIt && !(words(closer) subsetOf words(other))
  }

  /** A rival the title names by the very pieces it names `best` by whose pooled FACTS ([[EvidenceWeights.facts]]) fit
   *  better — or, named apart by disjoint pieces ([[IdentityMeasures.namedApart]]), at least as well. */
  private[identity] def namedAlikeAndFittingBetter(weighing: Weighing, best: Scored): Option[Scored] =
    weighing.eligible.find(rival => (rival ne best) && {
      val apart = IdentityMeasures.namedApart(best.listing, best.candidate.film, rival.candidate.film)
      val facts = weights.facts
      (namedAlike(best, rival) && facts(rival) > facts(best)) || (apart && facts(rival) >= facts(best))
    })

  /** A newer film named alike that a festival's bare billing names rather than `best` ([[ListingShape.festivalsOlderNamesake]]). */
  private[identity] def newerNamedAlikeByAFestival(weighing: Weighing, best: Scored): Option[Scored] =
    weighing.eligible.find(rival => (rival ne best) && namedAlike(best, rival) &&
      ListingShape.festivalsOlderNamesake(best.listing, best.candidate.film, rival.candidate.film))

  /** Does the billing name `rival` by the very pieces it names `best` by ([[IdentityMeasures.namingPieces]])? */
  private def namedAlike(best: Scored, rival: Scored): Boolean = {
    val pieces = IdentityMeasures.namingPieces(rival.listing, rival.candidate.film)
    pieces.nonEmpty && pieces == IdentityMeasures.namingPieces(best.listing, best.candidate.film)
  }

  /** Does the venue's billing name `best` only WITHIN another eligible candidate's longer title — the agreement's same
   *  concept ([[TitleTextMeasures.namedWithinAnother]]), read where pooled evidence alone would take it? PL "Kacper i
   *  Emma – najlepsi przyjaciele Edukacja Młode Horyzonty" names the 2026 short "Najlepsi przyjaciele" only as part of
   *  the 2013 "Kacper i Emma: Najlepsi przyjaciele". */
  private[identity] def namedWithinAnother(best: Scored, eligible: Seq[Scored]): Boolean = eligible.sizeIs > 1 && {
    val billed = Seq(services.titles.TitleContainment.tokens(best.listing.rawTitle.getOrElse(best.listing.title)))
    def spans(film: Film) = TitleTextMeasures.spansIn(billed, Seq(film))
    TitleTextMeasures.namedWithinAnother(spans(best.candidate.film), eligible.filterNot(_ eq best).map(other => spans(other.candidate.film)))
  }
}
