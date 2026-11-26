package services.identity

import services.identity.AcceptanceRule.isEligible
import services.identity.measures.{IdentityMeasures, MultiFilmBill, Scored}

/** What a node or a cluster no rule took still answers: the film a fallback source's pool FALLS BACK to, and the film its
 *  evidence LEANS to. Weighed with the evidence `weights`, the listing read as a bill by `billRules`, its facts by
 *  `factRules`, a closer record by `calibratedRules`; [[AcceptanceWiring]] makes the one [[Acceptance]] asks. */
private[identity] final class AcceptanceFallbacks(weights: EvidenceWeights, billRules: AcceptanceBillRules, factRules: AcceptanceFactRules,
                                                  calibratedRules: AcceptanceCalibratedRules) {
  import billRules.billsSeveralWorks
  import factRules.{contradicted, corroborated}
  import calibratedRules.closerThan
  import AcceptanceFallbacks.LeanMargin

  /** The film a cluster no TMDB film was taken for FALLS BACK to, from a fallback source's pool (a `fallback`
   *  [[FamilyScope]]): the best eligible film whose record carries the listing's title as its own — its title, original
   *  or an alternative title — CORROBORATED by a fact the listing publishes, its director or its very year, contradicted by
   *  none, on a listing billing one work, with no rival so titled that its facts do not favour it over. Stricter than any
   *  TMDB rule: a fallback record has no search rank or popularity to lend, and a wrong film is worse than none (measured
   *  2026-09-30: every correct rating page for a film TMDB lacks came from a row publishing a year or a director). */
  def fallback(ranked: Seq[Scored]): Option[Scored] = {
    // on the source's own record only — one parsed from IMDb's answer always holds its principal credits — never the
    // title and year a suggestion gave before the record was filed, which no credit or running time could rule out
    val titled = ranked.filter(isEligible).filter(scored => scored.category("title").exists(IdentityMeasures.Rivalling) && scored.candidate.film.directors.isDefined)
    titled.headOption.filter(best => !MultiFilmBill.billsTwoWorks(best.listing) && !contradicted(best) && corroborated(best) &&
      !best.category("director").contains("different") && titled.drop(1).forall(weights.favours(best, _)))
  }

  /** The film a node's evidence LEANS to though no rule took it: the best eligible candidate on a listing billing one
   *  work, at [[LeanMargin]] times the runner-up's probability — every fact the listing publishes weighed in it, the
   *  priors lending — and no closer record. Not the runner-up rule's facts alone: a bare title's "facts" are the
   *  records' missing credits (US "Lady Frankenstein", 35.8% beside 2025's "Frankenstein" at 4.0%, lost on its record
   *  crediting a director the listing does not name). Not a match: the film a no-match's card may keep the ratings of, when an
   *  earlier answer gave the card that film's ([[IdentityProjectionPlan]]). PL "Tatarak" read 18.4% for Wajda's 2009
   *  film, its 1965 namesake 3.3%; "Lalka" leans to the 2026 film at 68.1%, not the 1968 one the old pipeline rated
   *  it as; "Bolek i Lolek" (28.9% beside "Reksio" at 28.9%) and the 1986 and 2025 "Caravaggio" lean to neither. */
  def leaning(ranked: Seq[Scored]): Option[Scored] =
    Option.when(ranked.headOption.forall(any => !MultiFilmBill.billsTwoWorks(any.listing)) && !billsSeveralWorks(ranked))(ranked.filter(isEligible))
      .flatMap(eligible => eligible.headOption.filter(_.untaken.isEmpty).filter(best => eligible.lift(1).forall(runnerUp =>
        best.probability >= LeanMargin * runnerUp.probability) && closerThan(best, eligible).isEmpty))
}

private[identity] object AcceptanceFallbacks {
  /** How many times its runner-up's probability the film a no-match leans to holds ([[AcceptanceFallbacks.leaning]]): a tie
   *  between namesakes is no lean. */
  val LeanMargin = 1.25
}
