package services.identity

import services.identity.AcceptanceRule.Weighing

/** What the acceptance rules that take a film by its calibrated probability ask — favoured and unrivalled
 *  ([[AcceptanceRuleTable]]) — and the candidate the evidence leans to.
 *  Part of [[Acceptance]], which mixes it in. */
private[identity] trait AcceptanceCalibratedRules { this: Acceptance =>
  import weights.{factsAnswered, favours, facts}

  /** The runner-up `best`'s own facts must outweigh and do not ([[AcceptanceRuleTable.FavouredCalibrated]]): another
   *  film the billing could name, one the listing's own evidence reached. */
  private[identity] def runnerUpItsFactsDoNotBeat(weighing: Weighing, best: Scored): Option[Scored] =
    weighing.eligible.filterNot(r => (r eq best) || !r.ownReach || IdentityMeasures.eventNamed(r.listing, r.candidate.film)).headOption
      .filterNot(favours(best, _))
  /** `best` and the eligible records the listing's own evidence reached: a sibling's search alone brings a record and
   *  takes it away. */
  private[identity] def ownReached(weighing: Weighing, best: Scored): Seq[Scored] = weighing.eligible.filter(r => (r eq best) || r.ownReach)

  /** Another eligible film of `best`'s credited director that the title names, or that the listing's year dates as
   *  closely, when the title names nothing of `best` (`title=none`): walking a director's credit reaches his films, and a
   *  title naming none of two his facts fit alike picks neither — DE "München Mord: Leben und Sterben in Schwabing" [2019]
   *  {Sascha Bigler}, a TV episode, is neither of his 2019 films; UK "Gremlins Special Event" {Joe Dante} is not his
   *  "Innerspace", however its 120 minutes fit, beside the Gremlins it names. A translation the facts single out stands
   *  (PL "Fach" is Rossi's "Il Mestiere"). */
  private[identity] def anotherOfItsDirector(best: Scored, eligible: Seq[Scored]): Option[Scored] =
    if (!best.category("title").contains("none") || !IdentityMeasures.sameDirector(best.measures) || inflects(best)) None
    else eligible.find(other => (other ne best) && IdentityMeasures.sameDirector(other.measures) &&
      (other.titleNamesIt || (best.number("year.delta").isDefined && other.number("year.delta") == best.number("year.delta"))))
  /** Does a word of the billing inflect a word of the record's title ("Osobliwy świat Hieronymusa Boscha" of "The
   *  Curious World of Hieronymus Bosch"): a title in another language naming the film, though it shares no word whole. */
  private[identity] def inflects(scored: Scored): Boolean = {
    import services.titles.TitleContainment.tokens
    val billed = scored.listing.ownTitles.flatMap(tokens)
    scored.candidate.film.titles.flatMap(tokens).filter(_.length >= IdentityMeasures.StemLetters).exists(word => billed.exists(_.startsWith(word)))
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
    Option.when(ranked.headOption.forall(any => !MultiFilmBill.billsTwoWorks(any.listing)) && !billsSeveralWorks(ranked))(eligibleOf(ranked))
      .flatMap(eligible => eligible.headOption.filter(_.untaken.isEmpty).filter(best => eligible.lift(1).forall(runnerUp =>
        best.probability >= Acceptance.LeanMargin * runnerUp.probability) && closerThan(best, eligible).isEmpty))

  /** The eligible record the listing's title sits inside ([[titledCloser]]) whose answered facts
   *  `best`'s do not beat, if any: then `best`, which the title only overlaps, is not the listing's
   *  film on that evidence — on its own or pooled. */
  private[identity] def closerThan(best: Scored, eligible: Seq[Scored]): Option[Scored] =
    eligible.find(closer => (closer ne best) && titledCloser(closer, best) && factsAnswered(best, closer) <= factsAnswered(closer, closer))

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
    def spans(film: IdentityMeasures.Film) = TitleTextMeasures.spansIn(billed, Seq(film))
    TitleTextMeasures.namedWithinAnother(spans(best.candidate.film), eligible.filterNot(_ eq best).map(other => spans(other.candidate.film)))
  }
}
