package services.identity

import services.identity.AcceptanceRule.Weighing
import services.identity.measures.Scored.Accepted
import services.identity.measures.{DecisionTrace, Scored}

/** Which of a listing's scored candidates — ranked best first, the denied ones among them — it TAKES, and at what
 *  confidence: [[aloneNamed]] for a node on its own evidence, [[pooled]] for a cluster's pooled evidence. Every rule is
 *  a row of [[AcceptanceRuleTable]], each a function of the ranked candidates alone, tried in the table's order, the
 *  first to accept deciding; the listing's season production, when a record names it, pre-empts them all
 *  ([[AcceptanceStageRows.seasonProduction]]). A new rule is a new row of the rows of its kind ([[AcceptanceTitleRows]],
 *  [[AcceptanceFactRows]], [[AcceptanceStageRows]]), its questions functions of the rule class of its kind; this class
 *  only runs the table, over the rule graph [[AcceptanceWiring]] makes on the evidence `weights`. */
private[identity] final class Acceptance(val weights: EvidenceWeights) {

  private val wiring = new AcceptanceWiring(weights)
  /** Every rule this acceptance runs, in order ([[AcceptanceRuleTable]]). */
  private[identity] val table: AcceptanceRuleTable = wiring.table
  private val stageRows: AcceptanceStageRows       = wiring.stageRows
  private val titleRules: AcceptanceTitleRules     = wiring.titleRules
  private val billRules: AcceptanceBillRules       = wiring.billRules
  private val vetoRules: AcceptanceVetoRules       = wiring.vetoRules
  private val fallbacks: AcceptanceFallbacks       = wiring.fallbacks
  import table.{Vetoes, aloneRules, firstOf, pooledRules}
  import billRules.takesNoBill

  /** A rule's verdict: the film it accepts, or the first of its steps that refused — the identity trace's
   *  `refused:<rule>:<why>`, so a listing no rule took says which condition stopped each — with the candidate the rule
   *  was weighing when it did and what that candidate's evidence said ([[Acceptance.Refused]]). */
  private[identity] type Verdict = AcceptanceRule.Verdict
  /** `ranked` as the rules weigh it. */
  private[identity] def weighing(ranked: Seq[Scored]): Weighing = new Weighing(ranked, weights)

  // ── running the table ────────────────────────────────────────────────────────────────────────
  /** Why each rule a node may be taken by alone refused it: `(rule, the first condition that stopped it)`. */
  def refusals(ranked: Seq[Scored]): Seq[DecisionTrace.Refusal] =
    if (billsSeveralWorks(ranked)) Seq(DecisionTrace.Refusal("alone", "bills several works"))
    else {
      val weighed = weighing(ranked)
      aloneRules.flatMap(rule => rule.accepts(weighed, Vetoes).left.toOption.map(refused =>
        DecisionTrace.Refusal(rule.name, refused.why, refused.film, refused.detail)))
    }
  /** The rules a cluster is taken by on its pooled evidence, by name in the order they are tried — after
   *  `season-production`, which pre-empts them: what `identity-resolver.md` §21 lists, a spec holding the two together
   *  (the alone rules' order it reads off [[refusals]]). */
  private[identity] def pooledOrder: Seq[String] = "season-production" +: pooledRules.map(_.name)
  /** What the rule called `name` says of `ranked`, its vetoes asked — for a spec holding one rule to its cases. */
  private[identity] def ruleVerdict(name: String)(ranked: Seq[Scored]): Verdict =
    (aloneRules ++ pooledRules).find(_.name == name).getOrElse(throw new NoSuchElementException(s"no rule $name")).accepts(weighing(ranked), Vetoes)
  /** What the rule called `name` alone takes of `ranked`. */
  private[identity] def byRule(name: String)(ranked: Seq[Scored]): Option[Accepted] = ruleVerdict(name)(ranked).toOption
  /** What a node accepts ON ITS OWN, with the rule that took it — only when its own facts favour it over the runner-up:
   *  a bare "Lalka" beside two 2026 "Lalka"s, told apart only by TMDB's popularity ranking, is not decided alone — it
   *  follows the film its title's credited siblings chose (the cluster's), or the pooled vote — unless it is the
   *  listing's exact top hit, which is measured as a class. Every own match names the rule that took it, however far
   *  under the calibrated cut its probability stands ([[Acceptance.ruleTaking]]). A family's scope asks it once per
   *  node ([[FamilyScope.takenAlone]]). */
  def aloneNamed(ranked: Seq[Scored]): Option[(Accepted, String)] = {
    val weighed = weighing(ranked)
    takenBy(ranked, keep = { case ((taken, _), rule) => rule == "imdb-suggested" || !outweighedBySuggestion(ranked, taken) })(
      stageRows.preemptingAlone(weighed).orElse(firstOf(weighed, aloneRules)))
  }
  /** Does an undenied film only IMDb's suggestion reached ([[Scored.suggestedOnly]], which only the IMDb rule may take),
   *  that another venue's listing of the title corroborates, weigh at least as much on the evidence as `taken`? Then the
   *  evidence does not single `taken` out, and no other rule takes it: PL Kino Kryterium's "DKF: Ktoś całkiem obcy" took
   *  TMDB's Polish title of Foley's 2007 "Perfect Stranger" at 46.3% beside Andersen's 2025 "I Was a Stranger" at 47.8%
   *  — which TMDB holds under its Arabic title, and Kino Chatka Żaka's listing crediting him corroborates — the film
   *  Kryterium shows. An uncorroborated suggestion is no such rival: IMDb suggests a title's best-known films, and DE
   *  "Béni`s Noise", "Escape to Life" and a dozen more lost their own films to one. */
  private[identity] def outweighedBySuggestion(ranked: Seq[Scored], taken: Scored): Boolean =
    ranked.exists(other => (other ne taken) && other.suggestedOnly && !other.denied && other.number("venues.corroborating").exists(_ > 0) &&
      other.probability >= taken.probability)
  /** What a cluster's POOLED scoring accepts: its season production, its exact top hit, or the
   *  best eligible candidate the calibration accepts that no namesake out-fits ([[UnrivalledCalibrated]]).
   *  Each the edition of it the listing names, if any ([[AcceptanceTitleRules.editionNamed]]). */
  def pooled(ranked: Seq[Scored]): Option[Accepted] = pooledNamed(ranked).map(_._1)
  /** [[pooled]], with the rule that accepted — for the decision's trace. */
  def pooledNamed(ranked: Seq[Scored]): Option[(Accepted, String)] =
    // a relay's broadcast-season record pre-empts the pooled rules as it does the alone ones: a cluster of "MET: Macbeth"
    // spellings took the Met's 2014 staging by a pooled rule after the alone path ruled it another broadcast. Never
    // stage-production pooled: one member's stated year is not the cluster's broadcast — it took UK Aldeburgh's
    // "Royal Ballet: The Nutcracker" [2025] as the 2025/26 record, and split Savoy Worksop's "MET:Manon" off the Met's
    takenBy(ranked) {
      val weighed = weighing(ranked)
      stageRows.preemptingPooled(weighed)
        .orElse(if (Acceptance.tiedAtTop(ranked)) None else firstOf(weighed, pooledRules))
    }
  /** What both ways of taking a film share: nothing for a bill of several works; the season's production when there is
   *  one (it pre-empts every rule, and a season production refused takes nothing), else what `rules` take — either only
   *  if `keep` keeps it — each the edition of it the listing names, and never a film the bill's word refuses
   *  ([[takesNoBill]]). */
  private[identity] def takenBy(ranked: Seq[Scored], keep: ((Accepted, String)) => Boolean = _ => true)
                     (rules: => Option[(Accepted, String)]): Option[(Accepted, String)] =
    if (billsSeveralWorks(ranked)) None
    else stageRows.seasonProductionTaken(ranked).map(_.map(_ -> "season-production")).getOrElse(rules).filter(keep).filter(_._1._1.untaken.isEmpty)
      .map { case (accepted, rule) => titleRules.editionNamed(ranked)(accepted) -> rule }.filter(takesNoBill(ranked))
  /** The probability that `film` is the listing's film — the decision's confidence, on the scale
   *  the rating gate reads: the calibrated one (rivals are in it, the `rivals` measure), its
   *  evidence class's when the film is the listing's accepted exact top hit, or the one the priors
   *  lent when the listing's facts accepted it ([[Calibrated]]). */
  def confidenceOf(ranked: Seq[Scored], film: Int): Double =
    table.topHit(weighing(ranked)).filter(_._1.candidate.tmdbId == film).map(_._2)
      .orElse(table.calibrated(weighing(ranked)).toOption.filter(_._1.candidate.tmdbId == film).map(_._2))
      .orElse(pooled(ranked).filter(_._1.candidate.tmdbId == film).map(_._2))
      .getOrElse(ranked.filter(AcceptanceRule.isEligible).find(_.candidate.tmdbId == film).fold(0.0)(_.probability))
  /** Why an own match's confidence stands above its calibrated probability: its exact top hit's
   *  class, or the ranking priors lending ([[EvidenceWeights.priorsLent]]) — naming each rival the release veto took
   *  out of the class it was accepted by ([[ReleaseVeto]]). */
  def liftedBy(ranked: Seq[Scored], scored: Scored, confidence: Double): String =
    if (confidence <= scored.probability) ""
    else (if (table.topHit(weighing(ranked)).exists(_._1.candidate.tmdbId == scored.candidate.tmdbId)) " as its exact top hit"
      else " with the ranking priors lending, never withdrawing") +
      (if (scored.vetoedRivals == 0) "" else ranked.flatMap(rival => rival.denial.filter(_.isReleaseVeto)
        .map(why => s"rival ${rival.candidate.tmdbId} $why")).mkString(" (", "; ", ")"))

  /** Is the listing a bill of several works, which no rule takes a film of ([[AcceptanceBillRules.billsSeveralWorks]])? */
  def billsSeveralWorks(ranked: Seq[Scored]): Boolean = billRules.billsSeveralWorks(ranked)
  /** Does the listing's page read as reused for `scored` ([[AcceptanceVetoRules.copiedBeyondItsBilling]])? */
  private[identity] def copiedBeyondItsBilling(scored: Scored): Boolean = vetoRules.copiedBeyondItsBilling(scored)
  /** The film a node's evidence leans to though no rule took it ([[AcceptanceFallbacks.leaning]]). */
  def leaning(ranked: Seq[Scored]): Option[Scored] = fallbacks.leaning(ranked)
  /** The film a cluster no TMDB film was taken for falls back to ([[AcceptanceFallbacks.fallback]]). */
  def fallback(ranked: Seq[Scored]): Option[Scored] = fallbacks.fallback(ranked)
}

private[identity] object Acceptance {
  /** Do the two best TAKEABLE candidates of `ranked` score alike (within float noise)? Then the pooled vote takes
   *  neither: a tie the pooled population makes is no evidence for either, and any tie-break (the lower id, the title
   *  covering more of the listing's) takes a film only the arrival states that tie it pick -- DE Metropol-Kino-Center's
   *  bare "Madoka Magica - Der Film: Rebellion" went 212162 -> 42997 -> 212162 as Filmhaus's listing left and came back,
   *  1962's "Rebellion" and the 2013 Madoka film at 46.3% each. Unmatched beats a guess; the listing's own facts, or a
   *  pool that separates them, decide. */
  private[identity] def tiedAtTop(ranked: Seq[Scored]): Boolean =
  {
    val takeable = ranked.filter(_.takeable).map(_.probability)
    takeable.maxOption.exists(top => takeable.count(probability => top - probability < TieTolerance) > 1)
  }
  /** Two pooled probabilities closer than this are one score: the pool sums in arrival order. */
  private[identity] val TieTolerance = 1e-9

  /** The rule a node's alone acceptance (`aloneNamed`'s answer) took `film` by, if it took that film. */
  def ruleTaking(taken: Option[(Accepted, String)], film: Int): Option[String] =
    taken.collect { case ((scored, _), rule) if scored.candidate.tmdbId == film => rule }
  /** The condition that stopped a rule (`why`, a fixed phrase: the trace's rule id), the candidate it was weighing
   *  when it did, and what that candidate's evidence said there — the facts against it, its probability against the
   *  cut, the rival it lost to. */
  final case class Refused(why: String, film: Option[Int] = None, detail: String = "")
  object Refused {
    /** A candidate as a refusal names it: "1365683 Primavera (2025)". */
    def named(scored: Scored): String =
      s"${scored.candidate.tmdbId} ${scored.candidate.film.title}${scored.candidate.film.year.fold("")(year => s" ($year)")}"
  }
}
