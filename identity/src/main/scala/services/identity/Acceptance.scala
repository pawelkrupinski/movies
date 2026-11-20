package services.identity

import services.identity.AcceptanceRule.Weighing
import services.identity.measures.IdentityMeasures.ListingFilm
import services.identity.measures.Scored.Accepted
import services.identity.measures.{DecisionTrace, IdentityMeasures, Measure, Scored}

/** Which of a listing's scored candidates — ranked best first, the denied ones among them — it TAKES, and at what
 *  confidence: [[aloneNamed]] for a node on its own evidence, [[pooled]] for a cluster's pooled evidence. Every rule is
 *  a row of [[AcceptanceRuleTable]], each a function of the ranked candidates alone, tried in the table's order, the
 *  first to accept deciding; the listing's season production, when a record names it, pre-empts them all
 *  ([[seasonProduction]]). A new rule is a new row there, its questions named in the rule file of its kind (title,
 *  facts, calibration, stage, vetoes, bills); this class only runs the table. */
private[identity] final class Acceptance(private[identity] val calibration: IdentityCalibration)
    extends AcceptanceRuleTable, AcceptanceTitleRules, AcceptanceCalibratedRules, AcceptanceFactRules, AcceptanceStageRules,
      AcceptanceVetoRules, AcceptanceBillRules {

  val weights = new EvidenceWeights(calibration)
  import weights.{against, fitsBetter, own, speaksAgainst}
  import Acceptance.Refused

  /** A rule's verdict: the film it accepts, or the first of its steps that refused — the identity trace's
   *  `refused:<rule>:<why>`, so a listing no rule took says which condition stopped each — with the candidate the rule
   *  was weighing when it did and what that candidate's evidence said ([[Acceptance.Refused]]). */
  private[identity] type Verdict = AcceptanceRule.Verdict

  /** Neither denied nor reached only by IMDb's suggestion ([[Scored.suggestedOnly]]): a candidate the rules may take. */
  private[identity] def isEligible(scored: Scored): Boolean = AcceptanceRule.isEligible(scored)
  private[identity] def eligibleOf(ranked: Seq[Scored]): Seq[Scored] = ranked.filter(isEligible)
  /** `ranked` as the rules weigh it. */
  private[identity] def weighing(ranked: Seq[Scored]): Weighing = new Weighing(ranked, weights)

  // ── what a rule takes its candidate at ──────────────────────────────────────────────────────

  /** At its own probability. */
  private[identity] lazy val atItsOwn: AcceptanceRule.Take = (_, scored) => Right(atOwn(scored))
  private[identity] def atOwn(scored: Scored): Accepted = scored -> scored.probability
  /** At what the priors lend the best eligible candidate ([[AcceptanceRule.Weighing.lentToTheBest]]). */
  private[identity] lazy val atWhatThePriorsLend: AcceptanceRule.Take = (weighing, best) => Right(best -> weighing.lentToTheBest)
  /** At what its evidence CLASS measured, its evidence read as `measures` makes it ([[classAccepted]]). */
  private[identity] def atItsClass(measures: Scored => Map[String, Measure]): AcceptanceRule.Take =
    (_, scored) => classAccepted(scored, measures(scored))
  /** UNOPPOSED, read as `weighed` makes it (the candidate read as the rule reads its title): nothing it publishes speaks
   *  against it so read, no other of `among` fits its facts so read better, and its evidence class clears the cut
   *  ([[classAccepted]]). */
  private[identity] def unopposedAs(weighed: Scored => Scored, among: Weighing => Seq[Scored] = _.eligible): AcceptanceRule.Take =
    (weighing, scored) => {
      val read = weighed(scored)
      if (speaksAgainst(read)) Left(Refused("a published fact weighs against it", Some(scored.candidate.tmdbId), against(read).mkString(" ")))
      else among(weighing).find(rival => (rival ne scored) && fitsBetter(rival, read))
        .fold(classAccepted(scored, read.measures))(rival => Left(Refused("a rival fits its facts better", Some(scored.candidate.tmdbId), Acceptance.named(rival))))
    }
  /** `scored` at what its evidence CLASS measured (`IdentityCalibration.classProbability`), lending and never withdrawing —
   *  its class read with the namesakes the release veto took away counted as no rivals ([[Scored.unvetoed]]). */
  private[identity] def classAccepted(scored: Scored, measures: Map[String, Measure]): Verdict =
    calibration.classProbability(ListingFilm, scored.unvetoed(measures)).toRight(Refused("no evidence class measured for it", Some(scored.candidate.tmdbId)))
      .flatMap { classProbability =>
        val lent = math.max(scored.probability, classProbability)
        Either.cond(calibration.showsRatings(lent), scored -> lent, Refused("below the rating cut", Some(scored.candidate.tmdbId), cut(lent)))
      }

  // ── what the rows' refusals say ──────────────────────────────────────────────────────────────

  private[identity] def cut(probability: Double) = f"${probability * 100}%.1f%% < ${calibration.ratingCut * 100}%.1f%%"
  /** What a listing's facts leave a director's rule to go on: its credit, or that it credits nobody. */
  private[identity] def credited(ranked: Seq[Scored]): String =
    ranked.headOption.map(_.listing.directors).filter(_.nonEmpty).fold("the listing credits no director")(names => s"credits ${names.mkString(", ")}")
  private[identity] def firstHit(ranked: Seq[Scored]): String = ranked.find(_.rank.contains(1)).fold("its search returned nothing")(hit => s"first hit ${Acceptance.named(hit)}")
  /** Why the best candidate is not eligible, if it is denied. */
  private[identity] def bestDenied(weighing: Weighing): String = weighing.ranked.headOption.flatMap(_.denial).fold("")(denial => s"best denied: $denial")
  private[identity] def titleRelation(scored: Scored): String = s"title=${scored.category("title").getOrElse("absent")}"
  private[identity] def titleYearDelta(scored: Scored): String = scored.number("titleYear.delta").fold("")(delta => f"titleYear.delta=$delta%.0f")
  private[identity] def numeral(scored: Scored): String = s"numeral=${scored.category("numeral").getOrElse("")}"

  // ── questions several rules ask ──────────────────────────────────────────────────────────────

  private[identity] def otherInstalment(scored: Scored) = scored.category("numeral").exists(IdentityMeasures.OtherInstalment)
  private[identity] def joinedToAnotherWork(weighing: Weighing, scored: Scored): Boolean =
    IdentityMeasures.joinsAnotherWork(weighing.listing, scored.candidate.film)
  /** `found` without its HOLLOW records — no year, no running time — when it holds a full one beside them: TMDB's empty
   *  duplicate of a film ("Solo", 1450958, beside Sophie Dupuis's 2023 "Solo") is no second film of its director. */
  private[identity] def withoutHollow(found: Seq[Scored]): Seq[Scored] = {
    def hollow(scored: Scored) = scored.candidate.film.year.isEmpty && scored.candidate.film.runtime.forall(_ <= 0)
    val full = found.filterNot(hollow)
    if (full.nonEmpty && full.size < found.size) full else found
  }

  // ── running the table ────────────────────────────────────────────────────────────────────────

  /** The first of `rules` to accept, with its name. */
  private[identity] def firstOf(weighing: Weighing, rules: List[AcceptanceRule]): Option[(Accepted, String)] =
    rules.iterator.filterNot(rule => Acceptance.ablated(rule.name)).map(rule => rule.accepts(weighing, Vetoes).map(_ -> rule.name))
      .collectFirst { case Right(taken) => taken }

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

  /** The listing's exact top hit ([[ExactTopHit]]), asked only whether its title numbers another instalment: what the
   *  decision's confidence reads. */
  private[identity] def topHit(ranked: Seq[Scored]): Option[Accepted] = ExactTopHit.accepts(weighing(ranked), List(NumbersItsOwnInstalment)).toOption
  /** The best eligible candidate the calibration's cut passes ([[Calibrated]]). */
  private[identity] def calibrated(ranked: Seq[Scored]): Verdict = Calibrated.verdict(weighing(ranked))

  /** What a node accepts ON ITS OWN, with the rule that took it — only when its own facts favour it over the runner-up:
   *  a bare "Lalka" beside two 2026 "Lalka"s, told apart only by TMDB's popularity ranking, is not decided alone — it
   *  follows the film its title's credited siblings chose (the cluster's), or the pooled vote — unless it is the
   *  listing's exact top hit, which is measured as a class. Every own match names the rule that took it, however far
   *  under the calibrated cut its probability stands ([[Acceptance.ruleTaking]]). A family's scope asks it once per
   *  node ([[FamilyScope.takenAlone]]). */
  def aloneNamed(ranked: Seq[Scored]): Option[(Accepted, String)] = {
    val weighed = weighing(ranked)
    takenBy(ranked, keep = { case ((taken, _), rule) => rule == "imdb-suggested" || !outweighedBySuggestion(ranked, taken) })(
      relayOrBroadcastSeasonRecord(weighed).filterNot { case (_, rule) => Acceptance.ablated(rule) }.orElse(firstOf(weighed, aloneRules)))
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
   *  Each the edition of it the listing names, if any ([[editionNamed]]). */
  def pooled(ranked: Seq[Scored]): Option[Accepted] = pooledNamed(ranked).map(_._1)

  /** [[pooled]], with the rule that accepted — for the decision's trace. */
  def pooledNamed(ranked: Seq[Scored]): Option[(Accepted, String)] =
    // a relay's broadcast-season record pre-empts the pooled rules as it does the alone ones: a cluster of "MET: Macbeth"
    // spellings took the Met's 2014 staging by a pooled rule after the alone path ruled it another broadcast. Never
    // stage-production pooled: one member's stated year is not the cluster's broadcast — it took UK Aldeburgh's
    // "Royal Ballet: The Nutcracker" [2025] as the 2025/26 record, and split Savoy Worksop's "MET:Manon" off the Met's
    takenBy(ranked) {
      val weighed = weighing(ranked)
      broadcastSeasonRecord(weighed).map(_ -> "season-record").filterNot(_ => Acceptance.ablated("season-record"))
        .orElse(if (Acceptance.tiedAtTop(ranked)) None else firstOf(weighed, pooledRules))
    }

  /** What both ways of taking a film share: nothing for a bill of several works; the season's production when there is
   *  one (it pre-empts every rule, and a season production refused takes nothing), else what `rules` take — either only
   *  if `keep` keeps it — each the edition of it the listing names, and never a film the bill's word refuses
   *  ([[takesNoBill]]). */
  private[identity] def takenBy(ranked: Seq[Scored], keep: ((Accepted, String)) => Boolean = _ => true)
                     (rules: => Option[(Accepted, String)]): Option[(Accepted, String)] =
    if (billsSeveralWorks(ranked)) None
    else seasonProduction(ranked).filterNot(_ => Acceptance.ablated("season-production")).map(_.map(_ -> "season-production")).getOrElse(rules).filter(keep).filter(_._1._1.untaken.isEmpty)
      .map { case (accepted, rule) => editionNamed(ranked)(accepted) -> rule }.filter(takesNoBill(ranked))

  /** The probability that `film` is the listing's film — the decision's confidence, on the scale
   *  the rating gate reads: the calibrated one (rivals are in it, the `rivals` measure), its
   *  evidence class's when the film is the listing's accepted exact top hit, or the one the priors
   *  lent when the listing's facts accepted it ([[Calibrated]]). */
  def confidenceOf(ranked: Seq[Scored], film: Int): Double =
    topHit(ranked).filter(_._1.candidate.tmdbId == film).map(_._2)
      .orElse(calibrated(ranked).toOption.filter(_._1.candidate.tmdbId == film).map(_._2))
      .orElse(pooled(ranked).filter(_._1.candidate.tmdbId == film).map(_._2))
      .getOrElse(eligibleOf(ranked).find(_.candidate.tmdbId == film).fold(0.0)(_.probability))

  /** Why an own match's confidence stands above its calibrated probability: its exact top hit's
   *  class, or the ranking priors lending ([[EvidenceWeights.priorsLent]]) — naming each rival the release veto took
   *  out of the class it was accepted by ([[ReleaseVeto]]). */
  def liftedBy(ranked: Seq[Scored], scored: Scored, confidence: Double): String =
    if (confidence <= scored.probability) ""
    else (if (topHit(ranked).exists(_._1.candidate.tmdbId == scored.candidate.tmdbId)) " as its exact top hit"
      else " with the ranking priors lending, never withdrawing") +
      (if (scored.vetoedRivals == 0) "" else ranked.flatMap(rival => rival.denial.filter(_.isReleaseVeto)
        .map(why => s"rival ${rival.candidate.tmdbId} $why")).mkString(" (", "; ", ")"))

  // ── what pre-empts the rules, and what follows a take ────────────────────────────────────────

  /** The eligible candidate whose record names the listing's SEASON PRODUCTION — the season and
   *  the work its title names. `None`: no candidate does. `Some(Some(x))`: `x` is the listing's
   *  film on that identity, whatever the calibrated probability (the fitted weights do not read a
   *  season yet): a season names its production as a published year names a film, and the
   *  namesakes it rules out are already denied (`ListingConstraints.seasonsApart`).
   *  `Some(None)`: two records do (two houses' stagings of one work in one season) and the
   *  listing's own facts favour neither — ambiguity, so nothing is taken, on the season or on
   *  the database's ranking. The confidence stays the calibrated probability. */
  def seasonProduction(ranked: Seq[Scored]): Option[Option[Accepted]] =
    ranked.filter(scored => !scored.denied && scored.seasonProduction).sortBy(scored => (-own(scored), scored.candidate.tmdbId)) match {
      case Seq()       => None
      case Seq(one)    => Some(Some(atOwn(one)))
      case first +: second +: _ => Some(Option.when(own(first) > own(second))(atOwn(first)))
    }

  /** The EDITION of the accepted film that the listing's whole title names, when there is exactly
   *  one: a later record carrying the film's title under a qualifier (`IdentityMeasures.editionOf`
   *  — "Radiohead X Nosferatu: A Symphony of Horror" of Murnau's "Nosferatu"), which the listing
   *  names by its whole title MORE as its own than it names the film ([[Acceptance.namedAs]]). The
   *  listing's facts chose the work, and an edition carries its work's facts — the venue credits
   *  Murnau, TMDB the edition's maker — so they do not deny the edition; a pin still does. With the
   *  confidence of the work. The film itself when the listing names it as its own — by its title
   *  or its own original title — or no edition, or two.
   *
   *  A work the title names only by one of its ALTERNATIVE titles, while another record carries
   *  that title as its own, is named as closely by the title as that record: TMDB files "Caligula:
   *  The Ultimate Cut" among the 1979 "Caligula"'s alternatives beside the re-cut's own record, and
   *  "Nosferatu: A Symphony of Horror" among Murnau's beside David Lee Fisher's 2023 remake. Then
   *  the record is an edition by the work's OWN titles, and the listing's credit decides: a record
   *  crediting another person than the listing's is another film, one crediting nobody else is the
   *  work's edition — its running time is the cut's own. */
  def editionNamed(ranked: Seq[Scored])(accepted: Accepted): Accepted = {
    val (work, confidence) = accepted
    val named = Acceptance.namedAs(work.category("title"))
    // The listing's own original title naming the work whole names it too ("Die Puppe", "Lalka").
    if (named == Acceptance.NamedAsItsOwn || work.category("originalTitle").contains("match")) accepted
    else ranked.filter(edition => (edition ne work) && !edition.deniedByPin && namesTheEdition(work, named)(edition)) match {
      case Seq(edition) => edition -> confidence
      case _            => accepted
    }
  }

  /** Does the listing's title, naming `work` as `named` ([[Acceptance.namedAs]]), name `edition` more as its own, the
   *  record an edition of the work ([[editionNamed]]): by any of the work's titles where the title names it by none of
   *  them, else by the work's own titles alone and crediting no other person than the listing's? */
  private def namesTheEdition(work: Scored, named: Int)(edition: Scored): Boolean =
    Acceptance.namedAs(edition.category("title")) > named && (
      if (named == 0) IdentityMeasures.editionOf(edition.candidate.film, work.candidate.film)
      else IdentityMeasures.editionOf(edition.candidate.film, work.candidate.film.copy(alternativeTitles = Nil)) &&
        !edition.category("director").contains("different"))
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

  /** Is the acceptance rule called `rule` switched off for an ablation run ([[RuleAblation]], as `accept:<rule>`)? */
  def ablated(rule: String): Boolean = RuleAblation.disables("accept:" + rule)

  /** The rule a node's alone acceptance (`aloneNamed`'s answer) took `film` by, if it took that film. */
  def ruleTaking(taken: Option[(Accepted, String)], film: Int): Option[String] =
    taken.collect { case ((scored, _), rule) if scored.candidate.tmdbId == film => rule }
  /** The condition that stopped a rule (`why`, a fixed phrase: the trace's rule id), the candidate it was weighing
   *  when it did, and what that candidate's evidence said there — the facts against it, its probability against the
   *  cut, the rival it lost to. */
  final case class Refused(why: String, film: Option[Int] = None, detail: String = "")

  /** How many times its runner-up's probability the film a no-match leans to holds ([[Acceptance.leaning]]): a tie
   *  between namesakes is no lean. */
  val LeanMargin = 1.25
  /** A candidate as a refusal names it: "1365683 Primavera (2025)". */
  def named(scored: Scored): String =
    s"${scored.candidate.tmdbId} ${scored.candidate.film.title}${scored.candidate.film.year.fold("")(year => s" ($year)")}"

  /** How much as ITS OWN a title relation names a record: by the record's own title or original
   *  title ([[NamedAsItsOwn]]), by one of the alternative titles the database files beside them,
   *  or not whole (0). */
  val NamedAsItsOwn = 2
  def namedAs(titleRelation: Option[String]): Int = titleRelation match {
    case Some("exact") | Some("original") => NamedAsItsOwn
    case Some("alternative")              => 1
    case _                                => 0
  }
}
