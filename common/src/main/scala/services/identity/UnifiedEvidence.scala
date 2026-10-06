package services.identity

import play.api.libs.json.{Json, OFormat}
import services.identity.agreement.{Agreement, AgreementStage, Broadcast, FamilyPick, FamilyVerdict, SourceRecord, VoterFamily}

/**
 * The UNIFIED evidence model's signals: every signal the resolver's model and the agreement stage after it read about
 * one film a cluster's evidence reaches (a CONTENDER), as one feature vector — the calibrated model's probability and
 * the rule that accepted a film, the TMDB lean and best candidate, each film database family's take and lean, the
 * venue's own year, director and running time, the venues billing it, the title's guards, and the venue posters
 * (docs/design/identity-resolver.md §20, the inventory). Weighed by [[UnifiedWeights]], FITTED (`scripts.IdentityUnifiedFit`
 * over `integration.IdentityUnifiedDataset`'s rows), never hand-set; each signal's DIRECTION is known (a family's take
 * only ever speaks for a film), and the fit holds every weight to it.
 *
 * Phase 1 measures it beside today's stack; nothing decides by it yet.
 */
object UnifiedEvidence {

  /** A signal: its feature name, the group an ablation drops it with, and its direction (+1 speaks for the film, −1
   *  against it, 0 either way). */
  final case class Signal(name: String, group: String, direction: Int)

  private def familySignals(kind: String): Seq[Signal] = VoterFamily.values.toSeq.map(f => Signal(s"family.${f.label}.$kind", s"family-$kind", 1))

  val Signals: Seq[Signal] = Seq(
    Signal("model.logit", "model", 1), Signal("model.unscored", "model", 0), Signal("model.deniedBySome", "model", -1),
    Signal("rule.title", "rules", 1), Signal("rule.director", "rules", 1), Signal("rule.calibrated", "rules", 1), Signal("rule.imdb", "rules", 1),
    Signal("rule.stage", "rules", 1), Signal("rule.pooled", "rules", 1),
    Signal("model.lean", "model-vote", 1), Signal("model.best", "model-vote", 1),
    Signal("production.season", "productions", 1), Signal("production.house", "productions", 1)) ++
    familySignals("took") ++ familySignals("leans") ++ Seq(
    Signal("family.turnedDown", "family-against", -1), Signal("family.dissent", "family-against", -1),
    Signal("listing.facts", "listing", 1), Signal("listing.runtime", "listing", 1), Signal("listing.contradicts", "listing", -1),
    Signal("venues.current", "venues", 1),
    Signal("title.namesIt", "title", 1), Signal("title.anothersOwn", "title", -1), Signal("title.namesNone", "title", -1),
    Signal("edition.apart", "title", -1),
    Signal("bill.several", "bills", -1), Signal("stage.work", "bills", -1),
    Signal("poster.match", "poster", 1), Signal("poster.near", "poster", 1), Signal("poster.otherMatches", "poster", -1),
    Signal("agreement.quorum", "stage-verdicts", 1), Signal("poster.vote", "stage-verdicts", 1),
    Signal("broadcast.take", "stage-verdicts", 1), Signal("listing.catalogue", "listing", 1),
    // the COUNTS and conjunctions the agreement's thresholds read (Quorum, the rise per dissenting family) —
    // what an additive score over per-family indicators cannot express
    Signal("count.takers", "counts", 1), Signal("count.leaning", "counts", 1), Signal("count.takersLessDissent", "counts", 1),
    Signal("count.takers2", "counts", 1), Signal("count.takers3", "counts", 1),
    Signal("and.takersNoDissent", "conjunctions", 1), Signal("and.factsTakers", "conjunctions", 1),
    Signal("and.posterTakers", "conjunctions", 1), Signal("and.leanTakers", "conjunctions", 1))
  val Names: Seq[String] = Signals.map(_.name)

  /** The signals only a PINNED rule reads ([[UnifiedRules.pinned]]): carried by every contender beside [[Signals]], never a
   *  fitted feature — the rows `scripts.IdentityUnifiedFit` reads hold [[Names]] alone.
   *  `families.current`: two families take the film, one of them a film database's, released this year or last — the
   *  families' counterpart of `venues.current` (PL "TAJNY AGENT": IMDb and Filmweb take Mendonça Filho's 2025 film,
   *  Wikidata the 1936 namesake).
   *  `family.facts`: a film database's family takes the film AND the venue's own year and director credit it
   *  (`listing.facts`) — two independent sources naming one film, where either alone is a weak signal: a single family's
   *  take as a fill adds wrong films (§20.6), and a venue's facts alone name no film (PL "11. UFF - Dowżenko. Pierwsze
   *  spojrzenie": Wikidata takes Konovalov's 2026 film, the festival dates and credits it; docs/design/identity-resolver.md §20.12). */
  val PinnedSignals: Seq[String] = Seq("families.current", "family.facts")

  /** The NON-COMPENSATORY guards: a contender one of these fires on is no film to take, whatever else speaks for it — the
   *  agreement stage's vetoes (several works billed, a stage work, another film's own title, the
   *  venue's year or director against it, a venue poster naming another candidate, a family turning it down; and, as
   *  the agreement requires of what it takes, a title naming the film, and no other edition than the title numbers). A film
   *  the model's own rules accepted ([[ModelRules]]) passed the model's guards instead ([[Acceptance]]), as today. */
  val Guards: Seq[String] = Seq("bill.several", "stage.work", "title.anothersOwn", "listing.contradicts", "poster.otherMatches",
    "family.turnedDown", "title.namesNone", "edition.apart")
  val ModelRules: Seq[String] = Names.filter(_.startsWith("rule."))

  /** The guards `features` trips: none for a film the model's rules took. */
  def vetoes(features: String => Double): Seq[String] =
    if (ModelRules.exists(features(_) > 0)) Nil else Guards.filter(features(_) != 0)

  /** The rule groups an acceptance rule's name falls in ([[Acceptance]]'s rules, by the name a trace gives them). */
  val RuleGroups: Map[String, String] = Map(
    "exact-top-hit" -> "rule.title", "segment-top-hit" -> "rule.title", "sole-result" -> "rule.title", "sole-work" -> "rule.title",
    "dated-title" -> "rule.title", "directors-work" -> "rule.director", "directors-title" -> "rule.director",
    "favoured-calibrated" -> "rule.calibrated", "unrivalled-calibrated" -> "rule.calibrated", "imdb-suggested" -> "rule.imdb",
    "house-production" -> "rule.stage", "stage-production" -> "rule.stage", "season-record" -> "rule.stage", "season-production" -> "rule.stage")

  /** The calibrated probability as a feature: its log-odds, clamped at ±9.2 (1e-4) and rounded to a quarter, so rows the
   *  fit reads fold into few distinct ones. */
  def logit(probability: Double): Double = {
    val p = math.min(1 - 1e-4, math.max(1e-4, probability))
    math.rint(math.log(p / (1 - p)) * 4) / 4
  }

  /** What a cluster's evidence holds: its listings, the model's decision on it, its nodes' scored candidates, each
   *  family's verdict (the families that answered), each venue poster's nearest distance to each TMDB film hashed
   *  (empty where none), the TMDB film an IMDb id finds, and the year it is now. */
  final case class ClusterEvidence(listings: Seq[Listing], decision: ResolverDecision, nodes: Seq[IdentityResolver.NodeEvidence],
                                   verdicts: Seq[FamilyVerdict], posters: Seq[Map[Int, Option[Int]]], tmdbOf: String => Option[Int],
                                   thisYear: Int, stated: Seq[Listing] = Nil, measured: Option[Listing => IdentityMeasures.Listing] = None)

  /** One film the evidence reaches: `tmdb:<id>` or, a film only other databases hold, `imdb:<tt>`; its title, each family
   *  that took it with its own id of it (`"rt" -> "dune_2021"`, an id only, never read as evidence), and every signal that
   *  is not 0. */
  final case class Contender(film: String, tmdb: Option[Int], imdb: Option[String], title: String, familyIds: Map[String, String],
                             signals: Map[String, Double])

  private final class Building(val tmdb: Option[Int], val imdb: Option[String], val record: SourceRecord) {
    val records = scala.collection.mutable.ArrayBuffer(record)
    val ids     = scala.collection.mutable.Map.empty[String, String]
    def film: String = tmdb.fold(imdb.fold("")(id => s"imdb:$id"))(id => s"tmdb:$id")
    def is(other: SourceRecord): Boolean = records.exists(Agreement.sameFilm(_, other))
  }

  /** Every film the cluster's evidence reaches — the TMDB candidates some node scored and did not deny, and each film a
   *  family took or leans to (joined to a TMDB candidate by a shared id, else by its facts, as the agreement joins two
   *  families' picks) — with its signals. A family's film no id names is none: no film can be shown for it. */
  def contenders(c: ClusterEvidence): Seq[Contender] = {
    val scored   = c.nodes.flatMap(_.candidates).filterNot(candidate => FallbackIds.isFallback(candidate.tmdbId)).groupBy(_.tmdbId)
    val eligible = scored.filter(_._2.exists(!_.denied)).toSeq.sortBy(_._1)
    def tmdbRecord(id: Int, film: IdentityMeasures.Film) =
      SourceRecord(film, Map("tmdb" -> id.toString) ++ Option.when(film.imdbNumber > 0)("imdb" -> f"tt${film.imdbNumber}%07d"))
    val built = scala.collection.mutable.ArrayBuffer.from(eligible.map { case (id, candidates) =>
      val film = candidates.head.film
      new Building(Some(id), Option.when(film.imdbNumber > 0)(f"tt${film.imdbNumber}%07d"), tmdbRecord(id, film))
    })
    // the families' films first, each family's record joined to every other naming the same film (as the agreement joins
    // picks): a film IMDb names by its id alone and Wikidata by its TMDB id is one film, the TMDB candidate both name
    val familyRecords = c.verdicts.sortBy(_.family.ordinal).flatMap(v => v.pick.map(pick => pick.record -> Option(pick)) ++ v.leaning.map(_ -> None))
    val familyFilms = familyRecords.foldLeft(Vector.empty[Vector[(SourceRecord, Option[FamilyPick])]]) { (films, entry) =>
      val (joined, apart) = films.partition(_.exists { case (record, _) => Agreement.sameFilm(record, entry._1) })
      apart :+ (joined.flatten :+ entry)
    }
    familyFilms.foreach { members =>
      val records = members.map(_._1)
      val imdb = records.flatMap(_.crossIds.get("imdb")).headOption
      val tmdb = records.flatMap(_.crossIds.get("tmdb").flatMap(_.toIntOption)).headOption.orElse(records.flatMap(_.crossIds.get("imdb")).flatMap(c.tmdbOf).headOption)
      built.find(b => records.exists(b.is)).orElse(tmdb.flatMap(id => built.find(_.tmdb.contains(id)))).orElse(
        Option.when(tmdb.isDefined || imdb.isDefined)(new Building(tmdb, imdb, records.head)).map { fresh => built += fresh; fresh }
      ).foreach { film =>
        records.filterNot(record => film.records.exists(_ eq record)).foreach(film.records += _)
        members.flatMap(_._2).foreach(pick => film.ids(pick.family.label) = pick.id)
      }
    }
    // the agreement's own verdict and the posters' vote, as the stage reaches them: signals beside the evidence they rest on
    val modelVote = c.decision.leaning.orElse(c.decision.candidate).map(lean =>
      scored.get(lean.film).map(cs => tmdbRecord(lean.film, cs.head.film)).getOrElse(AgreementStage.leanRecord(lean)))
    // as the stage reads it: the listings as their venues' own pages state them, and a film only review sites take is none
    val agreed    = (if (c.decision.film.isDefined) None else Agreement.agreed(c.listings, c.verdicts, modelVote, Some(c.thisYear), c.stated))
      .filter(_.families.exists(_.namesFilms))
    val asStated  = if (c.stated.nonEmpty) c.stated else c.listings
    // the stage relay's broadcast date: the one record of the cluster's undenied candidates its screening days name
    val broadcastTake = c.measured.filter(_ => c.listings.exists(!_.screenings.isEmpty)).flatMap(measured =>
      Broadcast.take(c.listings, measured, eligible.map { case (id, candidates) => id -> candidates.head.film })).map(_.film)
    val posterVote = PosterEvidence.vote(c.posters).map(_._1)
    val agreedImdb = agreed.flatMap(_.crossId("imdb"))
    val agreedTmdb = agreed.flatMap(film => film.crossId("tmdb").flatMap(_.toIntOption).orElse(agreedImdb.flatMap(c.tmdbOf)))
    val venues      = c.listings.map(_.venue).distinct.size
    val severalBill = c.listings.exists(Agreement.billsSeveral)
    val stageWork   = c.listings.exists(Agreement.stagesAWork)
    val traced      = c.decision.trace.nodes.values.toSeq
    built.toSeq.map { contender =>
      val records = contender.records.toSeq
      val own     = contender.tmdb.flatMap(scored.get).getOrElse(Nil)
      val open    = own.filterNot(_.denied)
      val rules   = contender.tmdb.toSeq.flatMap { id =>
        traced.filter(_.candidate.contains(id)).flatMap(_.accepted).flatMap(RuleGroups.get) ++
          Option.when(c.decision.film.contains(id) && !traced.exists(node => node.candidate.contains(id) && node.accepted.isDefined))(
            c.decision.trace.pooled.flatMap(RuleGroups.get).getOrElse("rule.pooled"))
      }
      def took(family: VoterFamily) = c.verdicts.exists(v => v.family == family && v.pick.exists(pick => contender.is(pick.record)))
      def leans(family: VoterFamily) = c.verdicts.exists(v => v.family == family && v.pick.isEmpty && v.leaning.exists(contender.is))
      val turnedDown = c.verdicts.count(Agreement.turnsDown(_, c.listings, records, contender.is))
      val dissent = c.verdicts.count(v => v.pick.exists(pick => !contender.is(pick.record) && c.listings.nonEmpty &&
        c.listings.forall(Agreement.namesIt(_, Seq(pick.record.film)))))
      val votes  = Agreement.listingVotes(asStated, records)
      val namesIt = c.listings.nonEmpty && c.listings.forall(Agreement.namesIt(_, records.map(_.film)))
      val nearest = contender.tmdb.toSeq.flatMap(id => c.posters.flatMap(_.get(id).flatten)).minOption
      val flags: Seq[(String, Boolean)] = Seq(
        // as the stage takes it: by the TMDB id the agreed record links (or IMDb's find of its IMDb id), else as IMDb's
        // film — an agreed record linking neither is taken as nothing (DE "André Rieus Weihnachtskonzert 2026")
        "agreement.quorum"    -> (agreedTmdb.exists(contender.tmdb.contains) || (agreedTmdb.isEmpty && agreedImdb.exists(contender.imdb.contains))),
        "poster.vote"         -> posterVote.exists(contender.tmdb.contains),
        "broadcast.take"      -> broadcastTake.exists(contender.tmdb.contains),
        "listing.catalogue"   -> c.listings.exists(_.catalogueIds.exists(id => c.verdicts.exists(v =>
          v.family.database == id.source && v.pick.exists(pick => pick.id == id.id && contender.is(pick.record))))),
        "model.unscored"      -> open.isEmpty,
        "model.deniedBySome"  -> own.exists(_.denied),
        "model.lean"          -> contender.tmdb.exists(id => c.decision.leaning.exists(_.film == id)),
        "model.best"          -> contender.tmdb.exists(id => c.decision.candidate.exists(_.film == id)),
        "production.season"   -> open.exists(_.seasonProduction),
        "production.house"    -> open.exists(_.houseProduction),
        "listing.facts"       -> votes(Agreement.ListingFacts),
        "listing.runtime"     -> votes(Agreement.ListingRuntime),
        "listing.contradicts" -> Agreement.contradictedByTheListing(c.listings, contender.record),
        "venues.current"      -> (venues >= Agreement.WidelyBilled && records.flatMap(_.film.year).maxOption.exists(_ >= c.thisYear - 1)),
        "title.namesIt"       -> namesIt,
        "title.namesNone"     -> !namesIt,
        // another edition than the title numbers ("League of Legends Worlds 26" against the Worlds25 record)
        "edition.apart"       -> c.listings.exists(listing => PosterEvidence.editionsApart(listing, contender.record.film)),
        "title.anothersOwn"   -> Agreement.anothersOwnTitle(c.listings, records, c.verdicts),
        "bill.several"        -> severalBill,
        "stage.work"          -> stageWork,
        "poster.match"        -> nearest.exists(_ <= PosterEvidence.VoteBits),
        "poster.near"         -> nearest.exists(bits => bits > PosterEvidence.VoteBits && bits <= PosterEvidence.VetoMatchBits),
        "poster.otherMatches" -> (c.posters.nonEmpty && PosterEvidence.veto(contender.tmdb, c.posters).isDefined)) ++
        rules.distinct.map(_ -> true) ++
        VoterFamily.values.toSeq.flatMap(f => Seq(s"family.${f.label}.took" -> took(f), s"family.${f.label}.leans" -> leans(f)))
      val takers  = VoterFamily.values.count(took).toDouble
      val leaning = VoterFamily.values.count(leans).toDouble
      def and(on: Boolean) = if (on) takers else 0.0
      // [[PinnedSignals]]' families.current and family.facts
      val databaseTook    = VoterFamily.values.exists(f => f.namesFilms && took(f))
      val familiesCurrent = takers >= 2 && databaseTook && records.flatMap(_.film.year).maxOption.exists(_ >= c.thisYear - 1)
      val familyFacts     = databaseTook && votes(Agreement.ListingFacts)
      val counts = Seq("count.takers" -> takers, "count.leaning" -> leaning, "count.takersLessDissent" -> math.max(0.0, takers - dissent),
        "count.takers2" -> (if (takers >= 2) 1.0 else 0.0), "count.takers3" -> (if (takers >= 3) 1.0 else 0.0),
        "and.takersNoDissent" -> and(dissent == 0), "and.factsTakers" -> and(votes(Agreement.ListingFacts)),
        "and.posterTakers" -> and(nearest.exists(_ <= PosterEvidence.VoteBits)),
        "and.leanTakers" -> and(contender.tmdb.exists(id => c.decision.leaning.exists(_.film == id))),
        "families.current" -> (if (familiesCurrent) 1.0 else 0.0), "family.facts" -> (if (familyFacts) 1.0 else 0.0)).filter(_._2 != 0.0)
      val signals = counts.toMap ++ flags.collect { case (name, true) => name -> 1.0 }.toMap ++
        open.map(_.probability).maxOption.map(p => "model.logit" -> logit(p)).filter(_._2 != 0.0) ++
        Seq("family.turnedDown" -> turnedDown.toDouble, "family.dissent" -> dissent.toDouble).filter(_._2 != 0.0)
      val film  = contender.record.film
      Contender(contender.film, contender.tmdb, contender.imdb, s"${film.title}${film.year.fold("")(y => s" ($y)")}", contender.ids.toMap, signals)
    }.sortBy(_.film)
  }
}

/**
 * The unified evidence model's weights ([[UnifiedEvidence]]), as DATA: `identity-unified-weights.json`, written by
 * `scripts.IdentityUnifiedFit` from the rows `integration.IdentityUnifiedDataset` emits — a sign-constrained (monotone),
 * L2-regularised logistic regression ([[LogisticFit.fitSigned]]) — and `cut`, the probability a contender is taken at:
 * the lowest above every held-out take a hand label calls wrong. `heldOut` and `ablation` are what the fit measured
 * with whole venues held out.
 */
final case class UnifiedWeights(version: String, signals: Seq[String], weights: Seq[Double], cut: Double, l2: Double, folds: Int,
                                rows: Int, heldOut: Map[String, Double] = Map.empty, ablation: Seq[UnifiedWeights.Ablation] = Nil,
                                provenance: Map[String, String] = Map.empty, guards: Seq[String] = Nil) {
  private lazy val bySignal: Map[String, Double] = signals.zip(weights.drop(1)).toMap
  /** Each signal's contribution to the log-odds, largest first: what a decision lists as its evidence. */
  def contributions(features: Map[String, Double]): Seq[(String, Double)] =
    features.toSeq.flatMap { case (name, x) => bySignal.get(name).map(w => name -> w * x) }.filter(_._2 != 0.0).sortBy(c => (-math.abs(c._2), c._1))
  def logOdds(features: Map[String, Double]): Double = weights.head + contributions(features).map(_._2).sum
  def probability(features: Map[String, Double]): Double = LogisticFit.sigmoid(logOdds(features))
  /** `model.logit=2.25 +3.10 family.imdb.took=1 +1.42 …` */
  def explain(features: Map[String, Double]): String =
    contributions(features).map { case (name, c) => f"$name=${features(name)}%.2f ${if (c >= 0) "+" else ""}$c%.2f" }.mkString(" ")

  /** The guards `features` trips ([[UnifiedEvidence.vetoes]], of this model's `guards`): a contender tripping one is
   *  never taken, whatever it scores. */
  def vetoes(features: Map[String, Double]): Seq[String] =
    UnifiedEvidence.vetoes(name => features.getOrElse(name, 0.0)).filter(guards.contains)

  /** A decision as it explains itself: the guards it passed (or tripped), its probability against the cut, and every
   *  signal's weighted contribution — `taken 97.3% ≥ 95.0%; guards passed: bill.several, …; family.imdb.took=1.00 +2.74 …`. */
  def decision(features: Map[String, Double]): String = {
    val tripped = vetoes(features)
    val p       = probability(features)
    val verdict = if (tripped.nonEmpty) s"vetoed by ${tripped.mkString(", ")}" else if (p >= cut) f"taken ${p * 100}%.1f%% ≥ ${cut * 100}%.1f%%"
                  else f"not taken ${p * 100}%.1f%% < ${cut * 100}%.1f%%"
    s"$verdict; guards passed: ${guards.filterNot(tripped.contains).mkString(", ")}; ${explain(features)}"
  }
}

object UnifiedWeights {
  /** A signal group dropped: what the held-out log-loss and the right and wrong takes came to without it. */
  final case class Ablation(group: String, logLoss: Double, handLogLoss: Double, right: Int, wrong: Int)

  implicit val ablationFormat: OFormat[Ablation]      = Json.format[Ablation]
  implicit val format: OFormat[UnifiedWeights]        = Json.using[Json.WithDefaultValues].format[UnifiedWeights]

  val ResourcePath = "identity-unified-weights.json"
  /** The HYBRID: the guards hard, the fitted score in place of the thresholds among the contenders passing them. */
  val HybridResourcePath = "identity-unified-hybrid-weights.json"

  def fromResource(path: String = ResourcePath): Option[UnifiedWeights] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).map { in =>
      try Json.parse(in).as[UnifiedWeights] finally in.close()
    }
}

/**
 * The identity rules the signal selection CHOSE (`scripts.IdentityUnifiedStrategies`: greedy forward selection over
 * (signal, role) pairs, kept only where every fold with its venues held out selects it too), as DATA:
 * `identity-unified-rules.json`, refit by `scripts/identity-calibrate.sh --unified`. A FILL rule takes, for a cluster the
 * model and the agreement stage took nothing for, the one contender ([[UnifiedEvidence.contenders]]) its signal fires on
 * that trips none of `guards`; a CORRECT rule would act on a film the model took — none is selected today.
 */
final case class UnifiedRules(version: String, guards: Seq[String], fill: Seq[String], correct: Seq[String] = Nil,
                              measured: Map[String, Double] = Map.empty, pinned: Seq[String] = Nil) {
  /** Does the contender trip none of the guards? */
  def passes(contender: UnifiedEvidence.Contender): Boolean =
    UnifiedEvidence.vetoes(name => contender.signals.getOrElse(name, 0.0)).forall(guard => !guards.contains(guard))

  /** The film the first fill rule takes, with the rule's signal: the one guard-passing contender it fires on. */
  def filled(contenders: Seq[UnifiedEvidence.Contender]): Option[(UnifiedEvidence.Contender, String)] =
    (fill ++ pinned).iterator.flatMap { signal =>
      contenders.filter(c => passes(c) && UnifiedRules.fires(signal, name => c.signals.getOrElse(name, 0.0))) match {
        case Seq(one) => Some(one -> signal)
        case _        => None
      }
    }.nextOption()

  /** A fill as its decision explains it: the rule, the guards it passed, and every signal firing on the film. */
  def explain(contender: UnifiedEvidence.Contender, rule: String): String =
    s"filled by $rule: '${contender.title}' ${contender.film} — guards passed: ${guards.mkString(", ")}; signals: " +
      contender.signals.toSeq.sortBy(_._1).map { case (name, x) => if (x == 1.0) name else f"$name=$x%.2f" }.mkString(", ")
}

object UnifiedRules {
  implicit val format: OFormat[UnifiedRules] = Json.using[Json.WithDefaultValues].format[UnifiedRules]
  /** Does the rule `signal` fire on a contender's `features`: every `&`-joined term does — a signal above 0, or, written
   *  `!signal`, one that is 0 ("family.filmweb.took&!model.unscored"). */
  def fires(signal: String, features: String => Double): Boolean =
    signal.split('&').forall(term => if (term.startsWith("!")) features(term.drop(1)) == 0.0 else features(term) > 0.0)
  val ResourcePath = "identity-unified-rules.json"
  def fromResource(path: String = ResourcePath): Option[UnifiedRules] =
    Option(getClass.getClassLoader.getResourceAsStream(path)).map { in =>
      try Json.parse(in).as[UnifiedRules] finally in.close()
    }
  /** The rules on the classpath; none when the artefact is missing (a fill then takes nothing). */
  lazy val resolver: UnifiedRules = fromResource().getOrElse(UnifiedRules("none", UnifiedEvidence.Guards, Nil))
}
