package services.identity

import play.api.libs.json.{Json, OFormat}
import services.identity.agreement.{Agreement, AgreementStage, Broadcast, FamilyPick, FamilyVerdict, SourceRecord, VoterFamily}

/**
 * The UNIFIED evidence model's signals: every signal the resolver's model and the agreement stage after it read about
 * one film a cluster's evidence reaches (a CONTENDER), as one feature vector — the calibrated model's probability and
 * the rule that accepted a film, the TMDB lean and best candidate, each film database family's take and lean, the
 * venue's own year, director and running time, the venues billing it, the title's guards, and the venue posters
 * (docs/design/identity-resolver.md §20, the inventory). Weighed by `UnifiedWeights` (worker tests), FITTED (`scripts.IdentityUnifiedFit`
 * over `integration.IdentityUnifiedDataset`'s rows), never hand-set; each signal's DIRECTION is known (a family's take
 * only ever speaks for a film), and the fit holds every weight to it.
 *
 * Its FILL rules ([[UnifiedRules]]) decide: the agreement stage takes the one contender a rule fires on for a cluster
 * nothing else took (`AgreementStage`'s fill).
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
   *  spojrzenie": Wikidata takes Konovalov's 2026 film, the festival dates and credits it; docs/design/identity-resolver.md §20.12).
   *  `cast.venue`: of namesakes, the venue's own synopsis and cast field name two whole names of the film's top-billed
   *  TMDB cast, and of no other candidate's, where no listing bills a director nor several works ([[CastEvidence.tieBreak]];
   *  its value, how many) — PL Kino CK Lublin's "Lalka" names Dorociński, Urzędowska and Kondrat of Kawalski's 2026 film.
   *  `film.current`: the film released this year or last — what a fill on weak signals asks besides, as `venues.current`
   *  and `families.current` do: an older namesake fits a one-off event as well as a title (PL Kino Kosmos "Człowiek
   *  znikąd | uroczysty pokaz odrestaurowanego filmu", Kłyk's restored western: the model's and Filmweb's lean, and
   *  IMDb's take, are the 2010 Korean film).
   *  `family.feedLink`: a film database's family takes the film AND the listings' TMDB-keyed feed links it
   *  ([[FeedLink.linked]]) — the feed's link a hint no rule takes alone ([[FeedLink]]), a family's take of a
   *  bare title a namesake as often as the film (DE "Newborn", "Like Father, Like Son": Filmweb's namesakes, Cineamo
   *  linking other films); the two naming one film are two independent sources (DE 3001 Kino's "Amadeo", Cineamo's
   *  648574m and Filmweb's take, the 2023 film) — unless the year the feed copied denied a current film TMDB ranked first
   *  for the title, for an older one:
   *  the family's take then agrees with the feed's copy, no second source (DE Astor's "ATMOS: Michael").
   *  `family.director`: a film database's family takes the film AND every listing whose venue credits a director credits
   *  one of the film's — `family.facts` for a venue stating no year (UK Picture House Uckfield's "The Misanthrope (NT
   *  Live)", crediting Indhu Rubasingham: the National Theatre's 2026 production IMDb takes).
   *  `family.releaseKept`: a film database's family takes the film AND the release veto took out a namesake beside it
   *  for having no release in the venue's country, while TMDB dates one of this film there ([[ReleaseVeto]]) — two
   *  sources, a family's pick and a distributor's release, where a bare title's take alone is a namesake as often as the
   *  film (PL Kino Kultura Gołdap's "NADZIEJA": Filmweb takes Mucha's 2007 documentary, released in Poland, the 2019
   *  namesake not; Kino za Rogiem Cafe's "Wymarzony": Herry's 2018 film, not Coiro's 2013 one).
   *  `model.vetoedAbove`: TMDB's search ranked first a namesake the release veto took out — the film stands only for the
   *  veto, and a family's take of the bare title names the namesake its country knows just as the veto does: one signal,
   *  no corroboration (PL Kino za Rogiem Cafe's "Stan wyjątkowy": TMDB's first hit, Hřebejk's 2024 film, vetoed; Filmweb
   *  takes Brešan's 2018 one under it). */
  val PinnedSignals: Seq[String] = Seq("families.current", "family.facts", "cast.venue", "film.current", "family.feedLink", "family.director",
    "family.releaseKept", "model.vetoedAbove")

  /** The NON-COMPENSATORY guards: a contender one of these fires on is no film to take, whatever else speaks for it — the
   *  agreement stage's vetoes (several works billed, a stage work, another film's own title, the
   *  venue's year or director against it, its running time short of the film ([[Agreement.runsShort]]), a venue poster
   *  naming another candidate, a family turning it down; and, as the agreement requires of what it takes, a title naming
   *  the film, and no other edition than the title numbers). A film the model's own rules accepted ([[ModelRules]]) passed
   *  the model's guards instead ([[Acceptance]]), as today. `listing.runsShort`, `bill.piece` (the billing naming the film
   *  as one piece beside another thing, [[BilledPiece.ofFamilies]]) and `listing.certifiedBelow` (the venue's certificate
   *  well below TMDB's for the film in its country, [[Certificates.below]]) are guards only, no fitted signals of [[Signals]]: the
   *  training rows predate them. */
  val Guards: Seq[String] = Seq("bill.several", "stage.work", "title.anothersOwn", "listing.contradicts", "poster.otherMatches",
    "family.turnedDown", "title.namesNone", "edition.apart", "listing.runsShort", "bill.piece", "listing.certifiedBelow")
  /** The guards the fit's training rows carry, each a signal of [[Signals]]: what the offline fit and strategies read. */
  val FittedGuards: Seq[String] = Guards.filter(Names.contains)
  val ModelRules: Seq[String] = Names.filter(_.startsWith("rule."))

  /** The guards `features` trips: none for a film the model's rules took. */
  def vetoes(features: String => Double): Seq[String] =
    if (ModelRules.exists(features(_) > 0)) Nil else Guards.filter(features(_) != 0)

  /** The rule groups an acceptance rule's name falls in ([[Acceptance]]'s rules, by the name a trace gives them). */
  val RuleGroups: Map[String, String] = Map(
    "exact-top-hit" -> "rule.title", "segment-top-hit" -> "rule.title", "sole-result" -> "rule.title", "sole-work" -> "rule.title",
    "dated-title" -> "rule.title", "directors-work" -> "rule.director", "directors-title" -> "rule.director",
    "favoured-calibrated" -> "rule.calibrated", "unrivalled-calibrated" -> "rule.calibrated", "imdb-suggested" -> "rule.imdb",
    "house-production" -> "rule.stage", "season-record" -> "rule.stage", "season-production" -> "rule.stage",
    "relay-record" -> "rule.stage")

  /** The calibrated probability as a feature: its log-odds, clamped at ±9.2 (1e-4) and rounded to a quarter, so rows the
   *  fit reads fold into few distinct ones. */
  def logit(probability: Double): Double = {
    val p = math.min(1 - 1e-4, math.max(1e-4, probability))
    math.rint(math.log(p / (1 - p)) * 4) / 4
  }

  /** What a cluster's evidence holds: its listings, the model's decision on it, its nodes' scored candidates, each
   *  family's verdict (the families that answered), each venue poster's nearest distance to each TMDB film hashed
   *  (empty where none), the TMDB film an IMDb id finds, and the year it is now — and, for the broadcast join
   *  ([[Broadcast.take]]), the film databases' house productions and which records still wait on their day. */
  final case class ClusterEvidence(listings: Seq[Listing], decision: ResolverDecision, nodes: Seq[IdentityResolver.NodeEvidence],
                                   verdicts: Seq[FamilyVerdict], posters: Seq[Map[Int, Option[Int]]], tmdbOf: String => Option[Int],
                                   thisYear: Int, stated: Seq[Listing] = Nil, measured: Option[Listing => IdentityMeasures.Listing] = None,
                                   venueNames: VenueNames = VenueNames.None, cast: Int => Option[Seq[String]] = _ => None,
                                   productions: () => Answer[Seq[IdentityMeasures.Film]] = () => Answer.Known(Nil),
                                   undated: Int => Boolean = _ => false)

  /** One film the evidence reaches: `tmdb:<id>` or, a film only other databases hold, `imdb:<tt>`; its title, each family
   *  that took it with its own id of it (`"rt" -> "dune_2021"`, an id only, never read as evidence), every signal that
   *  is not 0, and the names of its cast the venue's own text names (`cast.venue`'s). */
  final case class Contender(film: String, tmdb: Option[Int], imdb: Option[String], title: String, familyIds: Map[String, String],
                             signals: Map[String, Double], castNamed: Seq[String] = Nil)

  private final class Building(val tmdb: Option[Int], val imdb: Option[String], val record: SourceRecord) {
    val records = scala.collection.mutable.ArrayBuffer(record)
    val ids     = scala.collection.mutable.Map.empty[String, String]
    def film: String = tmdb.fold(imdb.fold("")(id => s"imdb:$id"))(id => s"tmdb:$id")
    def is(other: SourceRecord): Boolean = Agreement.joins(records.toSeq, other)
  }

  /** Every film the cluster's evidence reaches — the TMDB candidates some node scored and did not deny, and each film a
   *  family took or leans to (joined to a TMDB candidate by a shared id, else by its facts, held apart from none of its
   *  records, as the agreement joins two families' picks: [[Agreement.joins]]) — with its signals. A family's film no id names is none: no film can be shown for it. */
  def contenders(c: ClusterEvidence): Seq[Contender] = new Contending(c).contenders

  /** One run of [[contenders]] over cluster `c`: the films its evidence reaches, then each one's signals. */
  private final class Contending(c: ClusterEvidence) {
    private val scored   = c.nodes.flatMap(_.candidates).filterNot(candidate => FallbackIds.isFallback(candidate.tmdbId)).groupBy(_.tmdbId)
    private val eligible = scored.filter(_._2.exists(!_.denied)).toSeq.sortBy(_._1)
    private val built = scala.collection.mutable.ArrayBuffer.from(eligible.map { case (id, candidates) =>
      val film = candidates.head.film
      new Building(Some(id), IdentityMeasures.imdbId(film.imdbNumber), SourceRecord.ofTmdb(id, film))
    })
    familyFilms.foreach(joinFamilyFilm)

    /** The families' films, each family's record joined to every other naming the same film (as the agreement joins
     *  picks): a film IMDb names by its id alone and Wikidata by its TMDB id is one film, the TMDB candidate both name. */
    private def familyFilms: Vector[Vector[(SourceRecord, Option[FamilyPick])]] = {
      val familyRecords = c.verdicts.sortBy(_.family.ordinal).flatMap(v => v.pick.map(pick => pick.record -> Option(pick)) ++ v.leaning.map(_ -> None))
      familyRecords.foldLeft(Vector.empty[Vector[(SourceRecord, Option[FamilyPick])]]) { (films, entry) =>
        val (joined, apart) = films.partition(film => Agreement.joins(film.map(_._1), entry._1))
        apart :+ (joined.flatten :+ entry)
      }
    }

    /** A family's film onto the contender it joins (a candidate its records join, else the TMDB film its ids name), or a
     *  contender of its own where an id names it. */
    private def joinFamilyFilm(members: Vector[(SourceRecord, Option[FamilyPick])]): Unit = {
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
    private val modelVote = c.decision.leaning.orElse(c.decision.candidate).map(lean =>
      scored.get(lean.film).map(cs => SourceRecord.ofTmdb(lean.film, cs.head.film)).getOrElse(AgreementStage.leanRecord(lean)))
    // as the stage reads it: the listings as their venues' own pages state them, and a film only review sites take is none
    private val agreed    = (if (c.decision.film.isDefined) None else Agreement.agreed(c.listings, c.verdicts, modelVote, Some(c.thisYear), c.stated))
      .filter(_.families.exists(_.namesFilms))
    private val asStated  = if (c.stated.nonEmpty) c.stated else c.listings
    // the stage relay's broadcast date: the one record of the cluster's undenied candidates its screening days name, as
    // the stage's take reads it (the productions credited, none while it waits on a record's day)
    private val broadcastTake = c.measured.filter(_ => c.listings.exists(!_.screenings.isEmpty)).flatMap(measured =>
      Broadcast.take(c.listings, measured, eligible.map { case (id, candidates) => id -> candidates.head.film }, c.productions)(c.undated)
        .toOption.flatten).map(_.film)
    private val posterVote = PosterEvidence.vote(c.posters).map(_._1)
    private val agreedImdb = agreed.flatMap(_.crossId("imdb"))
    private val agreedTmdb = agreed.flatMap(film => film.crossId("tmdb").flatMap(_.toIntOption).orElse(agreedImdb.flatMap(c.tmdbOf)))
    private val venues      = c.listings.map(_.venue).distinct.size
    private val stageWork   = c.listings.exists(ListingShape.stagesAWork)
    // the rules the model's nodes accepted a film by alone — none where the model decided its members apart as none of
    // that film: its own no-film, which no rule it overruled lifts a guard against (UK "CBeebies Panto: Treasure Island",
    // a pantomime's own TMDB record, which a title rule took and [[BilledPiece]] refused)
    private val traced      = if (c.decision.trace.decidedApart) Nil else c.decision.trace.nodes.values.toSeq
    // the one TMDB film the listings' TMDB-keyed feeds link, where they link one ([[PinnedSignals]]' family.feedLink)
    private val feedLinked = FeedLink.linked(c.listings).map(_._2)
    // the CURRENT film (released this year or last) TMDB's search ranked first for a title naming it, which a node denied
    // on a year no venue states itself — the feed's copy, its entry's claim — with its year: where the feed links an older
    // film, a family whose take agrees with the link agrees with the copied year, no second source ([[PinnedSignals]]'
    // family.feedLink). DE Astor's "ATMOS: Michael": Cineamo links Dreyer's 1924 film and copies its year, which denied the
    // 2026 biopic sixteen venues bill; Filmweb's family took the 1924 film by that year. Not a link to the current film
    // over an older namesake (DE Gröben-Lichtspiele's "Camouflage OmU": Cineamo and Filmweb on the 2026 Ukrainian film).
    private val copiedYearDenied = if (c.listings.isEmpty || c.listings.exists(SourceReliability.decides(_, SourceReliability.Fact.Year))) Map.empty[Int, Int]
      else c.nodes.flatMap(_.candidates).filter(first => first.rank.contains(1) && first.denied && first.titleNamesIt &&
        first.film.year.exists(_ >= c.thisYear - 1) && first.measures.get("year.delta").exists(!_.isInstanceOf[IdentityMeasures.Missing]))
        .map(first => first.tmdbId -> first.film.year.get).toMap
    // the listings whose venue credits a director ([[PinnedSignals]]' family.director): a feed's credit is its entry's
    private val credited   = asStated.filter(listing => SourceReliability.decides(listing, SourceReliability.Fact.Directors) && listing.directors.nonEmpty)
    // the namesakes the release veto kept beside one it took out ([[PinnedSignals]]' family.releaseKept)
    private val releaseKept = c.nodes.flatMap(_.candidates).flatMap(_.releasedBeside).toSet
    // [[PinnedSignals]]' cast.venue: a TIE-BREAK among the contenders and denied candidates the billing names alike — the
    // one whose top-billed cast the venue's own text names two whole names of, no other's twice ([[CastEvidence.tieBreak]];
    // a contender no TMDB record holds has no cast known, and leaves it unread) — and only where no listing bills a
    // director (where one does, the director decides: cast repeats it for 1,053 of 1,147 listings) nor several works (a
    // double bill names both films' casts)
    private val castTake =
      if (c.venueNames.isEmpty || c.listings.exists(_.directors.nonEmpty) || c.listings.exists(ListingShape.billsSeveral)) None
      else {
        def billed(films: Seq[IdentityMeasures.Film]) = c.listings.forall(Agreement.namesIt(_, films))
        val denied = scored.toSeq.filterNot { case (id, _) => eligible.exists(_._1 == id) }.sortBy(_._1)
          .collect { case (id, candidates) if billed(Seq(candidates.head.film)) => Option.empty[Building] -> c.cast(id) }
        val sameTitled = built.toSeq.filter(b => billed(b.records.toSeq.map(_.film))).map(b => Option(b) -> b.tmdb.flatMap(c.cast))
        CastEvidence.tieBreak(c.venueNames, sameTitled ++ denied).collect { case (Some(b), named) => b -> named }
      }
    // each candidate's titles as runs of the billing, found once for every contender's `title.anothersOwn` — a denied
    // candidate's too: the billing naming it more fully names another film than one it names only in part, whatever
    // ruled that one out (DE CineStar's "Herr der Ringe - Rückkehr des Königs (ext.)")
    private lazy val candidateFilms = built.toSeq.map(_.records.toSeq.map(_.film)) ++
      scored.collect { case (id, candidates) if !eligible.exists(_._1 == id) => Seq(candidates.head.film) }
    private lazy val candidateSpans = candidateFilms.map(Agreement.billedSpans(c.listings, _))
    // and as each listing bills it alone: a cluster can join a billing under two names to its neighbours billing one
    private lazy val listingSpans = c.listings.distinctBy(_.rawTitle).map(listing => listing -> candidateFilms.map(Agreement.billedSpans(Seq(listing), _)))

    def contenders: Seq[Contender] = built.toSeq.map(contender => new Scoring(contender).signalled).sortBy(_.film)

    /** One contender's signals. */
    private final class Scoring(contender: Building) {
      private val records = contender.records.toSeq
      private val own     = contender.tmdb.flatMap(scored.get).getOrElse(Nil)
      private val open    = own.filterNot(_.denied)
      private val rules   = contender.tmdb.toSeq.flatMap { id =>
        traced.filter(_.candidate.contains(id)).flatMap(_.accepted).flatMap(RuleGroups.get) ++
          Option.when(c.decision.film.contains(id) && !traced.exists(node => node.candidate.contains(id) && node.accepted.isDefined))(
            c.decision.trace.pooled.flatMap(RuleGroups.get).getOrElse("rule.pooled"))
      }
      private def took(family: VoterFamily) = c.verdicts.exists(v => v.family == family && v.pick.exists(pick => contender.is(pick.record)))
      private def leans(family: VoterFamily) = c.verdicts.exists(v => v.family == family && v.pick.isEmpty && v.leaning.exists(contender.is))
      private val turnedDown = c.verdicts.count(Agreement.turnsDown(_, c.listings, records, contender.is))
      private val dissent = c.verdicts.count(v => v.pick.exists(pick => !contender.is(pick.record) && c.listings.nonEmpty &&
        c.listings.forall(Agreement.namesIt(_, Seq(pick.record.film)))))
      // the venues' own facts: a feed's copied from its catalogue entry are one vote of the agreement's, no listing's ([[Agreement.factVotes]])
      private val votes  = Agreement.factVotes(asStated, records)
      private val namesIt = c.listings.nonEmpty && c.listings.forall(Agreement.namesIt(_, records.map(_.film)))
      private val nearest = contender.tmdb.toSeq.flatMap(id => c.posters.flatMap(_.get(id).flatten)).minOption
      // released this year or last: a current release, not a one-off event an older namesake fits
      private val current = records.flatMap(_.film.year).maxOption.exists(_ >= c.thisYear - 1)

      private def flags: Seq[(String, Boolean)] = evidenceFlags ++ titleFlags ++
        rules.distinct.map(_ -> true) ++
        VoterFamily.values.toSeq.flatMap(f => Seq(s"family.${f.label}.took" -> took(f), s"family.${f.label}.leans" -> leans(f)))

      /** The flags of the agreement, the posters, the broadcast, the model and the listings' facts. */
      private def evidenceFlags: Seq[(String, Boolean)] = Seq(
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
        // its year or director, whoever states them
        "listing.contradicts" -> Agreement.contradictedByAnyListing(c.listings, contender.record),
        // the venue's own running time well short of an older film's whole one
        "listing.runsShort"   -> Agreement.runsShort(c.listings, contender.record, Some(c.thisYear)).isDefined,
        // the venue's certificate well below the one TMDB records for the film in its country: another cut, another film
        "listing.certifiedBelow" -> own.headOption.exists(tmdbFilm => c.listings.exists(Certificates.below(_, tmdbFilm.film).isDefined)),
        "venues.current"      -> (venues >= Agreement.WidelyBilled && current))

      /** The flags of how the billing's title names it, and of its posters. */
      private def titleFlags: Seq[(String, Boolean)] = Seq(
        "title.namesIt"       -> namesIt,
        "title.namesNone"     -> !namesIt,
        // another edition than the title numbers ("League of Legends Worlds 26" against the Worlds25 record)
        "edition.apart"       -> c.listings.exists(listing => PosterEvidence.editionsApart(listing, contender.record.film)),
        // another film's own title billed, or the film named only within another candidate's title billed whole
        // or the billing naming a namesake's director, not the film's, or another candidate's titles naming more of it
        "title.anothersOwn"   -> (Agreement.anothersOwnTitle(c.listings, records, c.verdicts) || Agreement.billsAnothersDirector(c.listings, records, c.verdicts) ||
          Agreement.namedWithinAnother(Agreement.billedSpans(c.listings, records.map(_.film)), candidateSpans) ||
          listingSpans.exists { case (listing, spans) => Agreement.namesMoreThanIt(Agreement.billedSpans(Seq(listing), records.map(_.film)), spans) }),
        "bill.several"        -> c.listings.exists(ListingShape.billsSeveralBeside(_, contender.record.film)),
        // the billing names it as one piece beside another thing, as the agreement refuses its own film ([[BilledPiece.ofFamilies]])
        "bill.piece"          -> BilledPiece.ofFamilies(c.listings, contender.record.film, contender.tmdb, BilledPiece.Keeps.FactsOrCurrent).isDefined,
        "stage.work"          -> stageWork,
        "poster.match"        -> nearest.exists(_ <= PosterEvidence.VoteBits),
        "poster.near"         -> nearest.exists(bits => bits > PosterEvidence.VoteBits && bits <= PosterEvidence.VetoMatchBits),
        "poster.otherMatches" -> (c.posters.nonEmpty && PosterEvidence.veto(contender.tmdb, c.posters).isDefined))

      private val takers  = VoterFamily.values.count(took).toDouble
      private val leaning = VoterFamily.values.count(leans).toDouble
      private def and(on: Boolean) = if (on) takers else 0.0
      private val castNamed = castTake.collect { case (b, named) if b eq contender => named }.getOrElse(Nil)

      private def counts: Seq[(String, Double)] = {
        // [[PinnedSignals]]' families.current and family.facts
        val databaseTook    = VoterFamily.values.exists(f => f.namesFilms && took(f))
        val familiesCurrent = takers >= 2 && databaseTook && current
        val familyFacts     = databaseTook && votes(Agreement.ListingFacts)
        val familyFeedLink  = databaseTook && contender.tmdb.exists(feedLinked.contains) && !copiedYearDenied.exists { case (id, year) =>
          !contender.tmdb.contains(id) && records.flatMap(_.film.year).forall(_ < year) }
        val familyDirector  = databaseTook && credited.nonEmpty &&
          credited.forall(listing => records.exists(record => FactRelations.samePerson(listing.directors, record.film.directors.getOrElse(Nil))))
        val familyReleased  = databaseTook && contender.tmdb.exists(releaseKept)
        // [[PinnedSignals]]' model.vetoedAbove: a node's search for it ranked first a namesake the release veto took out
        val vetoedAbove = contender.tmdb.exists(id => c.nodes.exists(node => node.candidates.exists(_.tmdbId == id) &&
          node.candidates.exists(first => first.tmdbId != id && first.releasedBeside.isDefined && first.rank.contains(1))))
        Seq("count.takers" -> takers, "count.leaning" -> leaning, "count.takersLessDissent" -> math.max(0.0, takers - dissent),
          "count.takers2" -> (if (takers >= 2) 1.0 else 0.0), "count.takers3" -> (if (takers >= 3) 1.0 else 0.0),
          "and.takersNoDissent" -> and(dissent == 0), "and.factsTakers" -> and(votes(Agreement.ListingFacts)),
          "and.posterTakers" -> and(nearest.exists(_ <= PosterEvidence.VoteBits)),
          "and.leanTakers" -> and(contender.tmdb.exists(id => c.decision.leaning.exists(_.film == id))),
          "families.current" -> (if (familiesCurrent) 1.0 else 0.0), "family.facts" -> (if (familyFacts) 1.0 else 0.0),
          "family.feedLink" -> (if (familyFeedLink) 1.0 else 0.0), "family.director" -> (if (familyDirector) 1.0 else 0.0),
          "family.releaseKept" -> (if (familyReleased) 1.0 else 0.0), "model.vetoedAbove" -> (if (vetoedAbove) 1.0 else 0.0),
          "film.current" -> (if (current) 1.0 else 0.0),
          "cast.venue" -> castNamed.size.toDouble).filter(_._2 != 0.0)
      }

      def signalled: Contender = {
        val signals = counts.toMap ++ flags.collect { case (name, true) => name -> 1.0 }.toMap ++
          open.map(_.probability).maxOption.map(p => "model.logit" -> logit(p)).filter(_._2 != 0.0) ++
          Seq("family.turnedDown" -> turnedDown.toDouble, "family.dissent" -> dissent.toDouble).filter(_._2 != 0.0)
        val film  = contender.record.film
        Contender(contender.film, contender.tmdb, contender.imdb, s"${film.title}${film.year.fold("")(y => s" ($y)")}", contender.ids.toMap, signals,
          castNamed)
      }
    }
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
      contender.signals.toSeq.sortBy(_._1).map { case (name, x) => if (x == 1.0) name else f"$name=$x%.2f" }.mkString(", ") +
      (if (contender.castNamed.isEmpty) "" else s"; the venue's own text names its cast ${contender.castNamed.mkString(", ")}")
}

object UnifiedRules {
  implicit val format: OFormat[UnifiedRules] = Json.using[Json.WithDefaultValues].format[UnifiedRules]
  /** Does the rule `signal` fire on a contender's `features`: every `&`-joined term does — a signal above 0, or, written
   *  `!signal`, one that is 0 ("family.feedLink&!model.unscored"). */
  def fires(signal: String, features: String => Double): Boolean =
    signal.split('&').forall(term => if (term.startsWith("!")) features(term.drop(1)) == 0.0 else features(term) > 0.0)
  val ResourcePath = "identity-unified-rules.json"
  def fromResource(path: String = ResourcePath): Option[UnifiedRules] =
    ClasspathResource.json[UnifiedRules](path)
  /** The rules on the classpath; none when the artefact is missing (a fill then takes nothing). */
  lazy val resolver: UnifiedRules = fromResource().getOrElse(UnifiedRules("none", UnifiedEvidence.Guards, Nil))
}
