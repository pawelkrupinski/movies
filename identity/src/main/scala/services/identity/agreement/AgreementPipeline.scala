package services.identity.agreement

import services.identity.{Answer, CatalogueAnswers, IdentityCalibration, IdentityLookups, Listing, PosterAnswers, PosterEvidence,
  Question, Resolution, ResolverDecision}
import models.ListingKey
import services.titles.TitleNormalizer

import scala.collection.mutable

/**
 * What each model decision is TAKEN AS ([[decide]]): a model take read by its CORRECTION ([[AgreementCorrections]]), a
 * no-match by its families' VERDICT ([[ClusterVerdicts]]) and that verdict by its TAKE ([[AgreementTakes]]) — an event no
 * film database holds ([[NonFilmEvents]]) by none of them. Each stage is handed the evidence it reads
 * ([[AgreementEvidence]]) and what else it needs, never the pipeline; [[AgreementStage]] runs the pipeline over each
 * resolution, an apply at a time. Each verdict is decided under `decidedUnder` ([[AgreementStage.decidedUnder]]).
 */
private[agreement] final class AgreementPipeline(families: Map[VoterFamily, FamilyAnswers], venuePages: IdentityLookups,
                                                 normalizer: TitleNormalizer, calibration: IdentityCalibration,
                                                 tmdbOf: String => Answer[Option[Int]], stored: AgreementVerdicts,
                                                 clock: java.time.Clock, changes: AnswerChanges, tmdb: Option[IdentityLookups],
                                                 identities: Seq[VoterFamily], rules: services.identity.UnifiedRules,
                                                 catalogue: CatalogueAnswers, listedOn: Option[VoterFamily],
                                                 askNow: AgreementStage.AskNow, attempted: Question => Boolean,
                                                 tmdbReads: TmdbReadTracking, decidedUnder: String) {

  /** What every stage of the pipeline reads, through the receipt being made ([[AgreementEvidence]]). */
  private[agreement] val evidence = new AgreementEvidence(families, venuePages, tmdb, normalizer, calibration, tmdbOf, attempted, tmdbReads)
  import evidence.{looked, venues, verdictsOf}

  /** The verdicts kept ([[VerdictIndex]]), by their cluster's key and by their listings' digest. */
  private[agreement] val verdicts = new VerdictIndex(stored, changes)

  /** Each model decision the pipeline took, with what it took it as: handed back as that same object while neither moved,
   *  so the projection, which diffs decisions by identity, redrafts a cluster only when its take moved ([[reused]]). */
  private val takenAs = new java.util.IdentityHashMap[ResolverDecision, ResolverDecision]()
  /** Each model decision's cluster id, listings and their digest, kept while the decision is the same object: the model
   *  hands over a new decision when one of its listings or a fact it reads moves, so a quiet tick reads no listing again
   *  — and one re-published with a field only the stage reads is forgotten ([[apply]]'s `republished`). */
  private[agreement] val digested = new java.util.IdentityHashMap[ResolverDecision, AgreementEvidence.Cluster]()
  /** Each model decision's verdict as an event no film database holds ([[NonFilmEvents]]), kept while the decision is
   *  the same object: such a cluster is asked nothing — neither the agreement's questions nor a model take's correction. */
  private[agreement] val events = new java.util.IdentityHashMap[ResolverDecision, Option[String]]()

  // THE PIPELINE ([[decide]]): a model take is read by its CORRECTION, a no-match by its families' VERDICT and that
  // verdict by its TAKE -- each stage handed the evidence it reads and the rest it needs, never the stage itself.
  /** The TMDB films a cluster's own evidence reaches, its posters' distances to them: what the takes and corrections read. */
  private[agreement] val candidates      = new AgreementCandidates(evidence, changes)
  /** A cluster met in a new shape has its families asked inline ([[askNow]]). */
  private[agreement] val inline          = new InlineAsker(askNow, clock)
  /** A model take read against the evidence that can contradict it. */
  private[agreement] val corrections: ModelTakeCorrection = new AgreementCorrections(evidence, candidates, verdicts, inline, decidedUnder, listedOn)
  /** A no-match's take of its families' verdict: the agreed film, else the posters', the broadcast's, a fill's or the catalogue's. */
  private[agreement] val takes           = new AgreementTakes(evidence, candidates, rules, identities, catalogue, tmdbOf)
  /** A no-match's families' verdict, held while what it read stands. */
  private[agreement] val clusterVerdicts = new ClusterVerdicts(evidence, verdicts, takes, inline, decidedUnder, clock)
  import candidates.{posterDistances, searchedFilms}
  import corrections.{correctable, corrected}
  import AgreementTakes.taking

  /** What the model's `decision` is taken as: a model take read against the evidence that may correct it, a no-match
   *  the families, the posters, the screening days, a fill or the catalogue ids take a film for — else the decision as
   *  the model left it. What it reads and waits on is noted in `pass`. */
  def decide(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], pass: AgreementPass): ResolverDecision =
    if (correctable(decision) && eventOf(decision, listingOf).isEmpty) {
      val key = AgreementStage.correctionId(pass.keys.get(decision))
      pass.correcting += key
      reused(decision, corrected(decision, listingOf, key, pass))
    }
    else if (decision.film.isDefined || decision.unanswered > 0 || decision.fallback.isDefined || decision.members.isEmpty) decision
    else {
      val now = eventOf(decision, listingOf) match {
        // an event no film database holds: no family is asked about it, no poster hashed, no verdict kept
        case Some(why) => taking(decision)(ResolverDecision.Basis.Event, s"event, not a film: $why")
        case None      => unmatched(decision, listingOf, pass)
      }
      reused(decision, now)
    }

  /** `now`, what the model's `decision` is taken as — `decision` itself while it is taken as it stands, else the object
   *  it was last taken as while the two are equal ([[takenAs]]). */
  private def reused(decision: ResolverDecision, now: ResolverDecision): ResolverDecision =
    if (now eq decision) decision
    else Option(takenAs.get(decision)).filter(_ == now).getOrElse { takenAs.put(decision, now); now }

  /** What a model no-match that bills a film is taken as: its cluster read, its families' verdict found or decided
   *  ([[ClusterVerdicts.verdictOf]]), and that verdict's take ([[AgreementTakes.verdictTake]]) — the decision as the model
   *  left it while there is none. What its take asks is noted in `pass`. */
  private def unmatched(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], pass: AgreementPass): ResolverDecision = {
    val AgreementEvidence.Cluster(id, listings, digest) = digestedOf(decision, pass.keys.get(decision), listingOf)
    pass.seen += id
    pass.shapes += digest
    takes.verdictTake(decision, id, digest, listings, clusterVerdicts.verdictOf(decision, id, listings, digest, pass), pass)
  }

  /** Why the decision's cluster is an event no film database holds, or `None` — read once per decision object. */
  private def eventOf(decision: ResolverDecision, listingOf: ListingKey => Option[Listing]): Option[String] =
    Option(events.get(decision)).getOrElse {
      // a title shaped as a franchise's marathon is a film's own as the MODEL's searches found ([[ResolverDecision.filmTitled]]):
      // its answers, in its decision -- one moved hands the stage a new decision, read again -- never a search of the stage's
      val event = NonFilmEvents.of(decision.members.flatMap(listingOf), listing => decision.filmTitled(listing.key))
      events.put(decision, event); event
    }

  /** The decision's cluster key (`key`), its listings sorted and their digest, kept while the model hands the same
   *  decision over — which keeps its key as long. */
  private def digestedOf(decision: ResolverDecision, key: String, listingOf: ListingKey => Option[Listing]): AgreementEvidence.Cluster =
    Option(digested.get(decision)).getOrElse {
      val listings = AgreementEvidence.sortedListings(decision, listingOf)
      val fresh    = AgreementEvidence.Cluster(key, listings,
        AgreementStage.digest(Seq(digestOf(listings).toString, decision.leaning.toString, decision.candidate.toString,
          PosterEvidence.urls(listings).mkString("\u0001"))))
      digested.put(decision, fresh); fresh
    }

  /** The listings as published, the people their own text names, and the venue detail page the picks read of each. */
  private def digestOf(listings: Seq[Listing]): Long =
    AgreementStage.digest(listings.map(l => l.sortKey + l.screenedSeason.fold("")(season => s"\u0002$season") +
      (if (l.names.isEmpty) "" else s"\u0003${l.names.##}") +
      (if (venues.hasDetail(l)) "\u0001" + venues.detail(l) else "")))

  /** The questions `listing`'s OWN facts lead the stage to ask -- its titles' and directors' searches and the records
   *  their hits name, as the stage asks them of any cluster holding it (the family verdicts over the listing alone, every
   *  gap they meet), and the posters the poster vote and veto read of it while they are not hashed: its venue poster
   *  ([[PosterEvidence.urls]]), then those of the films its own searches find ([[posterDistances]] over the listing
   *  alone). None for an event no film database holds ([[NonFilmEvents]]), whose families are never asked nor its poster
   *  hashed -- a title shaped as a franchise's marathon reads as one here, the model's searches that may make it a film's
   *  ([[ResolverDecision.filmTitled]]) not read yet: its families are asked once its decision says so. What the model's
   *  input gate waits on before a listing's version is taken in (docs/design/identity-structural-fixes.md, fix 1: asking
   *  apart from deciding): a population-independent set, a
   *  function of the listing and the answers filed. Without the posters, a listing joined a cluster with one unhashed, the
   *  take waited on it, and the model's "no film" stood until the poster was filed seconds later (prod 11-07..11-10: UK
   *  "Hadoota" x14 and ES "DUNE" x6 on their own venue posters; US "Parkway Drive: Home" x83 and UK "Touché" x5 on a
   *  candidate film's). */
  def questionsOf(listing: Listing, posters: PosterAnswers): Set[Question] =
    if (NonFilmEvents.of(listing).isDefined) Set.empty
    else {
      val gaps = mutable.Set.empty[Question]
      verdictsOf(Seq(listing), gaps, mutable.Map.empty)
      // none of the films while a search is unanswered: the search is a question already, and the posters its answer
      // leads to are asked once it is (ModelInputGate's askLedTo); the id and digest key only the cluster's cache, unread
      posterDistances("", 0L, Seq(listing), also = None, gaps,
        candidates = (_, _, alone) => looked.flatMap(searchedFilms(alone, _)).getOrElse(Nil), hashes = posters)
      gaps.toSet
    }

  /** Forget the listings read for each decision holding one of `keys`; whether one the pipeline reads — a cluster nothing
   *  took, a model take it corrects — holds any. */
  def forgetRead(resolution: Resolution, keys: Iterable[ListingKey]): Boolean = keys.nonEmpty && {
    val moved   = keys.toSet
    val touched = resolution.decisions.filter(_.members.exists(moved))
    touched.foreach(digested.remove)
    touched.exists(decision => decision.film.isEmpty || correctable(decision))
  }

  /** Forget what the clusters `resolution` no longer holds were kept with — their verdicts, waits, candidates,
   *  corrections and the model's decisions — and store the verdicts `pass` moved. */
  def forgetGone(resolution: Resolution, pass: AgreementPass): Unit = {
    // a verdict under other rules decided again to a wait, none under these rules in its place: a cache of nothing now
    val renewed    = pass.moved.iterator.filter(_.rules == decidedUnder).map(_.id).toSet
    val superseded = pass.backfilled.toSet -- renewed
    val removed    = verdicts.memo.keys.toSet -- pass.seen -- pass.correcting ++ superseded
    val moved      = pass.moved.filterNot(verdict => superseded(verdict.id))
    clusterVerdicts.forgetAllBut(pass.seen)
    inline.asked.filterInPlace(pass.shapes)
    (candidates.candidateFilms.keys.toSet -- pass.seen).foreach(candidates.candidateFilms.remove)   // a correction's candidates are read, never kept
    corrections.correctionWaits --= corrections.correctionWaits.keySet.toSet -- pass.correcting
    takes.forgetAllBut(pass.seen)
    if (removed.nonEmpty || moved.nonEmpty) verdicts.store(removed, moved.toSeq)
    val current = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[ResolverDecision, java.lang.Boolean]())
    resolution.decisions.foreach(current.add)
    takenAs.keySet.retainAll(current)   // the model's decisions this resolution no longer holds
    digested.keySet.retainAll(current)
    events.keySet.retainAll(current)
  }

  /** The verdicts kept, by their cluster's key ([[VerdictIndex.memo]]). */
  def held: services.identity.Memo[String, StoredVerdict] = verdicts.memo

  /** What each cluster waits on, as the last apply left it ([[StageWaits]]): the waits the pipeline's stages keep, each
   *  cluster named by `keyOf`, and the takes standing meanwhile (`standingTakes`). */
  def waits(keyOf: ResolverDecision => String, standingTakes: StandingTakes, givenUp: Question => Boolean): StageWaits =
    new StageWaits(clusterVerdicts.waiting, takes.takeWaits, corrections.correctionWaits, keyOf, standingTakes, digested,
      id => verdicts.get(id), evidence.readNow, evidence.tried, givenUp)

  /** How many clusters wait on a family's answer. */
  def waitingClusters: Int = clusterVerdicts.waiting.size

  /** Has `question` been attempted ([[AgreementEvidence.tried]])? */
  def tried(question: Question): Boolean = evidence.tried(question)

  /** Is `year` another than the one the verdicts were read in? Then it is the year read from now, and every verdict that
   *  read another is read again ([[services.identity.Dependency.Year]]: an input no filing announces). */
  def yearTurned(year: Int): Boolean = year != evidence.yearNow && {
    evidence.yearNow = year
    verdicts.memo.moved(_ == services.identity.Dependency.Year, evidence.readNow)
    true
  }

  /** The verdicts whose receipt read a question's attempt as it no longer reads, read again: how many. */
  def uncheckAttempted(): Int = verdicts.memo.moved(VerdictIndex.isAttempt, evidence.readNow)

  /** An apply at `version` begins: the kept verdicts' and candidates' passes begun, and — on the stage's `first` apply,
   *  which meets every cluster and asks none inline — every verdict stored under these rules, of the year it read,
   *  trusted to stand: what this worker's families answered cannot have moved while it was down (prod PL 2026-10-05:
   *  re-reading every verdict's answers at boot was a 40 s, 514 MB pass). */
  def opened(version: Long, first: Boolean): Unit = {
    verdicts.memo.newPass(); candidates.candidateFilms.newPass()
    inline.firstPass = first
    if (first) {
      val year = evidence.readNow(services.identity.Dependency.Year)
      verdicts.memo.values.filter(v => v.rules == decidedUnder && v.reads.get(services.identity.Dependency.Year).forall(year.contains))
        .foreach(v => verdicts.check(v.id, v.listings, version))
    }
    evidence.version = version
  }

  /** The apply ends: the families' verdicts it did not read again dropped. */
  def closed(): Unit = evidence.familyVerdicts.applied()

  /** Forget the listings read for every decision: each read again. */
  def forgetAllRead(): Unit = digested.clear()
}
