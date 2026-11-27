package services.identity.agreement

import scala.collection.mutable

import models.ListingKey
import services.identity.measures.{Listing, Resolution, ResolverDecision}
import services.identity.vocabulary.{Question, VoterFamily}
import services.identity.NonFilmEvents

/**
 * What each model decision is TAKEN AS ([[decide]]): a model take read by its CORRECTION ([[AgreementCorrections]]), a
 * no-match by its families' VERDICT ([[ClusterVerdicts]]) and that verdict by its TAKE ([[AgreementTakes]]) — an event no
 * film database holds ([[NonFilmEvents]]) by none of them. Each stage is handed the evidence it reads
 * ([[AgreementEvidence]]) and what else it needs, never the pipeline; [[AgreementStage]] runs the pipeline over each
 * resolution, an apply at a time. Each verdict is decided under [[decidedUnder]]. Built by the stage's builders
 * ([[AgreementStageComposition]], the specs' `AgreementStageFixture`) and handed to the stage whole: the stage keeps
 * only what an apply does with it.
 */
private[agreement] final class AgreementPipeline(sources: AgreementSources.StageSources, identities: Seq[VoterFamily],
                                                 rules: services.identity.agreement.UnifiedRules, listedOn: Option[VoterFamily],
                                                 askNow: AgreementStage.AskNow,
                                                 private[agreement] val clock: java.time.Clock,
                                                 private[agreement] val tmdbReads: TmdbReadTracking = new TmdbReadTracking) {

  /** What every stage of the pipeline reads, through the receipt being made ([[AgreementEvidence]]). */
  private[agreement] val evidence = new AgreementEvidence(sources, tmdbReads)
  import evidence.verdictsOf

  /** What a stored verdict was decided under: the stage's code, the selected rules' version, and what the composition
   *  root configures it with outside that code ([[AgreementStage.configured]]) — a refit of the rules, or a family asked
   *  or let a film stand in one more country, decides every verdict again, as a change of the code does. The
   *  calibration is named by the evidence scored under it ([[AgreementEvidence.calibrationVersion]]). */
  private[agreement] val decidedUnder: String =
    s"${AgreementStage.RulesVersion}|${rules.version}|${AgreementStage.configured(sources.voters, identities, listedOn, evidence.calibrationVersion)}"

  /** What one apply reads of the posters' hashes and of `listedOn`'s programmes, afresh each apply ([[AgreementPass.Read]]). */
  def reading(): AgreementPass.Read = new AgreementPass.Read(sources, listedOn)

  /** The verdicts kept ([[VerdictIndex]]), by their cluster's key and by their listings' digest. */
  private[agreement] val verdicts = new VerdictIndex(sources)

  /** What the pipeline read of each model decision, kept while the model hands that same object over ([[DecisionCaches]]). */
  private val caches = new DecisionCaches(evidence)
  import caches.{eventOf, reused}

  // THE PIPELINE ([[decide]]): a model take is read by its CORRECTION, a no-match by its families' VERDICT and that
  // verdict by its TAKE -- each stage handed the evidence it reads and the rest it needs, never the stage itself.
  /** The TMDB films a cluster's own evidence reaches, its posters' distances to them: what the takes and corrections read. */
  private[agreement] val candidates      = new AgreementCandidates(evidence, sources)
  /** A cluster met in a new shape has its families asked inline ([[askNow]]). */
  private[agreement] val inline          = new InlineAsker(askNow, clock)
  /** A model take read against the evidence that can contradict it. */
  private[agreement] val corrections = new AgreementCorrections(evidence, candidates, verdicts, inline, decidedUnder, listedOn)
  /** A no-match's take of its families' verdict: the agreed film, else the posters', the broadcast's, a fill's or the catalogue's. */
  private[agreement] val takes           = new AgreementTakes(evidence, candidates, rules, identities, sources)
  /** A no-match's families' verdict, held while what it read stands. */
  private[agreement] val clusterVerdicts = new ClusterVerdicts(evidence, verdicts, takes, inline, decidedUnder, clock)
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

  /** What a model no-match that bills a film is taken as: its cluster read, its families' verdict found or decided
   *  ([[ClusterVerdicts.verdictOf]]), and that verdict's take ([[AgreementTakes.verdictTake]]) — the decision as the model
   *  left it while there is none. What its take asks is noted in `pass`. */
  private def unmatched(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], pass: AgreementPass): ResolverDecision = {
    val AgreementEvidence.Cluster(id, listings, digest) = caches.clusterOf(decision, pass.keys.get(decision), listingOf)
    pass.seen += id
    pass.shapes += digest
    takes.verdictTake(decision, id, digest, listings, clusterVerdicts.verdictOf(decision, id, listings, digest, pass), pass)
  }

  /** The questions `listing`'s OWN facts lead the stage to ask -- its titles' and directors' searches and the records
   *  their hits name, as the stage asks them of any cluster holding it (the family verdicts over the listing alone, every
   *  gap they meet), and the posters the poster vote and veto read of it while they are not hashed: its venue poster
   *  ([[PosterEvidence.urls]]), then those of the films its own searches find ([[AgreementCandidates.postersOf]] over the listing
   *  alone). None for an event no film database holds ([[NonFilmEvents]]), whose families are never asked nor its poster
   *  hashed -- a title shaped as a franchise's marathon reads as one here, the model's searches that may make it a film's
   *  ([[ResolverDecision.filmTitled]]) not read yet: its families are asked once its decision says so. What the model's
   *  input gate waits on before a listing's version is taken in (docs/design/identity-structural-fixes.md, fix 1: asking
   *  apart from deciding): a population-independent set, a
   *  function of the listing and the answers filed. Without the posters, a listing joined a cluster with one unhashed, the
   *  take waited on it, and the model's "no film" stood until the poster was filed seconds later (prod 11-07..11-10: UK
   *  "Hadoota" x14 and ES "DUNE" x6 on their own venue posters; US "Parkway Drive: Home" x83 and UK "Touché" x5 on a
   *  candidate film's). */
  def questionsOf(listing: Listing): Set[Question] =
    if (NonFilmEvents.of(listing).isDefined) Set.empty
    else {
      val gaps = mutable.Set.empty[Question]
      verdictsOf(Seq(listing), gaps, mutable.Map.empty)
      candidates.postersOf(listing, gaps)
      gaps.toSet
    }

  /** Forget the listings read for each decision holding one of `keys`; whether one the pipeline reads — a cluster nothing
   *  took, a model take it corrects — holds any. */
  def forgetRead(resolution: Resolution, keys: Iterable[ListingKey]): Boolean = keys.nonEmpty && {
    val moved   = keys.toSet
    val touched = resolution.decisions.filter(_.members.exists(moved))
    caches.forgetRead(touched)
    touched.exists(decision => decision.film.isEmpty || correctable(decision))
  }

  /** Forget what the clusters `resolution` no longer holds were kept with — their verdicts, waits, candidates,
   *  corrections and the model's decisions — and store the verdicts `pass` moved. */
  def forgetGone(resolution: Resolution, pass: AgreementPass): Unit = {
    // a verdict under other rules decided again to a wait, none under these rules in its place: a cache of nothing now
    val renewed    = pass.moved.iterator.filter(_.rules == decidedUnder).map(_.id).toSet
    val superseded = pass.backfilled.toSet -- renewed
    val removed    = verdicts.ids -- pass.seen -- pass.correcting ++ superseded
    val moved      = pass.moved.filterNot(verdict => superseded(verdict.id))
    clusterVerdicts.forgetAllBut(pass.seen)
    inline.asked.filterInPlace(pass.shapes)
    candidates.forgetAllBut(pass.seen)   // a correction's candidates are read, never kept
    corrections.correctionWaits --= corrections.correctionWaits.keySet.toSet -- pass.correcting
    takes.forgetAllBut(pass.seen)
    if (removed.nonEmpty || moved.nonEmpty) verdicts.store(removed, moved.toSeq)
    caches.retainOnly(resolution)
  }

  /** Does a verdict kept, or this apply's TMDB reads, read the TMDB record or search a filing under `id` comes as? */
  def reads(id: String): Boolean = verdicts.reads(id) || tmdbReads.reads(id)
  /** How many verdicts are kept. */
  def heldCount: Int = verdicts.size
  /** The verdicts kept, read afresh at each call. */
  def heldVerdicts(): Iterator[StoredVerdict] = verdicts.values

  /** What each cluster waits on, as the last apply left it ([[StageWaits]]): the waits the pipeline's stages keep, each
   *  cluster named by `keyOf`, and the takes standing meanwhile (`standingTakes`). */
  def waits(keyOf: ResolverDecision => String, standingTakes: StandingTakes, givenUp: Question => Boolean): StageWaits =
    new StageWaits(clusterVerdicts.waiting, takes.takeWaits, corrections.correctionWaits, keyOf, standingTakes, caches.digested,
      id => verdicts.get(id), evidence, givenUp)

  /** How many clusters wait on a family's answer. */
  def waitingClusters: Int = clusterVerdicts.waiting.size

  /** Has `question` been attempted ([[AgreementEvidence.tried]])? */
  def tried(question: Question): Boolean = evidence.tried(question)

  /** Is `year` another than the one the verdicts were read in? Then it is the year read from now, and every verdict that
   *  read another is read again ([[services.identity.vocabulary.Dependency.Year]]: an input no filing announces). */
  def yearTurned(year: Int): Boolean = year != evidence.yearNow && {
    evidence.yearNow = year
    verdicts.yearMoved(evidence)
    true
  }

  /** The verdicts whose receipt read a question's attempt as it no longer reads, read again: how many. */
  def uncheckAttempted(): Int = verdicts.attemptsMoved(evidence)

  /** An apply at `version` begins: the kept verdicts' and candidates' passes begun, and — on the stage's `first` apply,
   *  which meets every cluster and asks none inline — every verdict stored under these rules, of the year it read,
   *  trusted to stand: what this worker's families answered cannot have moved while it was down (prod PL 2026-10-05:
   *  re-reading every verdict's answers at boot was a 40 s, 514 MB pass). */
  def opened(version: Long, first: Boolean): Unit = {
    verdicts.newPass(); candidates.newPass()
    inline.firstPass = first
    if (first) verdicts.trustStored(decidedUnder, version, evidence)
    evidence.version = version
  }

  /** The apply ends: the families' verdicts it did not read again dropped. */
  def closed(): Unit = evidence.applied()

  /** Forget the listings read for every decision: each read again. */
  def forgetAllRead(): Unit = caches.forgetAllRead()
}
