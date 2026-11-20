package services.identity.agreement

import services.identity.{IdentityCalibration, Listing, Question, Resolution, ResolverDecision}
import models.ListingKey

import scala.collection.mutable


/**
 * The film a cluster the model left unmatched is, when ≥3 other film database families agree on it ([[Agreement]]) —
 * applied to the model's resolution on its way to the projection, never inside the model: only clusters TMDB matched
 * to nothing, with every TMDB question answered and no fallback film taken, are asked, so the model's footprint and
 * its order-independence stay as they are.
 *
 * An agreed film TMDB holds (its record links a TMDB id, or its IMDb id finds one: `tmdbOf`) is the cluster's film —
 * an ordinary TMDB match the projection fetches details for; one only other databases hold is its fallback film, by
 * its IMDb id — else, where none links one, by the own id of the first of `identities` (the families the country lets a
 * film stand on: Wikidata's, and Filmweb's in Poland alone) that names it ([[ResolverDecision.Fallback]]).
 * Every question a family could not answer yet is a gap — and so is an agreed IMDb id TMDB was not asked about yet:
 * the cluster stays as the model left it, and the stage hands the questions to `ask` (the queue) the moment it meets
 * them, with every stale answer it read — so a question is asked whenever an answer could be used: the projection runs
 * whenever the listings' facts move, and again once an answer it asked for is filed.
 *
 * Each cluster's verdict is kept in `stored` ([[AgreementVerdicts]]) with the digest of its listings and TMDB's own vote (the film the model
 * leans to, else the best candidate it weighed — which may complete an agreement), and of every answer it read, as the model keeps its families: it stands,
 * across restarts too, while none of those moved, and the resolver is asked again only for a cluster whose own listings,
 * vote or answers did. `version` (how many answers the
 * families filed) spares re-reading a standing verdict's answers while nothing was filed at all.
 *
 * The cluster's venue POSTERS ([[PosterEvidence]]) are read last, against the TMDB films the cluster's own evidence
 * reaches (`tmdb`'s candidates, none of them denied): a film the families agree on is not taken when a venue poster
 * matches another candidate and not it (the VETO), and a cluster nothing took takes the one candidate a venue poster
 * matches (the VOTE, [[ResolverDecision.Basis.Poster]]). A poster not hashed yet is a gap like a family's question:
 * the cluster waits as the model left it, and the poster is handed to `ask`. `version` must count the posters filed too.
 * So are the venues' own SYNOPSES ([[SynopsisEvidence]]), against the TMDB descriptions of those films and of what the
 * cluster's answered title searches find: a film taken — agreed or filled — whose namesake's description the synopsis
 * names over its own is REFUSED, the line saying why. A description not known is nothing to compare, never waited on.
 *
 * A cluster neither took is read last by the days its venues screen it on ([[Broadcast]]): a stage relay screening on
 * the day one record of its work was broadcast is that production ([[ResolverDecision.Basis.Broadcast]]).
 *
 * A cluster whose every listing bills an event no film database holds — a concert, a class, a marathon, a secret
 * screening ([[NonFilmEvents]]) — is none of the above's: no family is asked about it and no poster hashed, and it is
 * reported as an event, not a film ([[ResolverDecision.Basis.Event]]), its card shown as any no-match's. A model take
 * on such a cluster is not read against the evidence that corrects one either: it stands as the model took it.
 *
 * A cluster nothing above took — no agreement, poster, broadcast or fill — takes the film its listings' own catalogue
 * ids name ([[Catalogue]], [[ResolverDecision.Basis.Catalogue]]), read from `catalogue`: an id not mapped yet, or a venue
 * page whose links are not read yet, is a gap the stage hands to `ask`, as a family's question is. Last, so an exact id
 * never moves a film another rule took: where it names another, the projection keeps the take, for the measure to list.
 *
 * A film the MODEL took ([[ResolverDecision.Basis.OwnMatch]], [[ResolverDecision.Basis.PooledMatch]]) is read against the
 * evidence that can contradict it ([[Correction]]): its venue posters — first against the taken film's own posters,
 * the other candidates' hashed only where none of those comes within [[PosterEvidence.VetoBits]],
 * `correctionPostersAtOnce` at a time ([[AgreementStage.CorrectionPostersAtOnce]]) — and, once a poster names another
 * film, the families and the venues' programmes on `listedOn`'s site (Filmweb, in Poland: [[VenueListings]]), asked
 * only of a take a poster questions. The take is withdrawn ([[ResolverDecision.Basis.Withdrawn]]) or switched
 * ([[ResolverDecision.Basis.Corrected]]) as [[Correction.decide]] says, and decided again whenever an answer it read is
 * filed again or its listings move. A take met in a new shape — a listing joined it — has the families its correction
 * waits on asked inline, as a new cluster's are ([[newCorrection]]), and its venue posters hashed already read while
 * another is not: so its first take is the corrected one, the order its listing and their answers arrive in moving none.
 *
 * A cluster waiting on its families' questions SETTLES once every question it waits on has been attempted ([[settles]],
 * docs/design/identity-questions.md step 4): answered, durably unanswered, or asked and failed (`attempted`, the
 * memory's record of each ask; an answer counts by itself). It is then decided on the families that answered, the rest
 * left out as no verdict. A question merely handed to `ask` holds it; no clock does. Settling ends the wait, not the
 * asking: the questions none answered stay asked on every pass ([[unreached]]), and the first of them answered reads the
 * cluster again, so a later answer can always move the take. The one rule holds for every question a take reads — a
 * find, a poster, a catalogue page or id, a record's day, a family's question read by a correction or the broadcast
 * join: one attempted and not answered is read as no evidence ([[settled]]), the take decided without it, the question
 * asked on every pass and read again once answered. `attemptsMarked`, when given, counts the writes of the memory behind
 * `attempted`: an apply at the same version reads what it waits on again only once that count moved.
 *
 * `staging`, when given, is told each wait as it starts and ends ([[services.identity.staging.StagingEvent]]): a cluster
 * the apply put to wait, one it stopped waiting on, and a new cluster whose families it asked inline and decided within
 * the one apply — the waits the staging screen would never see by sampling. Only an apply that reads anything tells it;
 * a quiet one changes no wait.
 *
 * Each cluster is NAMED by its KEY ([[keyOf]], docs/design/identity-cluster-identity.md): a counter of its own, assigned
 * by overlap as the projection's film ids are ([[StableNames]], [[ClusterKeys]]), and kept in its store with each cluster's first
 * listing, which a restart re-attaches it by — what the staging screen and the drift check name a cluster by. A key
 * decides nothing (docs/design/identity-no-drift.md: every change re-evaluates everything as if all the data had been
 * there from the start): a verdict is found by its listings under whichever key it was filed ([[heldFor]]), and stands
 * while everything it read stands — its RECEIPT ([[StoredVerdict.reads]]: every family answer, the record of the film
 * TMDB leans to, all its fill reads, the year — each answered or not), which a filing of any of it reopens; a cluster
 * settles on what the question memory says, never on its own past, and a merge carries no memory with its key.
 */
final class AgreementStage private[agreement] (
                           /** What each model decision is taken as ([[AgreementPipeline]]): the stage runs it over each
                            *  resolution, an apply at a time — what is read when, kept how long, served how. Built with
                            *  everything it reads by the stage's builders ([[AgreementStageComposition]]). */
                           private[agreement] val pipeline: AgreementPipeline,
                           private[agreement] val ask: Set[Question] => Unit = _ => (), private[agreement] val metrics: AgreementStage.Metrics = AgreementStage.Metrics.Silent,
                           private[agreement] val correctionPostersAtOnce: Int = AgreementStage.CorrectionPostersAtOnce,
                           private[agreement] val correctionsPerApply: Int = AgreementStage.CorrectionsPerApply,
                           private[agreement] val backfillPerApply: Int = AgreementStage.BackfillPerApply,
                           /** A question attempted and given up on for good (the question memory's durable non-answer, or
                            *  its give-up after its failures in a row): absent, not pending. One failed and in its backoff
                            *  is attempted -- the settle rule decides without it -- but still pending (P1): what
                            *  [[StageWaits.outcomeOf]] types a settled take by. By default every attempted question is given up on. */
                           private[agreement] val givenUp: Question => Boolean = _ => true,
                           private[agreement] val attemptsMarked: Option[() => Long] = None,
                           private[agreement] val staging: Option[Seq[services.identity.staging.StagingEvent] => Unit] = None,
                           private[agreement] val clusterKeys: ClusterKeys,
                           private[agreement] val pressed: () => Boolean = () => false,
                           private[agreement] val fullValidationEvery: Int = AgreementStage.FullValidationEvery) {

  /** The clock the pipeline reads: the year, the takes' holds, the staging screen's times. */
  private def clock: java.time.Clock = pipeline.clock
  /** The TMDB records and searches this apply read, published at its end. */
  private def tmdbReads: TmdbReadTracking = pipeline.tmdbReads

  /** What a stored verdict was decided under ([[AgreementPipeline.decidedUnder]]). */
  private[agreement] def decidedUnder: String = pipeline.decidedUnder

  /** Every question the last [[apply]] met unanswered or stale, one each: the families' questions, TMDB's finds of agreed
   *  IMDb ids, the posters not hashed yet (the corrections' a few at a time), the catalogue ids not mapped and venue pages
   *  not read yet, and the TMDB records a broadcast take waits on to read again — filed before records kept the whole
   *  release day ([[IdentityLookups.releaseDay]]). A caller after one kind matches it out. */
  def wanted: Set[Question] = questions.wanted
  /** The questions on their way to `ask`, and what of them is awaited. */
  private[agreement] val questions = new QuestionHandOver(ask, attemptsMarked)

  /** `decision`'s cluster key ([[StableNames]]) as the last apply left it ([[ClusterKeys.keyOf]]): read from any thread. */
  def keyOf(decision: ResolverDecision): String = clusterKeys.keyOf(decision)

  /** Does a verdict the stage keeps read the TMDB record or search a filing under `id` comes as? Then that filing must
   *  reach the stage, as every answer it read does: the verdict is a cache of what it read — each kept verdict a cache of
   *  its receipt ([[AgreementPipeline.held]]), indexed by every TMDB record and search it read. Read from any thread. */
  def reads(id: String): Boolean = pipeline.reads(id)

  /** The `version` the stored verdicts were loaded at: what this worker's families answered cannot have moved while it
   *  was down — it alone files them — so a verdict kept under these rules, its listings as they were, stands then without
   *  a single answer read again (prod PL 2026-10-05: re-reading every verdict's answers at boot was a 40 s, 514 MB pass). */
  private[agreement] var loadedAt: Option[Long] = None
  /** Each cluster's last settled take, standing while the same listings' take waits ([[StandingTakes]]). */
  private[agreement] val standingTakes = new StandingTakes(clock)

  /** What each cluster waits on, as the last [[apply]] left it, and what a decision comes to meanwhile ([[StageWaits]]):
   *  read from any thread. */
  val waits: StageWaits = pipeline.waits(keyOf, standingTakes, givenUp)

  /** The last resolution applied to, at which `version`, and what it came to: a tick handed the same decisions while
   *  no answer was filed gets those again, nothing re-read. */
  private[agreement] var last: Option[(Resolution, Long, Resolution)] = None
  /** Did the last [[apply]] leave a model take unread for want of its budget ([[AgreementStage.CorrectionsPerApply]])? */
  @volatile private var unread = false

  /** Are model takes still to be read against the evidence, the last apply's budget spent before them? Until none is,
   *  the next apply of the same decisions reads on rather than hand back the last one's result: else a backlog
   *  beyond one apply's budget was read only as the model's decisions moved (US run 563: Burns Court's "NT Live:
   *  Hamlet", the 652nd of 2,024 takes, withdrawn only on the next day). */
  def backlog: Boolean = unread

  /** `resolution` with every unmatched, fully answered cluster its families agree on taken as that film. `republished`:
   *  the listings published again since the last apply equal to what the model holds but for a field the stage reads
   *  and the model does not — a poster, screening days, names ([[Listing.movedOutsideEquality]]) — which hand over no new
   *  decision; `None` when the caller cannot tell, and every cluster's listings are read again. */
  def apply(resolution: Resolution, listingOf: ListingKey => Option[Listing], version: Long,
            republished: Option[Iterable[ListingKey]] = Some(Nil)): Resolution = synchronized {
    val year = java.time.LocalDate.ofInstant(clock.instant(), java.time.ZoneOffset.UTC).getYear
    // the year an input no filing announces: its turning reaches the verdicts that read another year, through the index
    if (pipeline.yearTurned(year)) last = None
    val reread = republished.fold { pipeline.forgetAllRead(); true }(pipeline.forgetRead(resolution, _))
    val waited = questions.attemptedSince(pipeline.tried)
    if (waited) uncheckAttempted()
    last.collect { case (was, at, came) if !reread && !waited && !unread && !standingTakes.due && at == version && sameDecisions(was, resolution) =>
      resolution.copy(decisions = came.decisions) }.getOrElse {
      val came = applied(resolution, listingOf, version)
      last = Some((resolution, version, came))
      came
    }
  }

  /** An attempt marked since the last apply: the verdicts whose receipt read a question's attempt as it no longer reads
   *  ([[Dependency.Attempted]]) are read again — those alone. A receipt naming an attempt is not enough: the fill notes
   *  one for nearly every verdict, and unchecking every such verdict on any attempt anywhere read ~all receipts again
   *  (worker-de and -uk from 2026-10-25 19:00Z: ~58 s and 11 GB an agreement phase). A correction's receipt names none,
   *  and has no reading of it again but afresh (run 1641). Found through the held verdicts' push index ([[Memo.moved]]):
   *  each attempt a receipt names read once, and only its readers visited. How many it unchecked: [[attemptUnchecked]]. */
  private[agreement] def uncheckAttempted(): Unit =
    attemptUnchecked = pipeline.uncheckAttempted()
  /** How many verdicts the last attempt marked read again ([[uncheckAttempted]]). */
  private[agreement] var attemptUnchecked = 0


  /** The same decisions, object for object: the model hands each unchanged decision over as the same object. */
  private[agreement] def sameDecisions(a: Resolution, b: Resolution): Boolean = {
    val (x, y) = (a.decisions.iterator, b.decisions.iterator)
    while (x.hasNext && y.hasNext) if (!(x.next() eq y.next())) return false
    !x.hasNext && !y.hasNext
  }

  /** One apply that reads, in its phases: [[opened]] (the pass, its keys, its reads begun), [[decidedAll]] (each
   *  model decision read by [[AgreementPipeline.decide]]), [[servedAll]] (what is kept and served of them), the staging screen told the
   *  waits that moved ([[tellWaits]]), and [[closed]] (what the apply read and asks, handed over). */
  private[agreement] def applied(resolution: Resolution, listingOf: ListingKey => Option[Listing], version: Long): Resolution = {
    val started      = tools.Stopwatch.start()
    val pass         = opened(resolution, version)
    val waitedBefore = staging.map(_ => waits.waitingClusters)
    val served       = servedAll(resolution, decidedAll(resolution, listingOf, pass), pass)
    staging.zip(waitedBefore).foreach { case (tell, before) => tellWaits(tell, before, pass) }
    closed(pass)
    report(served, pass, started)
    resolution.copy(decisions = served)
  }

  /** The apply at `version` begins: its [[AgreementPass]], the memos' passes, each decision of `resolution` keyed
   *  ([[ClusterKeys.assign]]), on the stage's first pass every verdict stored under these rules trusted ([[AgreementPipeline.opened]]),
   *  TMDB's reads begun afresh, and the full validation's turn taken ([[validating]]). */
  private def opened(resolution: Resolution, version: Long): AgreementPass = {
    val pass = new AgreementPass(version, correctionsPerApply, pipeline.reading(), backfillPerApply)
    clusterKeys.assign(resolution, first = loadedAt.isEmpty, pass.keys, pass.merged)
    val first = loadedAt.isEmpty
    if (first) loadedAt = Some(version)
    pipeline.opened(version, first)
    tmdbReads.begin()
    validating(pass)
    pass
  }


  /** Each model decision of `resolution` as the pipeline takes it ([[AgreementPipeline.decide]]), in content order ([[AgreementStage.inContentOrder]]) —
   *  what it was taken as keyed as the decision is: the projection's takes are these. */
  private def decidedAll(resolution: Resolution, listingOf: ListingKey => Option[Listing], pass: AgreementPass): Seq[ResolverDecision] = {
    val decisions = AgreementStage.inContentOrder(resolution.decisions)(pipeline.decide(_, listingOf, pass))
    resolution.decisions.iterator.zip(decisions).foreach { case (model, took) =>
      if (!(took eq model)) { val key = pass.keys.get(model); if (key != null) pass.keys.put(took, key) }
    }
    decisions
  }

  /** The takes served of what the apply `took`: its keys published, what the clusters gone were kept with forgotten and
   *  the verdicts it moved stored ([[AgreementPipeline.forgetGone]]), and each take as it stands ([[StandingTakes]]). */
  private def servedAll(resolution: Resolution, took: Seq[ResolverDecision], pass: AgreementPass): Seq[ResolverDecision] = {
    clusterKeys.publish(pass.keys)
    pipeline.forgetGone(resolution, pass)
    standingTakes.served(resolution, took, pass, waits.openWaits)
  }

  /** The apply ends: what it read of TMDB published, what it met unanswered handed to the queue, whether its budget left
   *  a take unread, and the families' verdicts it did not read again dropped. */
  private def closed(pass: AgreementPass): Unit = {
    tmdbReads.publish()
    questions.handOver(pass.wanted(correctionPostersAtOnce, pipeline.tried), pass.version, pipeline.tried)
    unread = pass.spent
    pipeline.closed()
  }

  /** The questions `listing`'s own facts lead the stage to ask ([[AgreementPipeline.questionsOf]]): what the model's input
   *  gate waits on before a listing's version is taken in. */
  def questionsOf(listing: Listing): Set[Question] = pipeline.questionsOf(listing)

  /** Makes every `fullValidationEvery`th apply that reads anything, unless the heap is `pressed`, a FULL VALIDATION: up to
   *  [[AgreementStage.FullValidationBudget]] of the kept verdicts the dirty index calls standing ([[Memo.clean]]) are read
   *  again whole ([[Memo.verified]]) — the next budget's worth each time, round the set — and one whose receipt moved
   *  unannounced is DRIFT, counted and decided again. The proof the index equals full validation: `drifted` must read
   *  zero. A correction stands on its filings alone, and is not read. */
  private[agreement] def validating(pass: AgreementPass): Unit = {
    appliesSinceValidation += 1
    if (appliesSinceValidation >= fullValidationEvery && !pressed()) {
      appliesSinceValidation = 0
      if (validatedFrom >= pipeline.heldCount) validatedFrom = 0
      pass.validateFrom(validatedFrom, AgreementStage.FullValidationBudget)
      validatedFrom += AgreementStage.FullValidationBudget
    }
  }
  private var appliesSinceValidation = 0
  private var validatedFrom          = 0

  /** Tell `staging` the waits this apply started and ended, against those it began with (`before`): a cluster put to
   *  wait (since its inline ask, when it had one), one asked inline and decided within the apply, and one still a cluster
   *  that waits no more. A cluster gone — regrouped, or taken by the model — is no wait ended: its listings are another
   *  cluster's now. */
  private def tellWaits(tell: Seq[services.identity.staging.StagingEvent] => Unit, before: Set[String], pass: AgreementPass): Unit = {
    import services.identity.staging.StagingEvent
    val after  = waits.waitingClusters
    val now    = clock.millis()
    val events = Vector.newBuilder[StagingEvent]
    (after -- before).toSeq.sorted.foreach(id => events += StagingEvent.Waiting(id, pass.inline.get(id).fold(now)(_._1), waits.questionsOf(id)))
    pass.inline.toSeq.sortBy(_._1).foreach { case (id, (at, asked)) =>
      if (!after(id) && !before(id)) { events += StagingEvent.Waiting(id, at, asked); events += StagingEvent.Settled(id, now) }
    }
    (before -- after).toSeq.sorted.filter(id => pass.seen(id) || pass.correcting(AgreementStage.correctionId(id)))
      .foreach(id => events += StagingEvent.Settled(id, now))
    pass.merged.sorted.foreach { case (into, from) => events += StagingEvent.Merged(into, from, now) }
    val told = events.result()
    if (told.nonEmpty) tell(told)
  }









  /** What the apply came to, for [[metrics]]. */
  private[agreement] def report(decisions: Seq[ResolverDecision], pass: AgreementPass, started: tools.Stopwatch.Started): Unit =
    metrics.applied(AgreementStage.Applied.of(decisions, questions.wanted, pipeline.waitingClusters, () => pipeline.heldVerdicts(), decidedUnder, pass,
      standingTakes.held(), started.seconds))














}

object AgreementStage {
  /** Told what each [[AgreementStage.apply]] that read anything came to ([[Applied]]). */
  trait Metrics { def applied(applied: Applied): Unit }
  object Metrics { val Silent: Metrics = _ => () }

  /** What one [[AgreementStage.apply]] that read anything came to: the clusters waiting on a family's answer, the verdicts
   *  kept and how many agreed, the decisions taken as a TMDB film or an IMDb fallback, the questions still open per family
   *  and TMDB finds, how many clusters it resolved again, and how long it took; and the venue posters' part: the decisions
   *  they took ([[ResolverDecision.Basis.Poster]]), the agreed films they vetoed, and the posters not hashed yet; and the
   *  decisions the screening days took ([[ResolverDecision.Basis.Broadcast]]), a fill rule took
   *  ([[ResolverDecision.Basis.Filled]]) and the listings' catalogue ids took ([[ResolverDecision.Basis.Catalogue]]), with
   *  the catalogue questions still open, and the TMDB records the broadcast take waits on to read again; and the verdicts
   *  decided under other rules still to be decided again (`incomplete`: a deploy's backfill, [[BackfillPerApply]]);
   *  and the full validation's kept verdicts read again and those found drifted ([[FullValidationEvery]]). */
  final case class Applied(waiting: Int, verdicts: Int, agreed: Int, takenTmdb: Int, takenFallback: Int, open: Map[VoterFamily, Int],
                           finds: Int, resolves: Int, seconds: Double, takenPoster: Int = 0, posterVetoed: Int = 0, posters: Int = 0,
                           takenBroadcast: Int = 0, takenFilled: Int = 0, takenCatalogue: Int = 0, catalogue: Int = 0, undated: Int = 0,
                           withdrawn: Int = 0, corrected: Int = 0, correcting: Int = 0, correctionPosters: Int = 0,
                           events: Int = 0, incomplete: Int = 0, validated: Int = 0, drifted: Int = 0,
                           held: Held = Held.None, holdsEnded: Seq[Double] = Nil, holdsBounded: Seq[Double] = Nil)
  object Applied {
    /** What an apply at `pass` came to: its `decisions`, the questions it left `wanted`, the clusters `waiting` and the
     *  verdicts `held` (decided under `decidedUnder`, or still to be decided again), the takes standing (`standing`) and
     *  how long it took. */
    def of(decisions: Seq[ResolverDecision], wanted: Set[Question], waiting: Int, held: () => Iterator[StoredVerdict], decidedUnder: String,
           pass: AgreementPass, standing: Held, seconds: Double): Applied = {
      val agreedNow = decisions.filter(_.basis == ResolverDecision.Basis.Agreed)
      val byBasis   = decisions.groupMapReduce(_.basis)(_ => 1)(_ + _)
      def taken(basis: ResolverDecision.Basis) = byBasis.getOrElse(basis, 0)
      val open      = mutable.Map.empty[VoterFamily, Int]
      var finds, postersOpen, catalogueOpen, undated = 0
      wanted.foreach {
        case Question.Family(family, _) => open(family) = open.getOrElse(family, 0) + 1
        case Question.Find(_)           => finds += 1
        case Question.Poster(_)         => postersOpen += 1
        case Question.Catalogue(_)      => catalogueOpen += 1
        case Question.Record(_)         => undated += 1
        case _                          => ()
      }
      Applied(waiting = waiting, verdicts = held().count(_.correction.isEmpty), agreed = held().count(_.agreed.isDefined),
        takenTmdb = agreedNow.count(_.film.isDefined), takenFallback = agreedNow.count(_.fallback.isDefined),
        open = open.toMap, finds = finds, resolves = pass.resolves, seconds = seconds,
        takenPoster = taken(ResolverDecision.Basis.Poster), posterVetoed = pass.posterVetoed, posters = postersOpen,
        takenBroadcast = taken(ResolverDecision.Basis.Broadcast), takenFilled = taken(ResolverDecision.Basis.Filled),
        takenCatalogue = taken(ResolverDecision.Basis.Catalogue), catalogue = catalogueOpen, undated = undated,
        withdrawn = taken(ResolverDecision.Basis.Withdrawn), corrected = taken(ResolverDecision.Basis.Corrected),
        correcting = pass.correcting.size, correctionPosters = pass.correctionPosters.size,
        events = taken(ResolverDecision.Basis.Event),
        incomplete = held().count(_.rules != decidedUnder), validated = pass.validated.read, drifted = pass.validated.drifted,
        held = standing, holdsEnded = pass.holdsEnded.toSeq, holdsBounded = pass.holdsBounded.toSeq)
    }
  }
  /** The previous takes standing after an apply ([[StandingTakes]]): the `listings` they hold, the oldest's
   *  `oldestSeconds` (0 with none). */
  final case class Held(listings: Int, oldestSeconds: Double)
  object Held { val None: Held = Held(0, 0.0) }




  /** `decisions` each decided by `decide` in their content's order — by the least of their listings' keys, which no two
   *  clusters share — and handed back in theirs: the budgets ([[AgreementStage.BackfillPerApply]], [[AgreementStage.CorrectionsPerApply]]) reach the
   *  same clusters first however the resolution was read, never by an iteration or arrival order. The model hands them
   *  over in that order already ([[services.identity.IncrementalResolver.decisions]]), so it is only checked. */
  private[agreement] def inContentOrder(decisions: Seq[ResolverDecision])(decide: ResolverDecision => ResolverDecision): Seq[ResolverDecision] = {
    val byFirst = Ordering.Option(using ListingKey.ordering)
    val firsts  = decisions.iterator.map(_.members.minOption(using ListingKey.ordering)).toIndexedSeq
    if (firsts.indices.drop(1).forall(i => byFirst.lteq(firsts(i - 1), firsts(i)))) decisions.map(decide)
    else {
      val all     = decisions.toIndexedSeq
      val decided = new Array[ResolverDecision](all.size)
      all.indices.sortBy(firsts)(using byFirst).foreach(i => decided(i) = decide(all(i)))
      scala.collection.immutable.ArraySeq.unsafeWrapArray(decided)
    }
  }



  /** 64 bits of `parts`' text, two MurmurHash3 seeds: stable across runs, as a stored digest must be. */
  def digest(parts: Seq[String]): Long = {
    val text = parts.mkString("\u0000")
    (scala.util.hashing.MurmurHash3.stringHash(text, 0x2f1d7a3b).toLong << 32) | (scala.util.hashing.MurmurHash3.stringHash(text, 0x6c8e9cf5).toLong & 0xffffffffL)
  }

  /** What a TMDB record read again for its day is filed as among the answers' changes ([[AnswerChanges]]):
   *  [[Question.Record]]'s id. */
  def recordReadId(tmdbId: Int): String = Question.Record(tmdbId).id.get
  /** The TMDB record a [[recordReadId]] names. */
  def recordOf(id: String): Option[Int] = Question.parse(id).collect { case Question.Record(film) => film }
  /** What TMDB's find of an IMDb id is filed as ([[services.identity.Question.Find]]'s id). */
  val FindPrefix = "find|"


  /** [[PosterQuestion]] by the name it had here: the tool files compiled into older refs
   *  (`WhatIfAnswers`, `OlderRefs`) name it so, which both a ref before its move and one after compile. */
  type PosterQuestion = _root_.services.identity.agreement.PosterQuestion
  val PosterQuestion: _root_.services.identity.agreement.PosterQuestion.type = _root_.services.identity.agreement.PosterQuestion


  /* A model take's correction (Correction) is stored as its listings' digest and the `version` it was read (or last
   * found standing) at, the digests of the answers it read — a filing of one decides it again — what it came to, and
   * the questions it waits on. */

  /** A model take's correction's id among the stored verdicts. */
  def correctionId(cluster: String): String = s"$CorrectionPrefix$cluster"

  /** How long a cluster's previous take stands at most while its take waits ([[StandingTakes]]): the input
   *  gate's bound (`ModelInputGate.MaxHold`), one bound for every hold of a take on its answers. */
  val HoldAtMost: java.time.Duration = java.time.Duration.ofMinutes(10)
  /** What a correction's stored id is its cluster's id behind. */
  val CorrectionPrefix: String = VerdictIndex.CorrectionPrefix
  /** How many model takes with a decided correction standing — decided under a deploy's previous rules, or before an
   *  answer it read was filed again — one apply reads the evidence of afresh, the rest standing as decided until a later
   *  apply: a deploy's backfill spread over applies, never one boot's spike. A take with none standing — never read at
   *  its shape, or read waiting on an answer — is read at once: served as the model took it until a later apply's budget
   *  reached it, it was an interim take its correction overrode at an apply none of its inputs moved at (the drift
   *  replay's flips, convergence run 3656: UK "The Ties That Bind Us", "Cosi Fan Tutte", US "NT Live: Hamlet"). */
  val CorrectionsPerApply = 200
  /** How many verdicts decided under other rules one apply decides again: a deploy's backfill of every stored verdict
   *  (prod PL 2026-10-05: re-reading them all at boot was a 40 s, 514 MB pass; worker-us OOM-killed on a comparable
   *  working set) spread over applies, the rest standing as they were meanwhile. */
  val BackfillPerApply = 500

  /** How many of the posters the model takes' corrections wait on are handed to the queue at once: a one-off backfill of
   *  every take's posters (17.6k over the five corpora, 2026-10-06) drained a few at a time behind every other task. */
  val CorrectionPostersAtOnce = 64
  /** How often the kept verdicts are validated whole: one apply in this many that reads anything, as the projection
   *  reconciles the whole corpus between its scoped ticks. */
  val FullValidationEvery = 12
  /** How many kept verdicts one full validation reads again, resuming where the last stopped: the whole set read over
   *  successive validations, never one apply's spike (re-reading every verdict at once was prod PL's 40 s, 514 MB boot). */
  val FullValidationBudget = 2000



  /** Asks families' questions now, on the calling thread, for at most `within`, filing what they answer: a cluster with a
   *  newly scraped listing reads its families' answers in its first take (docs/design/identity-questions.md, step 3, the
   *  owner's choice). A question not answered by then is handed to the queue as every other is. */
  trait AskNow { def apply(questions: Set[Question], within: scala.concurrent.duration.FiniteDuration): Unit }
  object AskNow {
    val Never: AskNow = (_, _) => ()
  }
  /** How long a new cluster's resolve waits on its families asked inline, every round of them together. */
  val AskNowWithin: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(5, java.util.concurrent.TimeUnit.SECONDS)
  /** How many rounds of inline asks a new cluster's resolve makes at most: its families' questions, then those their
   *  answers lead to — a search's hits' records — and so on, each round asking only what none before it did. */
  val InlineRounds = 4

  /** The rules the stage decides by: a digest of its code and all it reaches (`agreement-rules-version.txt`, generated by
   *  `build.sbt` from `IdentityRulesSources.AgreementRoots`). A stored verdict decided under others is decided again. */
  lazy val RulesVersion: String = services.identity.ClasspathResource.stamp("agreement-rules")

  /** The stage's configuration as a verdict's rules name it: the families it asks, those a film may stand on, the site
   *  whose programmes it joins and the calibration. The worker picks them per country in code no rules version digests
   *  (`AgreementWiring.agreementFamilies`, `identities`, `listedOn`): a verdict decided without Filmweb stood
   *  as it was once Filmweb was asked, since a receipt names only the answers its verdict read. */
  private[agreement] def configured(families: Set[VoterFamily], identities: Seq[VoterFamily], listedOn: Option[VoterFamily],
                                    calibration: IdentityCalibration): String =
    Seq(families.toSeq.sortBy(_.ordinal).map(_.label).mkString(","), identities.map(_.label).mkString(","),
      listedOn.fold("")(_.label), calibration.##.toString).mkString(";")
}
