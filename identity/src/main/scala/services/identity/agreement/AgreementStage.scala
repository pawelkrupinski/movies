package services.identity.agreement

import services.identity.{Answer, CandidateQuery, CatalogueAnswers, Dependency, DetailFacts, Hit, IdentityCalibration, IdentityLookups,
  IdentityMeasures, Listing, ListingShape, Memo, PosterAnswers, PosterEvidence, PosterHash, Question, Resolution, ResolverDecision, StableNames,
  StoredFamily, WaitingOn}
import services.movies.{ListingKey, TitleNormalizer}

import scala.collection.concurrent.TrieMap
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
 * Each cluster is NAMED by its KEY ([[keyOf]], docs/design/identity-cluster-identity.md): a counter of its own, assigned
 * by overlap as the projection's film ids are ([[StableNames]]), and kept in `keyStore` with each cluster's first
 * listing, which a restart re-attaches it by — what the drift check names a cluster by. A key
 * decides nothing (docs/design/identity-no-drift.md: every change re-evaluates everything as if all the data had been
 * there from the start): a verdict is found by its listings under whichever key it was filed ([[heldFor]]), and stands
 * while everything it read stands — its RECEIPT ([[StoredVerdict.reads]]: every family answer, the record of the film
 * TMDB leans to, all its fill reads, the year — each answered or not), which a filing of any of it reopens; a cluster
 * settles on what the question memory says, never on its own past, and a merge carries no memory with its key.
 */
final class AgreementStage(private[agreement] val families: Map[VoterFamily, FamilyAnswers], venuePages: IdentityLookups, private[agreement] val normalizer: TitleNormalizer,
                           private[agreement] val calibration: IdentityCalibration, private[agreement] val tmdbOf: String => Answer[Option[Int]], private[agreement] val stored: AgreementVerdicts,
                           private[agreement] val ask: Set[Question] => Unit = _ => (), private[agreement] val metrics: AgreementStage.Metrics = AgreementStage.Metrics.Silent,
                           private[agreement] val clock: java.time.Clock, private[agreement] val changes: AnswerChanges = AnswerChanges.Unknown, private[agreement] val posters: PosterAnswers = PosterAnswers.Silent,
                           private[agreement] val tmdb: Option[IdentityLookups] = None, private[agreement] val identities: Seq[VoterFamily] = Nil,
                           private[agreement] val rules: services.identity.UnifiedRules = services.identity.UnifiedRules.resolver,
                           private[agreement] val catalogue: CatalogueAnswers = CatalogueAnswers.Silent, private[agreement] val listedOn: Option[VoterFamily] = None,
                           private[agreement] val correctionPostersAtOnce: Int = AgreementStage.CorrectionPostersAtOnce,
                           private[agreement] val correctionsPerApply: Int = AgreementStage.CorrectionsPerApply,
                           private[agreement] val backfillPerApply: Int = AgreementStage.BackfillPerApply,
                           private[agreement] val askNow: AgreementStage.AskNow = AgreementStage.AskNow.Never,
                           private[agreement] val attempted: Question => Boolean = _ => false,
                           private[agreement] val attemptsMarked: Option[() => Long] = None,
                           private[agreement] val keyStore: AgreementKeys = new InMemoryAgreementKeys,
                           private[agreement] val pressed: () => Boolean = () => false,
                           private[agreement] val fullValidationEvery: Int = AgreementStage.FullValidationEvery)
    extends AgreementCorrections, AgreementTakes, AgreementCandidates {

  /** What a stored verdict was decided under: the stage's code, the selected rules' version, and what the composition
   *  root configures it with outside that code ([[AgreementStage.configured]]) — a refit of the rules, or a family asked
   *  or let a film stand in one more country, decides every verdict again, as a change of the code does. */
  private[agreement] val decidedUnder: String =
    s"${AgreementStage.RulesVersion}|${rules.version}|${AgreementStage.configured(families.keySet, identities, listedOn, calibration)}"

  @volatile private var wantedNow: Set[Question] = Set.empty
  /** Every question the last [[apply]] met unanswered or stale, one each: the families' questions, TMDB's finds of agreed
   *  IMDb ids, the posters not hashed yet (the corrections' a few at a time), the catalogue ids not mapped and venue pages
   *  not read yet, and the TMDB records a broadcast take waits on to read again — filed before records kept the whole
   *  release day ([[IdentityLookups.releaseDay]]). A caller after one kind matches it out. */
  def wanted: Set[Question] = wantedNow

  /** What `decision`'s take waits on, as the last [[apply]] left it: the model's gaps, and the stage's own — its cluster's
   *  family questions or its fill's TMDB answers ([[waiting]]), and its take's correction's questions ([[correctionWaits]]).
   *  Read off the kept waits: a decision the stage does not wait on allocates nothing past its own. */
  def waitingOn(decision: ResolverDecision): WaitingOn =
    if (decision.members.isEmpty) WaitingOn.model(decision)
    else {
      val id         = keyOf(decision)
      val cluster    = waiting.get(id)
      val correction = correctionWaits.get(AgreementStage.correctionId(id))
      if (cluster.isEmpty && correction.isEmpty) WaitingOn.model(decision)
      else WaitingOn(
        decision.unanswered,
        // a question attempted is waited on no more: asked still, the take decided without it
        (cluster.fold(Set.empty[Question])(_.gaps) ++ correction.fold(Set.empty[Question])(_.gaps)).filterNot(tried),
        onTmdb = cluster.exists(_.blindAt.isDefined))
    }

  /** `decision`'s cluster key ([[StableNames]]) as the last apply left it: of a model decision it was handed, or of what
   *  it took one as. Read off maps no apply changes once built, from any thread, allocating nothing; a decision no apply
   *  was handed is keyed by its first listing ([[StoredFamily.idOf]]), as nothing the stage keeps is. */
  def keyOf(decision: ResolverDecision): String = {
    val now = keyed.get(decision)
    if (now != null) now
    else {
      val before = keyedBefore.get(decision)
      if (before != null) before else StoredFamily.idOf(decision.members)
    }
  }

  /** Each decision's cluster key: of the model's decisions the last apply that read anything was handed, and of what it
   *  took them as. Built whole by that apply and never changed after, so [[keyOf]] reads it unlocked; the apply's before
   *  stays readable beside it, since the projection's last written takes are that apply's until the projection writes. */
  @volatile private var keyed       = new java.util.IdentityHashMap[ResolverDecision, String]()
  @volatile private var keyedBefore = new java.util.IdentityHashMap[ResolverDecision, String]()
  /** Each live key's model decision, as the last apply was handed it: those the model no longer hands over are the
   *  clusters gone, whose keys the clusters overlapping them take ([[StableNames]]). */
  private val live    = mutable.HashMap.empty[Long, ResolverDecision]
  /** Each live key's first listing as `keyStore` holds it, and the next key the counter hands out. */
  private val firsts  = mutable.HashMap.empty[Long, String]
  private var nextKey = 0L

  /** The families in their order: the order their verdicts and searches are read in. */
  private[agreement] val familiesInOrder: Seq[(VoterFamily, FamilyAnswers)] = families.toSeq.sortBy(_._1.ordinal)
  /** Each family's calibration: TMDB's, its search priors' spread scaled by the family's [[VoterFamily.priorSpread]]. */
  private[agreement] val calibrations: Map[VoterFamily, IdentityCalibration] = families.keys.map(family => family -> calibration.withPriorSpread(family.priorSpread)).toMap
  /** The verdicts kept, by their cluster's key: each a cache of its receipt ([[Memo]]), standing while none of it moved —
   *  checked at a version under its listings' digest, and indexed by every TMDB record and search it read, what [[reads]]
   *  tells the filing thread. */
  private[agreement] lazy val held: Memo[String, StoredVerdict] = {
    val memo = new Memo[String, StoredVerdict](_.reads, _.readsTmdb, AgreementStage.pushed)
    stored.all().foreach(v => memo.put(v.id, v))
    memo
  }
  /** The held verdicts by their listings' digest, a model take's correction apart from a cluster's: what a verdict is found
   *  by, its key only naming it ([[heldFor]]). Kept beside [[held]] by every apply that moves it. */
  private[agreement] lazy val heldByDigest: (mutable.LongMap[StoredVerdict], mutable.LongMap[StoredVerdict]) = {
    val index = (mutable.LongMap.empty[StoredVerdict], mutable.LongMap.empty[StoredVerdict])
    held.values.foreach(indexed(index, _))
    index
  }

  /** Does a verdict the stage keeps read the TMDB record or search a filing under `id` comes as? Then that filing must
   *  reach the stage, as every answer it read does: the verdict is a cache of what it read. Read from any thread. */
  def reads(id: String): Boolean = held.reads(id) || (id match {
    case Dependency.QueryFiled => queriedNow
    case _ if id.startsWith(AgreementStage.FindPrefix) => findsNow.contains(id)
    case _ => AgreementStage.recordOf(id).exists(film => recordsNow.contains(film))
  })

  /** The receipt the decision being made notes what it reads in ([[Noting]]): set by [[receipting]] on the applying
   *  thread. */
  private[agreement] var receipt: mutable.Map[Dependency, Long] = null
  /** `decide`, everything it reads through the stage's lookups noted in `into`, its receipt. */
  private[agreement] def receipting[A](into: mutable.Map[Dependency, Long])(decide: => A): A = {
    val was = receipt
    receipt = into
    try decide finally receipt = was
  }
  /** What the current apply's takes read of TMDB — its records, whether a search, the IMDb ids found — and what the last
   *  apply's did ([[reads]]): what a take reads afresh on every apply must reach it when filed, as a verdict's receipt
   *  does. */
  private[agreement] var recordsReading = java.util.concurrent.ConcurrentHashMap.newKeySet[Integer]()
  private[agreement] var findsReading   = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
  private[agreement] var queriedReading = false
  @volatile private var recordsNow: java.util.Set[Integer] = java.util.Collections.emptySet()
  @volatile private var findsNow: java.util.Set[String]    = java.util.Collections.emptySet()
  @volatile private var queriedNow = false

  /** The venues' lookups: their pages, and nothing of TMDB's, whatever `venuePages` answers besides ([[AgreementStage.PagesOnly]]) —
   *  what the worker hands the stage there (VenueDetailLookups), so a harness handing one lookups for both roles reads
   *  as production reads. Handed TMDB's records through this role, every harness read the model's vote by its facts
   *  where production read it unknown, and none of them saw prod keep 26 takes a whole resolve made (2026-10-27). */
  private[agreement] val venues: IdentityLookups = AgreementStage.PagesOnly(venuePages)

  /** TMDB's and the venues' lookups as the stage reads them: each read noted ([[Noting]]). */
  private[agreement] lazy val looked: Option[IdentityLookups] = tmdb.map(new Noting(_, Dependency.Via.Tmdb))
  private[agreement] lazy val venuesLooked: IdentityLookups   = new Noting(venues, Dependency.Via.Venues)

  /** `inner`, each read noted: among what the current apply read ([[reads]]), and in the [[receipt]] being made, if any —
   *  as the question it reads, through `via`, with the digest of what it read, nothing there yet too
   *  ([[AgreementStage.Receipt.reread]] reads it again). */
  private[agreement] final class Noting(inner: IdentityLookups, via: Dependency.Via) extends services.identity.ForwardingIdentityLookups(inner) {
    private def noted[A](aspect: Dependency.Aspect, question: Question, answer: Answer[A]): Answer[A] = {
      if (receipt != null) receipt(Dependency.Lookup(via, aspect, question)) = Dependency.digest(answer)
      answer
    }
    private def record[A](aspect: Dependency.Aspect, tmdbId: Int, answer: Answer[A]): Answer[A] = { recordsReading.add(tmdbId); noted(aspect, Question.FilmRecord(tmdbId), answer) }
    override def detail(listing: Listing): Answer[Option[DetailFacts]]               = noted(Dependency.Aspect.Answer, Question.VenuePage(listing.key), inner.detail(listing))
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]]                 = { queriedReading = true; noted(Dependency.Aspect.Answer, Question.Candidate(query), inner.candidates(query)) }
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]            = record(Dependency.Aspect.Answer, tmdbId, inner.film(tmdbId))
    override def releaseDay(tmdbId: Int): Answer[Option[java.time.LocalDate]]        = record(Dependency.Aspect.Release, tmdbId, inner.releaseDay(tmdbId))
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]]                      = record(Dependency.Aspect.Cast, tmdbId, inner.cast(tmdbId))
    override def overview(tmdbId: Int): Answer[Option[services.identity.ProseWords]] = record(Dependency.Aspect.Overview, tmdbId, inner.overview(tmdbId))
  }
  /** The `version` the stored verdicts were loaded at: what this worker's families answered cannot have moved while it
   *  was down — it alone files them — so a verdict kept under these rules, its listings as they were, stands then without
   *  a single answer read again (prod PL 2026-10-05: re-reading every verdict's answers at boot was a 40 s, 514 MB pass). */
  private[agreement] var loadedAt: Option[Long] = None
  /** Whether the apply under way is this stage's first: the one that meets every cluster, and asks none inline. Kept apart
   *  from [[loadedAt]], a version of the families' answers, which moves only when one is filed: judged by it, every apply
   *  after a deploy was the first until a family answer was filed, and a take that moved meanwhile -- a record filed --
   *  had its correction's questions left on the queue, the model's film served until they came in (UK Chichester's "The
   *  Ties That Bind Us": 2008's film from its record's filing to a later queue round, drift slice seed 3). */
  private[agreement] var firstPass: Boolean = true
  /** Each model decision the stage took, with what it took it as: handed back as that same object while neither moved,
   *  so the projection, which diffs decisions by identity, redrafts an agreed cluster only when its verdict moved. */
  private[agreement] val takenAs = new java.util.IdentityHashMap[ResolverDecision, ResolverDecision]()
  /** Each model decision's cluster id, listings and their digest, kept while the decision is the same object: the model
   *  hands over a new decision when one of its listings or a fact it reads moves, so a quiet tick reads no listing again
   *  — and one re-published with a field only the stage reads is forgotten ([[apply]]'s `republished`). */
  private[agreement] val digested = new java.util.IdentityHashMap[ResolverDecision, AgreementStage.Digested]()
  /** Each model decision's verdict as an event no film database holds ([[NonFilmEvents]]), kept while the decision is
   *  the same object: such a cluster is asked nothing — neither the agreement's questions nor a model take's correction. */
  private[agreement] val events = new java.util.IdentityHashMap[ResolverDecision, Option[String]]()
  /** The clusters a family has not answered for yet, at the `version` and listings' digest they were last resolved at,
   *  with the questions they wait on: not resolved again until one of those is answered or their listings move. */
  private[agreement] val waiting = TrieMap.empty[String, AgreementStage.Waiting]
  /** The SHAPES (listings' digests) whose families were asked inline ([[askNow]]): each asked so once, whichever key
   *  names its cluster — a key only names (docs/design/identity-no-drift.md, "One dependency index"). Kept while a
   *  cluster of that shape is met. */
  private[agreement] val askedNow = mutable.HashSet.empty[Long]
  /** The questions the last [[apply]] waited on that were not attempted yet: one attempted since — a failed ask files
   *  nothing — is what reads the same decisions at the same version again ([[attemptedSince]]). */
  private[agreement] var awaiting = Set.empty[Question]
  /** `attemptsMarked` as it stood when [[awaiting]] was last found unattempted: while the memory has marked nothing
   *  since, none of them can have been attempted, and a quiet apply reads none of their records (3,459 on the e2e corpus,
   *  4.4 MB a tick). */
  private[agreement] var awaitingMarks: Option[Long] = None
  /** The questions handed to `ask` since the families' answers last moved (at `handedAt`): a tick that filed nothing
   *  hands nothing. */
  private[agreement] var handed   = Set.empty[Question]
  private[agreement] var handedAt = -1L
  /** Each cluster's TMDB candidates, none denied, at its listings' digest: what its posters are compared against — a cache
   *  of the TMDB answers the read took ([[Memo]]): kept while none of them was filed since. */
  private[agreement] val candidateFilms = new Memo[String, AgreementStage.CandidateFilms](_.reads, _ => false)
  /** The `version` the current [[apply]] runs at. */
  private[agreement] var applying = -1L
  /** The year the current [[apply]] reads a release as current by — an input every verdict's receipt names
   *  ([[Dependency.Year]]): a year turning reads every verdict again, as a whole resolve in it would. */
  private[agreement] var yearNow = -1
  /** What a correction still waiting on an answer asks, by its stored id, at the digest of the take's listings it was
   *  read at: a take's posters, families' questions and finds while the backfill hashes them — none kept once its
   *  evidence is all answered, nor once the take's listings move. */
  private[agreement] val correctionWaits = TrieMap.empty[String, AgreementStage.Waiting]

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
    if (year != yearNow) { yearNow = year; held.moved(_ == Dependency.Year, readNow); last = None }
    val reread = republished.fold { digested.clear(); true }(forgetRead(resolution, _))
    val waited = attemptedSince
    if (waited) uncheckAttempted()
    last.collect { case (was, at, came) if !reread && !waited && !unread && at == version && sameDecisions(was, resolution) =>
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
    attemptUnchecked = held.moved(AgreementStage.isAttempt, readNow)
  /** How many verdicts the last attempt marked read again ([[uncheckAttempted]]). */
  private[agreement] var attemptUnchecked = 0

  /** Was a question the last apply waited on attempted since, though nothing was filed — a cluster's wait settling, a
   *  take's evidence read as none? */
  private[agreement] def attemptedSince: Boolean = awaiting.nonEmpty && {
    val marks = attemptsMarked.map(_())
    (marks.isEmpty || marks != awaitingMarks) && {
      val since = awaiting.exists(tried)
      if (!since) awaitingMarks = marks   // marked since, but none of these: not read again until the next mark
      since
    }
  }

  /** Forget the listings read for each decision holding one of `keys`; whether one the stage reads — a cluster nothing
   *  took, a model take it corrects — holds any. */
  private[agreement] def forgetRead(resolution: Resolution, keys: Iterable[ListingKey]): Boolean = keys.nonEmpty && {
    val moved   = keys.toSet
    val touched = resolution.decisions.filter(_.members.exists(moved))
    touched.foreach(digested.remove)
    touched.exists(decision => decision.film.isEmpty || correctable(decision))
  }

  /** The same decisions, object for object: the model hands each unchanged decision over as the same object. */
  private[agreement] def sameDecisions(a: Resolution, b: Resolution): Boolean = {
    val (x, y) = (a.decisions.iterator, b.decisions.iterator)
    while (x.hasNext && y.hasNext) if (!(x.next() eq y.next())) return false
    !x.hasNext && !y.hasNext
  }

  private[agreement] def applied(resolution: Resolution, listingOf: ListingKey => Option[Listing], version: Long): Resolution = {
    val started = tools.Stopwatch.start()
    val pass    = new AgreementStage.Pass(version, correctionsPerApply, new AgreementStage.Read(posters, listedOn.flatMap(families.get)), backfillPerApply)
    held.newPass(); candidateFilms.newPass()
    keying(resolution, pass)
    firstPass = loadedAt.isEmpty
    if (loadedAt.isEmpty) {   // first pass: every verdict kept under these rules stands at this version
      loadedAt = Some(version)
      val year = readNow(Dependency.Year)
      held.values.filter(v => v.rules == decidedUnder && v.reads.get(Dependency.Year).forall(year.contains)).foreach { v =>
        held.check(v.id, v.listings, version)
      }
    }
    applying = version
    recordsReading = java.util.concurrent.ConcurrentHashMap.newKeySet[Integer](); findsReading = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    queriedReading = false
    validating(pass)
    val decisions = AgreementStage.inContentOrder(resolution.decisions)(decide(_, listingOf, pass))
    // what the stage took a decision as is keyed as the decision is: the projection's takes are these
    resolution.decisions.iterator.zip(decisions).foreach { case (model, took) =>
      if (!(took eq model)) { val key = pass.keys.get(model); if (key != null) pass.keys.put(took, key) }
    }
    keyedBefore = keyed
    keyed = pass.keys
    forgetGone(resolution, pass)
    recordsNow = recordsReading; findsNow = findsReading; queriedNow = queriedReading
    wantedNow = pass.wanted(correctionPostersAtOnce, tried)
    handOver(version)
    // read before the questions are: a mark landing while they are read moves it, and the next apply reads them again
    awaitingMarks = attemptsMarked.map(_())
    awaiting = wantedNow.filterNot(tried)
    unread = pass.spent
    familyVerdicts.applied()
    report(decisions, pass, started)
    resolution.copy(decisions = decisions)
  }

  /** Makes every `fullValidationEvery`th apply that reads anything, unless the heap is `pressed`, a FULL VALIDATION: up to
   *  [[AgreementStage.FullValidationBudget]] of the kept verdicts the dirty index calls standing ([[Memo.clean]]) are read
   *  again whole ([[Memo.verified]]) — the next budget's worth each time, round the set — and one whose receipt moved
   *  unannounced is DRIFT, counted and decided again. The proof the index equals full validation: `drifted` must read
   *  zero. A correction stands on its filings alone, and is not read. */
  private[agreement] def validating(pass: AgreementStage.Pass): Unit = {
    appliesSinceValidation += 1
    if (appliesSinceValidation >= fullValidationEvery && !pressed()) {
      appliesSinceValidation = 0
      if (validatedFrom >= held.size) validatedFrom = 0
      pass.validateFrom(validatedFrom, AgreementStage.FullValidationBudget)
      validatedFrom += AgreementStage.FullValidationBudget
    }
  }
  private var appliesSinceValidation = 0
  private var validatedFrom          = 0

  /** Keys each decision of `resolution` with listings into `pass.keys` ([[StableNames]]): one the last apply was handed
   *  keeps its key; the rest take the keys of the clusters gone since by overlap — on the first apply, the stored key of
   *  their first listing first ([[keyStore]]), every other fresh. The keys that moved are stored. A key only NAMES a cluster:
   *  nothing the stage decides is found by it ([[heldFor]]), and no memory goes with it (docs/design/identity-no-drift.md). */
  private[agreement] def keying(resolution: Resolution, pass: AgreementStage.Pass): Unit = {
    val before  = keyed
    // on the first apply, every key stored: re-attached by first listing, else retired — and the counter past them all
    val stored  = Option.when(loadedAt.isEmpty)(keyStore.all())
    stored.foreach(held => nextKey = nextKey max held.next)   // a retired key is never handed out again, keyed now or not
    val unkeyed = mutable.ArrayBuffer.empty[ResolverDecision]
    resolution.decisions.foreach { decision =>
      if (decision.members.nonEmpty) { val key = before.get(decision); if (key != null) pass.keys.put(decision, key) else unkeyed += decision }
    }
    val gone    = mutable.ArrayBuffer.empty[(Long, Seq[ListingKey])]
    live.foreachEntry((key, decision) => if (!pass.keys.containsKey(decision)) gone += key -> decision.members)
    if (unkeyed.nonEmpty || gone.nonEmpty || stored.exists(_.keys.nonEmpty)) {
      val storedFirsts = stored.fold[collection.Map[Long, String]](firsts)(held => held.keys.iterator.map(k => k.key -> k.first).toMap)
      val counter      = nextKey
      val firstOf  = unkeyed.map(decision => StoredFamily.idOf(decision.members))
      val attached = mutable.LongMap.empty[Int]
      stored.foreach { held =>
        val byFirst = held.keys.iterator.map(k => k.first -> k.key).toMap
        unkeyed.indices.foreach(i => byFirst.get(firstOf(i)).foreach(key => attached(key) = i))
      }
      val open     = unkeyed.indices.filterNot(attached.valuesIterator.toSet)
      val assigned = StableNames.assign(gone, open.map(unkeyed(_).members), counter)
      val keyOfIndex = Array.fill(unkeyed.size)(-1L)
      attached.foreachEntry((key, i) => keyOfIndex(i) = key)
      open.indices.foreach(j => keyOfIndex(open(j)) = assigned.names(j))
      val retired = (gone.iterator.map(_._1) ++ stored.iterator.flatMap(_.keys.map(_.key))).toSet -- keyOfIndex
      val moved   = unkeyed.indices.collect { case i if !storedFirsts.get(keyOfIndex(i)).contains(firstOf(i)) => StoredKey(keyOfIndex(i), firstOf(i)) }
      // stored first: a write that fails leaves the stage as it was, and the next apply keys the same decisions again
      if (retired.nonEmpty || moved.nonEmpty || assigned.nextFresh != counter) keyStore.replace(retired, moved, assigned.nextFresh)
      nextKey = assigned.nextFresh
      stored.foreach { _ => firsts.clear(); firsts ++= storedFirsts }
      firsts --= retired
      moved.foreach(k => firsts(k.key) = k.first)
      live --= retired
      unkeyed.indices.foreach { i =>
        live(keyOfIndex(i)) = unkeyed(i)
        pass.keys.put(unkeyed(i), keyOfIndex(i).toString)
      }
    }
  }

  /** The verdict held for the cluster `key` names with listings digesting to `digest` — under its key, else under
   *  whichever key it was filed by (a restart's keys lost or numbered otherwise, a pre-key verdict under its first
   *  listing): filed under `key` from now, the move stored with the apply's. A verdict stands on its listings and receipt,
   *  never on the key it was filed by. */
  private[agreement] def heldFor(key: String, digest: Long, pass: AgreementStage.Pass): Option[StoredVerdict] =
    held.get(key).filter(_.listings == digest).orElse {
      val index = if (key.startsWith(AgreementStage.CorrectionPrefix)) heldByDigest._2 else heldByDigest._1
      index.get(digest).filter(_.listings == digest).map { found =>
        val under = found.copy(id = key)
        file(under); pass.moved += under
        // checked as it was under the key it was filed by (a boot's every standing verdict), its waits not carried
        held.moveCheck(found.id, key, digest)
        correctionWaits -= key
        under
      }
    }

  /** `verdict` in `index`, by its listings' digest. */
  private[agreement] def indexed(index: (mutable.LongMap[StoredVerdict], mutable.LongMap[StoredVerdict]), verdict: StoredVerdict): Unit =
    (if (verdict.correction.isDefined) index._2 else index._1)(verdict.listings) = verdict

  /** The clusters the stage waits on now: on their families' questions or fill ([[waiting]]), or on their take's
   *  correction ([[correctionWaits]]), by cluster id. */
  private[agreement] def waitingClusters: Set[String] =
    waiting.keySet.toSet ++ correctionWaits.keySet.iterator.map(_.stripPrefix(AgreementStage.CorrectionPrefix))

  /** What the model's `decision` is taken as: a model take read against the evidence that may correct it, a no-match
   *  the families, the posters, the screening days, a fill or the catalogue ids take a film for — else the decision as
   *  the model left it. What it reads and waits on is noted in `pass`. */
  private[agreement] def decide(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], pass: AgreementStage.Pass): ResolverDecision =
    if (correctable(decision) && eventOf(decision, listingOf).isEmpty) {
      val key = AgreementStage.correctionId(pass.keys.get(decision))
      pass.correcting += key
      corrected(decision, listingOf, key, pass)
    }
    else if (decision.film.isDefined || decision.unanswered > 0 || decision.fallback.isDefined || decision.members.isEmpty) decision
    else {
      val now = eventOf(decision, listingOf) match {
        // an event no film database holds: no family is asked about it, no poster hashed, no verdict kept
        case Some(why) => taking(decision)(ResolverDecision.Basis.Event, s"event, not a film: $why")
        case None =>
          val AgreementStage.Digested(id, listings, digest) = digestedOf(decision, pass.keys.get(decision), listingOf)
          pass.seen += id
          pass.shapes += digest
          val verdict = verdictOf(decision, id, listings, digest, pass)
          // the venue posters' distances to the cluster's candidates and to the film the families agree on
          def distances(also: Option[Int]) = posterDistances(id, digest, listings, also, pass.asked)
          // the posters vote once the families reached a verdict that takes no film: none agreed, or a poster vetoed it
          verdict.fold(decision) { v =>
            lazy val unagreed = AgreementStage.Unagreed(decision, id, digest, listings, v, distances)
            pieceOf(v, listings).fold(
            v.agreed.fold[AgreementStage.Take](AgreementStage.Take.Untaken)(taken(decision, _, pass.asked, distances,
              film => synopsisAgainst(id, digest, listings, film))) match {
              // a screen adaptation agreed for a listing naming a stage work ([[Agreement.agreed]]) yields to the relay its
              // screening days name: PL Kino Amok's bare "Manon" on the Met's broadcast day is the Met's, not Clouzot's
              case AgreementStage.Take.Taken(agreed) if listings.exists(ListingShape.stagesAWork) =>
                takeOf(AgreementStage.BroadcastTake, unagreed, pass).getOrElse(agreed)
              case AgreementStage.Take.Taken(agreed) => agreed
              case AgreementStage.Take.Pending       => decision
              case AgreementStage.Take.Vetoed        => pass.posterVetoed += 1; fellThrough(unagreed, pass)
              // the venue's synopsis naming a namesake's description says why, where nothing else takes one
              case AgreementStage.Take.Refused(why)  =>
                val fell = fellThrough(unagreed, pass)
                if (fell eq decision) taking(decision)(decision.basis, why) else fell
              // the guard that refused the film the families would agree on says why, where nothing else takes one
              case AgreementStage.Take.Untaken       =>
                val fell = fellThrough(unagreed, pass)
                v.refused.filter(_ => fell eq decision).fold(fell)(why => taking(decision)(decision.basis, why))
            })(line => taking(decision)(decision.basis, line))
          }
      }
      if (now eq decision) decision else takenAsBefore(decision, now)
    }

  /** A cluster the stage meets in this SHAPE (its listings digesting to `digest`) for the first time since it started —
   *  no verdict, wait or inline ask of that shape held, under ANY key: found by the digest, as a verdict is ([[heldFor]]),
   *  never by the key naming the cluster now — past its first pass, which meets every cluster:
   *  what [[askNow]] asks inline. A new key and a known key a scrape reshaped alike: under its kept key, PL "Bez końca" ×7
   *  (2026-10-23, 10-26) gained a listing whose title no family had been asked, waited on the queue, and was published
   *  lost until the answers came in half a minute later — where a cluster new to its key would have read them. */
  private[agreement] def newCluster(digest: Long): Boolean =
    asksInline(digest, heldByDigest._1) && !waiting.valuesIterator.exists(_.listings == digest)

  /** A model take's correction the stage meets in this SHAPE (`digest`, [[correctionDigest]]) for the first time since it
   *  started — none held of that shape under any key, none asked inline — past its first pass: what [[askNow]] asks the
   *  families of inline, as [[newCluster]] does a cluster's. */
  private[agreement] def newCorrection(digest: Long): Boolean = asksInline(digest, heldByDigest._2)

  /** What [[newCluster]] and [[newCorrection]] share: an inline ask is wired, the first pass is over, and no verdict of
   *  `index` (the clusters' or the corrections') nor an inline ask has met the SHAPE `digest` yet. */
  private def asksInline(digest: Long, index: mutable.LongMap[StoredVerdict]): Boolean =
    (askNow ne AgreementStage.AskNow.Never) && !firstPass && !index.get(digest).exists(_.listings == digest) && !askedNow(digest)

  /** Forget what the clusters `resolution` no longer holds were kept with — their verdicts, waits, candidates,
   *  corrections and the model's decisions — and store the verdicts `pass` moved. */
  private[agreement] def forgetGone(resolution: Resolution, pass: AgreementStage.Pass): Unit = {
    // a verdict under other rules decided again to a wait, none under these rules in its place: a cache of nothing now
    val renewed    = pass.moved.iterator.filter(_.rules == decidedUnder).map(_.id).toSet
    val superseded = pass.backfilled.toSet -- renewed
    val removed    = held.keys.toSet -- pass.seen -- pass.correcting ++ superseded
    val moved      = pass.moved.filterNot(verdict => superseded(verdict.id))
    waiting --= waiting.keySet.toSet -- pass.seen
    askedNow.filterInPlace(pass.shapes)
    (candidateFilms.keys.toSet -- pass.seen).foreach(candidateFilms.remove)   // a correction's candidates are read, never kept
    correctionWaits --= correctionWaits.keySet.toSet -- pass.correcting
    if (removed.nonEmpty || moved.nonEmpty) {
      stored.replace(removed, moved.toSeq)
      removed.foreach(unfile)
      moved.foreach(file)
    }
    val current = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[ResolverDecision, java.lang.Boolean]())
    resolution.decisions.foreach(current.add)
    takenAs.keySet.retainAll(current)   // the model's decisions this resolution no longer holds
    digested.keySet.retainAll(current)
    events.keySet.retainAll(current)
  }

  /** `verdict` held under its id — found by its digest ([[heldByDigest]]) — the verdict it replaces dropped. */
  private[agreement] def file(verdict: StoredVerdict): Unit = {
    val index = heldByDigest   // built from what is held before it moves
    held.get(verdict.id).foreach(dropped)
    held.put(verdict.id, verdict); indexed(index, verdict)
  }

  /** The verdict held under `id`, if any, dropped. */
  private[agreement] def unfile(id: String): Unit = {
    val _ = heldByDigest
    held.remove(id).foreach(dropped)
  }

  private def dropped(verdict: StoredVerdict): Unit = {
    val index = if (verdict.correction.isDefined) heldByDigest._2 else heldByDigest._1
    if (index.get(verdict.listings).exists(_ eq verdict)) index -= verdict.listings
  }

  /** Hands `ask` what the last apply met unanswered and was not handed at `version` yet. */
  private[agreement] def handOver(version: Long): Unit = {
    if (handedAt != version) { handed = Set.empty; handedAt = version }
    val open = wantedNow -- handed
    if (open.nonEmpty) { ask(open); handed = handed ++ open }
  }

  /** What the apply came to, for [[metrics]]. */
  private[agreement] def report(decisions: Seq[ResolverDecision], pass: AgreementStage.Pass, started: tools.Stopwatch.Started): Unit = {
    val agreedNow = decisions.filter(_.basis == ResolverDecision.Basis.Agreed)
    val open      = mutable.Map.empty[VoterFamily, Int]
    var finds, postersOpen, catalogueOpen, undated = 0
    wantedNow.foreach {
      case Question.Family(family, _) => open(family) = open.getOrElse(family, 0) + 1
      case Question.Find(_)           => finds += 1
      case Question.Poster(_)         => postersOpen += 1
      case Question.Catalogue(_)      => catalogueOpen += 1
      case Question.Record(_)         => undated += 1
      case _                          => ()
    }
    metrics.applied(AgreementStage.Applied(waiting = waiting.size, verdicts = held.values.count(_.correction.isEmpty),
      agreed = held.values.count(_.agreed.isDefined),
      takenTmdb = agreedNow.count(_.film.isDefined), takenFallback = agreedNow.count(_.fallback.isDefined),
      open = open.toMap, finds = finds, resolves = pass.resolves, seconds = started.seconds,
      takenPoster = decisions.count(_.basis == ResolverDecision.Basis.Poster), posterVetoed = pass.posterVetoed, posters = postersOpen,
      takenBroadcast = decisions.count(_.basis == ResolverDecision.Basis.Broadcast), takenFilled = decisions.count(_.basis == ResolverDecision.Basis.Filled),
      takenCatalogue = decisions.count(_.basis == ResolverDecision.Basis.Catalogue), catalogue = catalogueOpen, undated = undated,
      withdrawn = decisions.count(_.basis == ResolverDecision.Basis.Withdrawn), corrected = decisions.count(_.basis == ResolverDecision.Basis.Corrected),
      correcting = pass.correcting.size, correctionPosters = pass.correctionPosters.size,
      events = decisions.count(_.basis == ResolverDecision.Basis.Event),
      incomplete = held.values.count(_.rules != decidedUnder), validated = pass.validated.read, drifted = pass.validated.drifted))
  }

  /** `d` taken on `basis`, `line` explaining it — with the `film`, `fallback` and `agreed` ids it takes, where they move. */
  private[agreement] def taking(d: ResolverDecision)(basis: ResolverDecision.Basis, line: String, film: Option[Int] = d.film,
                                          fallback: Option[ResolverDecision.Fallback] = d.fallback, agreed: Map[String, String] = d.agreed,
                                          rule: Option[String] = None): ResolverDecision =
    d.copy(film = film, basis = basis, explanation = d.explanation :+ line, fallback = fallback, agreed = agreed)(
      rule.fold(d.trace)(r => d.trace.copy(stage = Some(r))))

  /** The takes a cluster the families' agreement took no film for is read by, by name — in [[AgreementStage.FallThrough]]'s
   *  order, the first to take one deciding: each reads its own evidence and notes in `asks` (the apply's [[AgreementStage.Pass]])
   *  every question it waits on. */
  private[agreement] val takes: Map[String, (AgreementStage.Unagreed, AgreementStage.Pass) => Option[ResolverDecision]] = Map(
    AgreementStage.PosterVoteTake -> ((u, _) => voted(u.decision, u.distances(None))),
    AgreementStage.BroadcastTake  -> ((u, asks) => broadcast(u.decision, u.id, u.digest, u.listings, asks.version, asks.asked)),
    AgreementStage.FillTake       -> ((u, asks) => filledTake(u.decision, u.verdict, asks.asked, u.distances,
      film => synopsisAgainst(u.id, u.digest, u.listings, film))),
    AgreementStage.CatalogueTake  -> ((u, asks) => catalogued(u.decision, u.listings, asks.asked)))
  private[agreement] def takeOf(rule: String, untaken: AgreementStage.Unagreed, asks: AgreementStage.Pass): Option[ResolverDecision] = takes(rule)(untaken, asks)
  private[agreement] val fallThrough = AgreementStage.FallThrough.map(takes)
  /** The first of [[AgreementStage.FallThrough]] to take a film for the cluster, else the cluster as the model left it. */
  private[agreement] def fellThrough(untaken: AgreementStage.Unagreed, asks: AgreementStage.Pass): ResolverDecision =
    fallThrough.iterator.flatMap(_(untaken, asks)).nextOption().getOrElse(untaken.decision)

  /** Why the decision's cluster is an event no film database holds, or `None` — read once per decision object. */
  private[agreement] def eventOf(decision: ResolverDecision, listingOf: ListingKey => Option[Listing]): Option[String] =
    Option(events.get(decision)).getOrElse {
      val event = NonFilmEvents.of(decision.members.flatMap(listingOf))
      events.put(decision, event); event
    }

  /** The decision's listings, sorted by key. */
  private[agreement] def sortedListings(decision: ResolverDecision, listingOf: ListingKey => Option[Listing]): Seq[Listing] =
    decision.members.flatMap(listingOf).sortBy(_.key)(using ListingKey.ordering)

  /** The decision's cluster key (`key`), its listings sorted and their digest, kept while the model hands the same
   *  decision over — which keeps its key as long. */
  private[agreement] def digestedOf(decision: ResolverDecision, key: String, listingOf: ListingKey => Option[Listing]): AgreementStage.Digested =
    Option(digested.get(decision)).getOrElse {
      val listings = sortedListings(decision, listingOf)
      val fresh    = AgreementStage.Digested(key, listings,
        AgreementStage.digest(Seq(digestOf(listings).toString, decision.leaning.toString, decision.candidate.toString,
          PosterEvidence.urls(listings).mkString("\u0001"))))
      digested.put(decision, fresh); fresh
    }

  /** TMDB's own vote on a no-match: the film it leans to, else the best-ranked candidate it weighed — its record as
   *  `lookups` reads it (the verdict's receipt), so it links to a family's pick by facts as well as by ids. */
  private[agreement] def voteOf(decision: ResolverDecision, lookups: IdentityLookups): Option[SourceRecord] =
    decision.leaning.orElse(decision.candidate).map { vote =>
      val ids = AgreementStage.leanRecord(vote)
      lookups.film(vote.film).toOption.flatten.fold(ids)(film => SourceRecord(film,
        ids.crossIds ++ IdentityMeasures.imdbId(film.imdbNumber).map("imdb" -> _)))
    }

  /** The listings as published, the people their own text names, and the venue detail page the picks read of each. */
  private[agreement] def digestOf(listings: Seq[Listing]): Long =
    AgreementStage.digest(listings.map(l => l.sortKey + l.screenedSeason.fold("")(season => s"\u0002$season") +
      (if (l.names.isEmpty) "" else s"\u0003${l.names.##}") +
      (if (venues.hasDetail(l)) "\u0001" + venues.detail(l) else "")))

  /** The cluster's verdict: the stored one while its listings and everything it read stand (its receipt,
   *  [[StoredVerdict.reads]]), else the resolver's over the answers now — `None` while a family's is a gap. */
  private[agreement] def verdictOf(decision: ResolverDecision, id: String, listings: Seq[Listing], digest: Long,
                        pass: AgreementStage.Pass): Option[StoredVerdict] = {
    val version = pass.version
    // a wait is decided for one shape of the cluster: one kept for another is no wait of this one, whatever decides it now
    if (waiting.get(id).exists(_.listings != digest)) waiting -= id
    val found = heldFor(id, digest, pass)
    // settled without some families: their questions asked on every pass — one answered reads the verdict again
    found.foreach(v => pass.asked ++= unreached(v))
    found.filter(_.rules == decidedUnder).filter { kept =>
      // a filing of what it read is compared by value, that answer alone read again ([[Memo.clean]]'s `again`): one that
      // moved nothing leaves it standing — every TMDB search is filed under one id (QueryFiled), so any search filed
      // anywhere read every verdict that read one whole (DE 2026-10-27: ~4 s, 1.5 GB an apply)
      var reread = false
      val clean  = held.clean(id, digest, version, changes.changedSince, again = { (read, recorded) =>
        reread = true
        digestNow(read, listingIn(listings)).contains(recorded)
      })
      // read again because an answer was filed: a stale one among its reads is asked again too — a quiet tick reads none
      if (clean && reread) pass.asked ++= kept.reads.keys.flatMap(staleOf)
      if (clean && pass.validating()) {
        // the full validation's: read again whole, though the index calls it standing — a drift is decided again
        val checked = held.verified(id, kept, digestNow(_, listingIn(listings)))
        pass.validated += checked
        checked.drifted == 0
      } else clean || {
        val stands = held.stands(kept, digestNow(_, listingIn(listings)))
        // read again because an answer was filed: a stale one among its reads is asked again too — a quiet tick reads none
        if (stands) { held.check(id, digest, version); pass.asked ++= kept.reads.keys.flatMap(staleOf) }
        stands
      }
    }
    // decided under other rules (a deploy's, or before receipts were whole): decided again while the apply's backfill
    // budget lasts, standing as it was until a later apply's reaches it — a deploy's boot resolves no whole corpus. One
    // under these rules whose answers moved spends none of it: decided again now, never standing on a stale receipt.
    // One decided again to a wait is dropped with the apply ([[forgetGone]]): it neither stands again nor spends a
    // later apply's budget again while its cluster waits
    .orElse(found.filter(_.rules != decidedUnder).filter { _ =>
      val again = pass.backfill()
      if (again) pass.backfilled += id
      !again
    })
    .orElse(waiting.get(id).filter(w => w.listings == digest && !due(w)) match {
      // its questions not all answered yet: a resolve now could reach no verdict — or, its fill read blind, the verdict
      // that read gave, at answers nothing has been filed over since: a resolve now would read the very same
      case Some(w) => pass.asked ++= w.gaps; w.read
      case None    => resolved(decision, id, digest, listings, pass)
    })
  }

  /** The digest of what `read` reads now ([[reread]]), of the listings `listingOf` names: the one digest a receipt
   *  records it by ([[Dependency.digest]]), so no kind is compared in another form than it was recorded in. */
  private[agreement] def digestNow(read: Dependency, listingOf: ListingKey => Option[Listing]): Option[Long] =
    reread(read, listingOf).map(Dependency.digest)
  /** What `read` — one no listing names: the year, an attempt — digests to now. A receipt records it by this, the reader it
   *  is compared through ([[digestNow]]): one form both ways. */
  private[agreement] def readNow(read: Dependency): Option[Long] = digestNow(read, _ => None)

  /** One of `listings`, by its key: a receipt reads the detail page of its cluster's own listings alone. */
  private[agreement] def listingIn(listings: Seq[Listing]): ListingKey => Option[Listing] = key => listings.find(_.key == key)

  /** Every family's verdict on `listings`, in the families' order — each question read noted in `reads`, each gap in
   *  `gaps`. */
  private[agreement] def verdictsOf(listings: Seq[Listing], gaps: mutable.Set[Question], reads: mutable.Map[Dependency, Long]): Seq[Answer[FamilyVerdict]] =
    familiesInOrder.map { case (_, answers) => familyVerdicts.verdict(listings, answers, gaps, reads) }

  /** The families' verdicts the last applies resolved, each handed back while all it read reads alike ([[FamilyVerdicts]]). */
  private[agreement] val familyVerdicts = new FamilyVerdicts(venues, normalizer, calibrations)

  /** The cluster's verdict, the resolver asked again over every family's answers now — `None`, and the cluster kept
   *  waiting on its questions, while one is a gap. */
  private[agreement] def resolved(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing],
                       pass: AgreementStage.Pass): Option[StoredVerdict] = {
    val version = pass.version
    pass.resolves += 1
    // the verdict's receipt: everything its decision reads, each by its id with the digest of what it read — nothing
    // there yet too — so a filing of any of it reads the verdict again ([[Noting]])
    val reads = mutable.Map.empty[Dependency, Long]
    val gaps  = mutable.Set.empty[Question]
    val verdicts = {
      val first = verdictsOf(listings, gaps, reads)
      if (!first.contains(Answer.Unknown) || !newCluster(digest)) first
      else {
        // a cluster with a newly scraped listing: its families asked now, inline, so its first take reads their answers —
        // and the questions those answers lead to (a search's hits' records), a round each, asking only what no round
        // before did, all within the one bound. Else such a question waited on the queue, and the take came at a later
        // apply none of its own inputs moved at, another cluster's ask having filed it (UK Rio's "Rocky Horror",
        // convergence #2473: the record of a hit Metacritic's search found)
        askedNow += digest
        askedInline(first, gaps.toSet) { () => gaps.clear(); reads.clear(); verdictsOf(listings, gaps, reads) } { (now, asked) =>
          if (now.contains(Answer.Unknown)) gaps.toSet -- asked else Set.empty
        }
      }
    }
    pass.asked ++= gaps
    val unknown = verdicts.contains(Answer.Unknown)
    // settled once every question it waits on was attempted: the question memory's to say, never the cluster's past
    val settling = unknown && gaps.forall(tried)
    if (unknown && !settling) { waiting(id) = AgreementStage.Waiting(digest, gaps.toSet); None }
    else {
      waiting -= id
      Some {
        val thisYear = yearNow
        readNow(Dependency.Year).foreach(reads(Dependency.Year) = _)
        val known    = verdicts.flatMap(_.toOption)
        val stated   = listings.map(asStated)
        val (weighed, fill) = receipting(reads) {
          // the vote's record is TMDB's: the venues' lookups answer venue pages alone, every film unknown (the worker's
          // VenueDetailLookups), so read there the vote linked to no family's pick by its facts in production, while a
          // whole resolve handed one lookups for both linked it -- 26 takes prod kept as none on 2026-10-27 (DE
          // "Rental Family" ×9, "Dolly" ×9, PL "Ice Cream Man" ×4, …): each verdict read its vote's record as unknown
          val lean    = voteOf(decision, looked.getOrElse(venuesLooked))
          val weighed = Agreement.weighed(listings, known, lean, Some(thisYear), stated)
          // an agreement only review sites make is taken as nothing ([[taken]]): the fill reads the cluster as one none agreed on
          weighed -> (if (weighed.agreed.exists(_.families.exists(_.namesFilms))) Answer.Known(None)
            else filledOf(decision, listings, known, stated, thisYear, version, pass.asked, reads))
        }
        val agreed   = weighed.agreed
        // settled: what none answered is in the receipt as unanswered ([[Recording]]), so its answer, once filed, reads
        // the verdict again
        val verdict  = StoredVerdict(id, digest, reads.toMap, agreed, decidedUnder, fill.toOption.flatten, refused = weighed.refused)
        if (fill == Answer.Unknown) {
          // the fill met a TMDB question not answered yet: this pass reads the verdict, none keeps it — kept, it stood once
          // TMDB had answered (its reads are the families' only), and the fill was never read again (PL "Lalka (Dolly)" ×5).
          // The wait holds what the pass read, handed back while nothing is filed: dropped, the next decision of the same
          // cluster at the same answers read no verdict and took none — a re-resolve of the family elsewhere flipped the
          // take, the next filing flipped it back (UK Cineworld's "RBO Cinema Season 2026-27: La Fanciulla Del West" ×73)
          waiting(id) = AgreementStage.Waiting(digest, Set.empty, blindAt = Some(version), read = Some(verdict))
          held.uncheck(id)
        } else {
          if (!held.get(id).contains(verdict)) pass.moved += verdict
          held.check(id, digest, version)
        }
        verdict
      }
    }
  }

  /** The INLINE ASK ([[askNow]]) of a cluster or a take's correction met in a new shape: `first`, read, asks `firstAsking`
   *  now, reads again, and asks what that read leads to (`asking`, given what was asked) — a round each, asking only what
   *  no round before did, all rounds within [[AgreementStage.AskNowWithin]] and at most [[AgreementStage.InlineRounds]] of
   *  them. What the last read came to; a question still open there is handed to the queue as every other is. */
  private[agreement] def askedInline[A](first: A, firstAsking: Set[Question])(read: () => A)(asking: (A, Set[Question]) => Set[Question]): A = {
    val deadline = clock.millis() + AgreementStage.AskNowWithin.toMillis
    var asked    = Set.empty[Question]
    var now      = first
    var next     = firstAsking
    var rounds   = 0
    while (next.nonEmpty && rounds < AgreementStage.InlineRounds && clock.millis() < deadline) {
      asked ++= next
      rounds += 1
      askNow(next, scala.concurrent.duration.Duration(deadline - clock.millis(), java.util.concurrent.TimeUnit.MILLISECONDS))
      now  = read()
      next = asking(now, asked)
    }
    now
  }

  /** Has the family answered `question` since — any answer, fresh? Read per waiting cluster, per tick: an answer filed
   *  for another cluster's question resolves none of the rest again (prod PL 2026-10-04: re-resolving every waiting
   *  cluster on each filing ran a 24 s, 540 MB agreement phase back to back). */
  private[agreement] def answered(gap: Question): Boolean = gap match {
    case Question.Family(family, question) => families.get(family).exists(_.fresh(question))
    case _                                 => false
  }

  /** Is a waiting cluster due a resolve? Once every question it waits on is answered — any gap left, and the families
   *  could reach no verdict, so resolving on each answer as a backlog drains (every cluster gets one, every tick) only
   *  re-ran the same partial resolve: prod PL 2026-10-04, 25–48 s an agreement phase after the per-question fix — or once
   *  it settles ([[settles]]): then it is decided on the families that answered. */
  private[agreement] def due(waiting: AgreementStage.Waiting): Boolean =
    // a fill read blind waits on no family's question but on TMDB's answer: due once something is filed, not every pass
    waiting.blindAt.fold(waiting.gaps.forall(answered))(_ != applying) || settles(waiting)

  /** The one settle rule (docs/design/identity-questions.md, step 4): a cluster every one of whose questions has been
   *  attempted ([[tried]]) is decided on the families that answered, the rest left out as no verdict — no clock, and a
   *  question merely handed to `ask` holds it. Settling ends the wait, not the asking: its unanswered questions stay in
   *  [[unreached]], asked on every pass, and the first of them answered reads the cluster again ([[verdictOf]]). */
  private[agreement] def settles(waiting: AgreementStage.Waiting): Boolean =
    waiting.blindAt.isEmpty && waiting.gaps.forall(tried)

  /** Has `gap` been attempted: an answer to it held — a stale one too, read all the same — or, by `attempted`, asked and
   *  come to a durable non-answer or a failure? */
  private[agreement] def tried(gap: Question): Boolean = gap match {
    case family: Question.Family => answered(gap) || reread(Dependency.Answer(family), _ => None).exists(_ != Answer.Unknown) || attempted(gap)
    case _                                 => attempted(gap)
  }

  /** `answer` to `question`, or — not answered though attempted (it failed, or came to nothing durable) — `absent`: no
   *  evidence, the take decided without it (the settle rule, docs/design/identity-questions.md step 4). The caller notes
   *  the question asked all the same, so it is asked on every pass and read again once answered. */
  private[agreement] def settled[A](question: Question, answer: Answer[A], absent: => A): Answer[A] =
    if (answer == Answer.Unknown && attempted(question)) Answer.Known(absent) else answer

  /** The questions a settled verdict's families never answered ([[settles]]): its receipt's unanswered entries
   *  ([[Dependency.Unanswered]]) — read off the verdict found by its listings, never kept by a key, so a restart's
   *  keys lost or renumbered ask each cluster's own. */
  private[agreement] def unreached(verdict: StoredVerdict): Iterator[Question] =
    verdict.reads.iterator.collect { case (read, Dependency.Unanswered) => read }.flatMap(staleOf)

  /** A family's answer a receipt holds that is stale, as the question to ask again. */
  private[agreement] def staleOf(read: Dependency): Option[Question] = read match {
    case Dependency.Answer(question @ Question.Family(family, asked)) if families.get(family).exists(!_.fresh(asked)) => Some(question)
    case _                                                                                                           => None
  }

  /** What a receipt's dependency reads now — the one reader a receipt is compared through: a family's answer, as
   *  [[Recording]] notes it; TMDB's and the venues' lookups', as [[Noting]] notes them, a detail page of a listing
   *  `listingOf` names; whether a question was attempted; the year the apply reads by. None for one nothing reads again —
   *  what holds it never stands. */
  private[agreement] def reread(read: Dependency, listingOf: ListingKey => Option[Listing]): Option[Answer[?]] = read match {
    case Dependency.Attempted(question)     => Some(Dependency.attemptRead(attempted(question)))
    case Dependency.Year                    => Some(Dependency.yearRead(yearNow))
    case Dependency.Lookup(Dependency.Via.Venues, aspect, question) => AgreementStage.Receipt.reread(venues, aspect, question, listingOf)
    case Dependency.Lookup(Dependency.Via.Tmdb, aspect, question)   => tmdb.flatMap(AgreementStage.Receipt.reread(_, aspect, question, listingOf))
    case Dependency.Answer(Question.Family(family, question)) =>
      families.get(family).flatMap { answers =>
        question.split("\\|", 2) match {
          case Array("title", text)    => Some(answers.titled(text))
          case Array("director", text) => Some(answers.directedBy(text))
          case Array("record", text)   => Some(answers.record(text))
          case Array("showing", text)  => Some(answers.showing(text))
          case _                       => None
        }
      }
    case Dependency.Asked(_) | Dependency.Unparsed(_) => None
  }

  /** The TMDB film a record names: its TMDB id, else the film TMDB finds by its IMDb id — `Unknown` while that find is
   *  not answered yet, the IMDb id then noted in `finding`. */
  private[agreement] def tmdbFilmOf(tmdb: Option[Int], imdb: Option[String], finding: mutable.Set[Question]): Answer[Option[Int]] =
    tmdb.fold(imdb.fold[Answer[Option[Int]]](Answer.Known(None)) { id =>
      findsReading.add(AgreementStage.FindPrefix + id)
      Agreement.tmdbFound(tmdbOf, id, finding, attempted)
    })(film => Answer.Known(Some(film)))
}

object AgreementStage {
  /** `inner`'s venue pages alone: TMDB's searches and records unknown, a film's cast and description none to go on —
   *  as the worker's venue lookups answer them (VenueDetailLookups). TMDB is the stage's `tmdb`. */
  final class PagesOnly private (val inner: IdentityLookups) extends IdentityLookups {
    def hasDetail(listing: Listing): Boolean                       = inner.hasDetail(listing)
    def detail(listing: Listing): Answer[Option[DetailFacts]]      = inner.detail(listing)
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit =
      inner.prefetch(Nil, Nil, details)
    override def prefetchAnswered(): Unit                          = inner.prefetchAnswered()
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]        = Answer.Unknown
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]]   = Answer.Unknown
  }
  object PagesOnly {
    def apply(lookups: IdentityLookups): IdentityLookups = lookups match {
      case pages: PagesOnly => pages
      case other            => new PagesOnly(other)
    }
  }

  /** A family's answers that note every question still a gap, and the digest of every answer read. */
  private[agreement] final class Recording(answers: FamilyAnswers, asked: mutable.Set[Question], reads: mutable.Map[Dependency, Long]) extends FamilyAnswers {
    val family: VoterFamily = answers.family
    private def noted[A](question: String, answer: Answer[A]): Answer[A] = {
      AgreementStage.noted(answers, question, Dependency.digest(answer), answer == Answer.Unknown, asked, reads)
      answer
    }
    def titled(text: String): Answer[Seq[SourceHit]]     = noted(s"title|$text", answers.titled(text))
    def directedBy(name: String): Answer[Seq[SourceHit]] = noted(s"director|$name", answers.directedBy(name))
    def record(id: String): Answer[Option[SourceRecord]] = noted(s"record|$id", answers.record(id))
    override def showing(venue: String): Answer[Seq[Showing]] = noted(s"showing|$venue", answers.showing(venue))
  }

  /** A fallback film standing on `source`'s `id`: one standing on another database's id than IMDb's carries its `title`
   *  and `year`, that a link to its page is built of. */
  private[agreement] def fallbackOn(source: String, id: String, title: Option[String], year: Option[Int]): ResolverDecision.Fallback = {
    val named = source != VoterFamily.Imdb.database
    ResolverDecision.Fallback(source, id, 1.0, title.filter(_ => named), year.filter(_ => named))
  }

  /** A family's answer to `question` read, as its digest and whether it was a gap: in the receipt `reads` — answered or
   *  not, so an answer filed where none was reads the verdict again as one that moved does — and among the questions
   *  `asked` when a gap, or read and stale (asked again). */
  private[agreement] def noted(answers: FamilyAnswers, question: String, digest: Long, unknown: Boolean, asked: mutable.Set[Question],
                               reads: mutable.Map[Dependency, Long]): Unit = {
    reads(Dependency.Answer(Question.Family(answers.family, question))) = digest
    if (unknown || !answers.fresh(question)) asked += Question.Family(answers.family, question)
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
  object Receipt {
    /** The answer a lookup's dependency names (as [[AgreementStage.Noting]] notes it), read from `lookups` now: a detail
     *  page of a listing `listingOf` names (one it does not reads as moved). */
    def reread(lookups: IdentityLookups, aspect: Dependency.Aspect, question: Question, listingOf: ListingKey => Option[Listing]): Option[Answer[?]] =
      (question, aspect) match {
        case (Question.VenuePage(key), Dependency.Aspect.Answer)  => listingOf(key).map(lookups.detail)
        case (Question.Candidate(query), Dependency.Aspect.Answer) => Some(lookups.candidates(query))
        case (Question.FilmRecord(film), Dependency.Aspect.Answer)   => Some(lookups.film(film))
        case (Question.FilmRecord(film), Dependency.Aspect.Release)  => Some(lookups.releaseDay(film))
        case (Question.FilmRecord(film), Dependency.Aspect.Cast)     => Some(lookups.cast(film))
        case (Question.FilmRecord(film), Dependency.Aspect.Overview) => Some(lookups.overview(film))
        case _                                                       => None
      }
  }

  /** The takes a cluster the families agreed on no film for (or one a venue poster vetoed) is read by, IN ORDER: the
   *  first to take a film decides, and none after it is read (nor asks a question). The order is behaviour:
   *  - [[PosterVoteTake]]: the one candidate a venue poster matches within [[PosterEvidence.VoteBits]], none vetoing it;
   *  - [[BroadcastTake]]: the one record of the billed stage work broadcast on a screening day ([[agreement.Broadcast]]);
   *  - [[FillTake]]: the first selected unified fill rule's film ([[services.identity.UnifiedRules.filled]]), unless a venue
   *    poster vetoes it;
   *  - [[CatalogueTake]]: the film the listings' own catalogue ids name ([[agreement.Catalogue]]). Last though an exact id:
   *    where it names another film than a take above, the projection keeps that take, for the measure to list.
   *  A film the families agree on for a listing billing a stage work yields to [[BroadcastTake]] first (PL Kino Amok's bare
   *  "Manon" on the Met's broadcast day is the Met's, not Clouzot's). `identity-resolver.md` §21 lists this order; a spec
   *  holds the two together. */
  val PosterVoteTake = "poster-vote"
  val BroadcastTake  = "broadcast"
  val FillTake       = "fill"
  val CatalogueTake  = "catalogue"
  val FallThrough: Seq[String] = Seq(PosterVoteTake, BroadcastTake, FillTake, CatalogueTake)

  /** A model no-match the families' agreement took no film for, as the takes read it: its cluster id, listings and their
   *  digest, the families' verdict, and its venue posters' distances to its candidates (and to `also`). */
  private[agreement] final case class Unagreed(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], verdict: StoredVerdict,
                                   distances: Option[Int] => Answer[Seq[Map[Int, Option[Int]]]])
  /** One apply at the families' `version`: where its takes and corrections note what they wait on (`asked`: the families'
   *  questions, the IMDb ids TMDB was not asked about, the posters, the catalogue questions and the records to read again
   *  for their day) with the verdicts it moved, the clusters and model takes it read, and its cost. */
  private[agreement] final class Pass(val version: Long, corrections: Int, val reading: Read, backfills: Int = Int.MaxValue) {
    val asked        = mutable.Set.empty[Question]
    /** The posters the model takes' corrections wait on: handed to the queue a few at a time ([[wanted]]). */
    val correctionPosters = mutable.Set.empty[PosterQuestion]
    val moved        = mutable.ArrayBuffer.empty[StoredVerdict]
    /** The clusters read, by id, and the model takes read against the evidence, by their correction's id. */
    val seen         = mutable.Set.empty[String]
    val correcting   = mutable.Set.empty[String]
    /** The shapes (listings' digests) of the clusters read. */
    val shapes       = mutable.Set.empty[Long]
    /** Each decision's cluster key ([[AgreementStage.keying]]). */
    val keys         = new java.util.IdentityHashMap[ResolverDecision, String]()
    /** What the apply's full validation of the kept verdicts came to ([[AgreementStage.FullValidationEvery]]). */
    var validated    = services.identity.Memo.Validation.None
    private var cleanHits = 0
    private var validateAt, validateUntil = 0
    /** A full validation of the clean verdicts this apply meets from the `from`th, `budget` of them. */
    def validateFrom(from: Int, budget: Int): Unit = { validateAt = from; validateUntil = from + budget }
    /** Is the clean verdict met now one the apply's full validation reads again? Each asking counts one met. */
    def validating(): Boolean = { val at = cleanHits; cleanHits += 1; at >= validateAt && at < validateUntil }
    /** How many clusters the resolver was asked about again — the apply's cost — and how many agreed films a poster vetoed. */
    var resolves     = 0
    var posterVetoed = 0
    private val freshReads    = new Budget(corrections)
    private val backfillReads = new Budget(backfills)
    /** May one more model take be read afresh ([[CorrectionsPerApply]])? Each asking spends one of the budget. */
    def fresh(): Boolean = freshReads.spend()
    /** The clusters whose verdict decided under other rules this apply decided again, by id. */
    val backfilled = mutable.Set.empty[String]
    /** May one more verdict decided under other rules be decided again ([[BackfillPerApply]])? Each asking spends one. */
    def backfill(): Boolean = backfillReads.spend()
    /** Did a take or a verdict go unread for want of budget: asked once the budget was spent? */
    def spent: Boolean = freshReads.overdrawn || backfillReads.overdrawn
    /** What the apply met unanswered: the corrections' posters only `postersAtOnce` at a time, the backfill of every
     *  take's posters draining behind every other task — those not `tried` yet first, so posters that keep failing hold
     *  no slot another poster could use. */
    def wanted(postersAtOnce: Int, tried: Question => Boolean): Set[Question] = {
      val capped = if (correctionPosters.sizeIs <= postersAtOnce) correctionPosters
        else correctionPosters.toSeq.sortBy(poster => (tried(Question.Poster(poster)), PosterAnswers.idOf(poster))).take(postersAtOnce)
      asked.toSet ++ capped.iterator.map(Question.Poster(_))
    }
  }

  /** One apply's allowance of some costly read: each [[spend]] asks for one, granted while any is left; [[overdrawn]] once
   *  one was asked for past it — a read left for a later apply. */
  private[agreement] final class Budget(allowance: Int) {
    private var left = allowance
    def spend(): Boolean = { val granted = left > 0; left -= 1; granted }
    def overdrawn: Boolean = left < 0
  }

  /** What became of the film a cluster's families agree on: taken, pending an answer, vetoed by a venue poster, refused by
   *  the venue's synopsis (and why), or none to take. */
  private[agreement] enum Take {
    case Taken(decision: ResolverDecision)
    case Pending
    case Vetoed
    case Refused(why: String)
    case Untaken
  }

  /** A poster to hash: a venue's, by its URL, or a TMDB film's. */
  enum PosterQuestion {
    case Venue(url: String)
    case Film(tmdbId: Int)
  }

  /** `inner`, a listings site's catalogue entry linked as a listing's page read as no page at all ([[uncatalogued]]). */
  private[agreement] final class CatalogueBlind(inner: IdentityLookups) extends services.identity.ForwardingIdentityLookups(inner) {
    private def entry(listing: Listing) = listing.page.exists(services.identity.SourceReliability.listingsSiteEntry)
    override def hasDetail(listing: Listing): Boolean                  = !entry(listing) && inner.hasDetail(listing)
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = if (entry(listing)) Answer.Known(None) else inner.detail(listing)
  }

  /** `inner`, noting whether any answer it gave was `Unknown` — a cast, overview or release day not known left unnoted:
   *  it leaves the evidence that reads it unread, never the fill waiting. */
  private[agreement] final class UnknownNoting(inner: IdentityLookups) extends services.identity.ForwardingIdentityLookups(inner) {
    @volatile var unknown = false
    private def noted[A](answer: Answer[A]): Answer[A] = { if (answer == Answer.Unknown) unknown = true; answer }
    override def detail(listing: Listing): Answer[Option[DetailFacts]]    = noted(inner.detail(listing))
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]]      = noted(inner.candidates(query))
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = noted(inner.film(tmdbId))
  }

  /* A model take's correction (Correction) is stored as its listings' digest and the `version` it was read (or last
   * found standing) at, the digests of the answers it read — a filing of one decides it again — what it came to, and
   * the questions it waits on. */
  /** The dependencies no filing announces, which the held verdicts' index keeps their readers of ([[Memo.moved]]): the
   *  year, and the question memory's attempts. */
  val pushed: Dependency => Boolean = read => read == Dependency.Year || isAttempt(read)
  val isAttempt: Dependency => Boolean = {
    case Dependency.Attempted(_) => true
    case _                       => false
  }

  /** A model take's correction's id among the stored verdicts. */
  def correctionId(cluster: String): String = s"$CorrectionPrefix$cluster"
  /** What a correction's stored id is its cluster's id behind. */
  val CorrectionPrefix = "correction|"
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

  /** `decisions` each decided by `decide` in their content's order — by the least of their listings' keys, which no two
   *  clusters share — and handed back in theirs: the budgets ([[BackfillPerApply]], [[CorrectionsPerApply]]) reach the
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
  /** How many of the posters the model takes' corrections wait on are handed to the queue at once: a one-off backfill of
   *  every take's posters (17.6k over the five corpora, 2026-10-06) drained a few at a time behind every other task. */
  val CorrectionPostersAtOnce = 64
  /** How often the kept verdicts are validated whole: one apply in this many that reads anything, as the projection
   *  reconciles the whole corpus between its scoped ticks. */
  val FullValidationEvery = 12
  /** How many kept verdicts one full validation reads again, resuming where the last stopped: the whole set read over
   *  successive validations, never one apply's spike (re-reading every verdict at once was prod PL's 40 s, 514 MB boot). */
  val FullValidationBudget = 2000

  /** `posters` and `programmes` (the family whose site lists the venues' programmes), each answer decoded once: an
   *  apply's reads, dropped with it. */
  private[agreement] final class Read(hashes: PosterAnswers, family: Option[FamilyAnswers]) {
    private val venues = mutable.HashMap.empty[String, Answer[Option[PosterHash]]]
    private val films  = mutable.HashMap.empty[Int, Answer[Seq[PosterHash]]]
    val posters: PosterAnswers = new PosterAnswers {
      def venue(url: String): Answer[Option[PosterHash]] = venues.getOrElseUpdate(url, hashes.venue(url))
      def film(tmdbId: Int): Answer[Seq[PosterHash]]     = films.getOrElseUpdate(tmdbId, hashes.film(tmdbId))
      override def unread(question: PosterQuestion): Boolean = hashes.unread(question)
      override def fresh(question: PosterQuestion): Boolean  = hashes.fresh(question)
    }
    val programmes: Option[FamilyAnswers] = family.map { answers =>
      val showings = mutable.HashMap.empty[String, Answer[Seq[Showing]]]
      val records  = mutable.HashMap.empty[String, Answer[Option[SourceRecord]]]
      new FamilyAnswers {
        val family: VoterFamily = answers.family
        def titled(text: String): Answer[Seq[SourceHit]]     = answers.titled(text)
        def directedBy(name: String): Answer[Seq[SourceHit]] = answers.directedBy(name)
        def record(id: String): Answer[Option[SourceRecord]] = records.getOrElseUpdate(id, answers.record(id))
        override def showing(venue: String): Answer[Seq[Showing]] = showings.getOrElseUpdate(venue, answers.showing(venue))
        override def fresh(question: String): Boolean = answers.fresh(question)
      }
    }
  }

  /** A model decision's cluster key, its listings sorted, and their digest with TMDB's vote on it. */
  private[agreement] final case class Digested(id: String, listings: Seq[Listing], digest: Long)

  /** The TMDB film a no-match leans to, as the agreement links it: by its TMDB and IMDb ids alone. */
  def leanRecord(lean: ResolverDecision.Leaning): SourceRecord =
    SourceRecord(services.identity.IdentityMeasures.Film(""), Map("tmdb" -> lean.film.toString) ++
      services.identity.IdentityMeasures.imdbId(lean.imdbNumber).map("imdb" -> _))

  /** What one [[AgreementStage.apply]] that read anything came to: the clusters waiting on a family's answer, the verdicts
   *  kept and how many agreed, the decisions taken as a TMDB film or an IMDb fallback, the questions still open per family
   *  and TMDB finds, how many clusters it resolved again, and how long it took; and the venue posters' part: the decisions
   *  they took ([[ResolverDecision.Basis.Poster]]), the agreed films they vetoed, and the posters not hashed yet; and the
   *  decisions the screening days took ([[ResolverDecision.Basis.Broadcast]]), a fill rule took
   *  ([[ResolverDecision.Basis.Filled]]) and the listings' catalogue ids took ([[ResolverDecision.Basis.Catalogue]]), with
   *  the catalogue questions still open, and the TMDB records the broadcast take waits on to read again; and the verdicts
   *  decided under other rules still to be decided again (`incomplete`: a deploy's backfill, [[BackfillPerApply]]); and
   *  the full validation's kept verdicts read again and those found drifted ([[FullValidationEvery]]). */
  final case class Applied(waiting: Int, verdicts: Int, agreed: Int, takenTmdb: Int, takenFallback: Int, open: Map[VoterFamily, Int],
                           finds: Int, resolves: Int, seconds: Double, takenPoster: Int = 0, posterVetoed: Int = 0, posters: Int = 0,
                           takenBroadcast: Int = 0, takenFilled: Int = 0, takenCatalogue: Int = 0, catalogue: Int = 0, undated: Int = 0,
                           withdrawn: Int = 0, corrected: Int = 0, correcting: Int = 0, correctionPosters: Int = 0,
                           events: Int = 0, incomplete: Int = 0, validated: Int = 0, drifted: Int = 0)
  trait Metrics { def applied(applied: Applied): Unit }
  object Metrics { val Silent: Metrics = _ => () }

  /** A cluster (or a take's correction) waiting on answers: its listings' digest and the questions it waits on — none where
   *  only its fill's TMDB answers were missing (`blindAt`), resolved again once something is filed, and until then read as
   *  the verdict that resolve gave (`read`). */
  private[agreement] final case class Waiting(listings: Long, gaps: Set[Question], blindAt: Option[Long] = None,
                                              read: Option[StoredVerdict] = None)
  /** A cluster's candidates read at its listings' `digest` — `blindAt` the version they were read at while a TMDB search had
   *  no answer, to be read again at another — with what the read took (its receipt). */
  private[agreement] final case class CandidateFilms(digest: Long, films: Seq[Int], blindAt: Option[Long], reads: Map[Dependency, Long])
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
   *  (`IdentityProjectionWiring.agreementFamilies`, `identities`, `listedOn`): a verdict decided without Filmweb stood
   *  as it was once Filmweb was asked, since a receipt names only the answers its verdict read. */
  private[agreement] def configured(families: Set[VoterFamily], identities: Seq[VoterFamily], listedOn: Option[VoterFamily],
                                    calibration: IdentityCalibration): String =
    Seq(families.toSeq.sortBy(_.ordinal).map(_.label).mkString(","), identities.map(_.label).mkString(","),
      listedOn.fold("")(_.label), calibration.##.toString).mkString(";")
}
