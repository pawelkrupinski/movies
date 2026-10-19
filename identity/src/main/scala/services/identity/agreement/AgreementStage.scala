package services.identity.agreement

import services.identity.{Answer, CandidateQuery, CatalogueAnswers, CatalogueQuestion, DetailFacts, Hit, IdentityCalibration, IdentityLookups,
  IdentityMeasures, Listing, ListingShape, PosterAnswers, PosterEvidence, PosterHash, Resolution, ResolverDecision, StoredFamily}
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
 * filed again or its listings move.
 */
final class AgreementStage(private[agreement] val families: Map[VoterFamily, FamilyAnswers], private[agreement] val venues: IdentityLookups, private[agreement] val normalizer: TitleNormalizer,
                           private[agreement] val calibration: IdentityCalibration, private[agreement] val tmdbOf: String => Answer[Option[Int]], private[agreement] val stored: AgreementVerdicts,
                           private[agreement] val ask: AgreementStage.Open => Unit = _ => (), private[agreement] val metrics: AgreementStage.Metrics = AgreementStage.Metrics.Silent,
                           private[agreement] val clock: java.time.Clock, private[agreement] val changes: AnswerChanges = AnswerChanges.Unknown, private[agreement] val posters: PosterAnswers = PosterAnswers.Silent,
                           private[agreement] val tmdb: Option[IdentityLookups] = None, private[agreement] val identities: Seq[VoterFamily] = Nil,
                           private[agreement] val rules: services.identity.UnifiedRules = services.identity.UnifiedRules.resolver,
                           private[agreement] val catalogue: CatalogueAnswers = CatalogueAnswers.Silent, private[agreement] val listedOn: Option[VoterFamily] = None,
                           private[agreement] val correctionPostersAtOnce: Int = AgreementStage.CorrectionPostersAtOnce,
                           private[agreement] val correctionsPerApply: Int = AgreementStage.CorrectionsPerApply) extends AgreementCorrections, AgreementTakes, AgreementCandidates {

  /** What a stored verdict was decided under: the stage's code and the selected rules' version — a refit of the rules
   *  decides every verdict again, as a change of the code does. */
  private[agreement] val decidedUnder: String = s"${AgreementStage.RulesVersion}|${rules.version}"

  /** Everything the last [[apply]] met unanswered: what the getters below read. */
  @volatile private var wantedNow: AgreementStage.Open = AgreementStage.Open.empty
  /** The clusters' family questions no family has answered yet, as the last [[apply]] met them. */
  def wanted: Set[(VoterFamily, String)] = wantedNow.questions
  /** The agreed IMDb ids TMDB was not asked about yet, as the last [[apply]] met them. */
  def wantedFinds: Set[String] = wantedNow.finds
  /** The posters not hashed yet that a cluster's take waits on, as the last [[apply]] met them. */
  def wantedPosters: Set[AgreementStage.PosterQuestion] = wantedNow.posters
  /** The catalogue ids not mapped yet, and the venue pages whose links are not read yet, that a cluster's take waits on,
   *  as the last [[apply]] met them. */
  def wantedCatalogue: Set[CatalogueQuestion] = wantedNow.catalogue
  /** The TMDB records a cluster's broadcast take waits on to read again — filed before records kept the whole release
   *  day ([[IdentityLookups.releaseDay]]) — as the last [[apply]] met them. */
  def wantedRecords: Set[Int] = wantedNow.records

  /** The families in their order: the order their verdicts and searches are read in. */
  private[agreement] val familiesInOrder: Seq[(VoterFamily, FamilyAnswers)] = families.toSeq.sortBy(_._1.ordinal)
  /** Each family's calibration: TMDB's, its search priors' spread scaled by the family's [[VoterFamily.priorSpread]]. */
  private[agreement] val calibrations: Map[VoterFamily, IdentityCalibration] = families.keys.map(family => family -> calibration.withPriorSpread(family.priorSpread)).toMap
  private[agreement] lazy val held: TrieMap[String, StoredVerdict] = TrieMap.from(stored.all().map(v => v.id -> v))
  /** The `version` the stored verdicts were loaded at: what this worker's families answered cannot have moved while it
   *  was down — it alone files them — so a verdict kept under these rules, its listings as they were, stands then without
   *  a single answer read again (prod PL 2026-10-05: re-reading every verdict's answers at boot was a 40 s, 514 MB pass). */
  private[agreement] var loadedAt: Option[Long] = None
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
  /** The verdicts whose answers were last read at a `version`, with their listings' digest then. */
  private[agreement] val checked = TrieMap.empty[String, (Long, Long)]
  /** The clusters a family has not answered for yet, at the `version` and listings' digest they were last resolved at,
   *  with the questions they wait on: not resolved again until one of those is answered or their listings move. */
  private[agreement] val waiting = TrieMap.empty[String, AgreementStage.Waiting]
  /** The questions handed to `ask` since the families' answers last moved (at `handedAt`): a tick that filed nothing
   *  hands nothing. */
  private[agreement] var handed   = AgreementStage.Open.empty
  private[agreement] var handedAt = -1L
  /** Each TMDB record handed to `ask` to read again for its day, with the version and instant it was handed at — asked
   *  ONCE in the stage's life: a record still holding its year after the read was filed ([[AgreementStage.recordReadId]]),
   *  or after [[AgreementStage.RecordWait]], is waited on no more. One entry per stage-work record, a few hundred. */
  private[agreement] val rereads = TrieMap.empty[Int, (Long, java.time.Instant)]
  /** Each cluster's TMDB candidates, none denied, at its listings' digest: what its posters are compared against. */
  private[agreement] val candidateFilms = TrieMap.empty[String, AgreementStage.CandidateFilms]
  /** The `version` the current [[apply]] runs at. */
  private[agreement] var applying = -1L
  /** What a correction still waiting on an answer asks, by its stored id: a take's posters, families' questions and
   *  finds while the backfill hashes them — none kept once its evidence is all answered. */
  private[agreement] val correctionWaits = TrieMap.empty[String, AgreementStage.CorrectionWaits]

  /** The last resolution applied to, at which `version`, and what it came to: a tick handed the same decisions while
   *  no answer was filed gets those again, nothing re-read. */
  private[agreement] var last: Option[(Resolution, Long, Resolution)] = None
  /** The first instant a wait of the last [[apply]]'s ends — a waiting cluster's [[AgreementStage.PartialAfter]], a
   *  record's [[AgreementStage.RecordWait]]: from then the same decisions at the same version are read again, since
   *  the clock, not a filing, is what releases them. */
  private[agreement] var releaseAt: Option[java.time.Instant] = None

  /** `resolution` with every unmatched, fully answered cluster its families agree on taken as that film. `republished`:
   *  the listings published again since the last apply equal to what the model holds but for a field the stage reads
   *  and the model does not — a poster, screening days, names ([[Listing.movedOutsideEquality]]) — which hand over no new
   *  decision; `None` when the caller cannot tell, and every cluster's listings are read again. */
  def apply(resolution: Resolution, listingOf: ListingKey => Option[Listing], version: Long,
            republished: Option[Iterable[ListingKey]] = Some(Nil)): Resolution = synchronized {
    val reread = republished.fold { digested.clear(); true }(forgetRead(resolution, _))
    val waited = releaseAt.exists(at => !clock.instant().isBefore(at))
    last.collect { case (was, at, came) if !reread && !waited && at == version && sameDecisions(was, resolution) =>
      resolution.copy(decisions = came.decisions) }.getOrElse {
      val came = applied(resolution, listingOf, version)
      last = Some((resolution, version, came))
      came
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
    if (loadedAt.isEmpty) {   // first pass: every verdict kept under these rules stands at this version
      loadedAt = Some(version)
      held.valuesIterator.filter(_.rules == decidedUnder).foreach(v => checked(v.id) = (v.listings, version))
    }
    val started = tools.Stopwatch.start()
    applying = version
    val pass      = new AgreementStage.Pass(version, correctionsPerApply, new AgreementStage.Read(posters, listedOn.flatMap(families.get)))
    val decisions = resolution.decisions.map(decide(_, listingOf, pass))
    forgetGone(resolution, pass)
    wantedNow = pass.wanted(correctionPostersAtOnce)
    handOver(version)
    releaseAt = nextRelease()
    report(decisions, pass, started)
    resolution.copy(decisions = decisions)
  }

  /** What the model's `decision` is taken as: a model take read against the evidence that may correct it, a no-match
   *  the families, the posters, the screening days, a fill or the catalogue ids take a film for — else the decision as
   *  the model left it. What it reads and waits on is noted in `pass`. */
  private[agreement] def decide(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], pass: AgreementStage.Pass): ResolverDecision =
    if (correctable(decision) && eventOf(decision, listingOf).isEmpty) {
      val key = AgreementStage.correctionId(StoredFamily.idOf(decision.members))
      pass.correcting += key
      corrected(decision, listingOf, key, pass)
    }
    else if (decision.film.isDefined || decision.unanswered > 0 || decision.fallback.isDefined || decision.members.isEmpty) decision
    else {
      val now = eventOf(decision, listingOf) match {
        // an event no film database holds: no family is asked about it, no poster hashed, no verdict kept
        case Some(why) => taking(decision)(ResolverDecision.Basis.Event, s"event, not a film: $why")
        case None =>
          val AgreementStage.Digested(id, listings, digest, lean) = digestedOf(decision, listingOf)
          pass.seen += id
          val verdict = verdictOf(decision, id, listings, digest, lean, pass)
          // the venue posters' distances to the cluster's candidates and to the film the families agree on
          def distances(also: Option[Int]) = posterDistances(id, digest, listings, also, pass.postersAsked)
          // the posters vote once the families reached a verdict that takes no film: none agreed, or a poster vetoed it
          verdict.fold(decision) { v =>
            lazy val unagreed = AgreementStage.Unagreed(decision, id, digest, listings, v, distances)
            pieceOf(v, listings).fold(
            v.agreed.fold[AgreementStage.Take](AgreementStage.Take.Untaken)(taken(decision, _, pass.finding, distances,
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

  /** Forget what the clusters `resolution` no longer holds were kept with — their verdicts, waits, candidates,
   *  corrections and the model's decisions — and store the verdicts `pass` moved. */
  private[agreement] def forgetGone(resolution: Resolution, pass: AgreementStage.Pass): Unit = {
    val removed = held.keySet.toSet -- pass.seen -- pass.correcting
    waiting --= waiting.keySet.toSet -- pass.seen
    candidateFilms --= candidateFilms.keySet.toSet -- pass.seen   // a correction's candidates are read, never kept
    correctionWaits --= correctionWaits.keySet.toSet -- pass.correcting
    if (removed.nonEmpty || pass.moved.nonEmpty) {
      stored.replace(removed, pass.moved.toSeq)
      held --= removed; checked --= removed
      pass.moved.foreach(v => held(v.id) = v)
    }
    val current = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[ResolverDecision, java.lang.Boolean]())
    resolution.decisions.foreach(current.add)
    takenAs.keySet.retainAll(current)   // the model's decisions this resolution no longer holds
    digested.keySet.retainAll(current)
    events.keySet.retainAll(current)
  }

  /** Hands `ask` what the last apply met unanswered and was not handed at `version` yet — a record to read again for
   *  its day ONCE in the stage's life ([[rereads]]). */
  private[agreement] def handOver(version: Long): Unit = {
    if (handedAt != version) { handed = AgreementStage.Open.empty; handedAt = version }
    val unhanded = wantedNow -- handed
    val open     = unhanded.copy(records = unhanded.records.filterNot(rereads.contains))
    if (!open.isEmpty) {
      ask(open); handed = handed ++ open
      open.records.foreach(film => rereads(film) = (version, clock.instant()))
    }
  }

  /** The first instant a wait ends: a waiting cluster's [[AgreementStage.PartialAfter]], a record's
   *  [[AgreementStage.RecordWait]] — none passed yet. */
  private[agreement] def nextRelease(): Option[java.time.Instant] = {
    val now = clock.instant()
    (waiting.valuesIterator.map(_.since.plusMillis(AgreementStage.PartialAfter.toMillis)) ++
      rereads.valuesIterator.map(_._2.plusMillis(AgreementStage.RecordWait.toMillis))).filter(_.isAfter(now)).minOption
  }

  /** What the apply came to, for [[metrics]]. */
  private[agreement] def report(decisions: Seq[ResolverDecision], pass: AgreementStage.Pass, started: tools.Stopwatch.Started): Unit = {
    val agreedNow = decisions.filter(_.basis == ResolverDecision.Basis.Agreed)
    metrics.applied(AgreementStage.Applied(waiting = waiting.size, verdicts = held.valuesIterator.count(_.correction.isEmpty),
      agreed = held.valuesIterator.count(_.agreed.isDefined),
      takenTmdb = agreedNow.count(_.film.isDefined), takenFallback = agreedNow.count(_.fallback.isDefined),
      open = wantedNow.questions.groupMapReduce(_._1)(_ => 1)(_ + _), finds = wantedNow.finds.size, resolves = pass.resolves, seconds = started.seconds,
      takenPoster = decisions.count(_.basis == ResolverDecision.Basis.Poster), posterVetoed = pass.posterVetoed, posters = wantedNow.posters.size,
      takenBroadcast = decisions.count(_.basis == ResolverDecision.Basis.Broadcast), takenFilled = decisions.count(_.basis == ResolverDecision.Basis.Filled),
      takenCatalogue = decisions.count(_.basis == ResolverDecision.Basis.Catalogue), catalogue = wantedNow.catalogue.size, undated = wantedNow.records.size,
      withdrawn = decisions.count(_.basis == ResolverDecision.Basis.Withdrawn), corrected = decisions.count(_.basis == ResolverDecision.Basis.Corrected),
      correcting = pass.correcting.size, correctionPosters = pass.correctionPosters.size,
      events = decisions.count(_.basis == ResolverDecision.Basis.Event)))
  }

  /** `d` taken on `basis`, `line` explaining it — with the `film`, `fallback` and `agreed` ids it takes, where they move. */
  private[agreement] def taking(d: ResolverDecision)(basis: ResolverDecision.Basis, line: String, film: Option[Int] = d.film,
                                          fallback: Option[ResolverDecision.Fallback] = d.fallback, agreed: Map[String, String] = d.agreed): ResolverDecision =
    d.copy(film = film, basis = basis, explanation = d.explanation :+ line, fallback = fallback, agreed = agreed)(d.trace)

  /** The takes a cluster the families' agreement took no film for is read by, by name — in [[AgreementStage.FallThrough]]'s
   *  order, the first to take one deciding: each reads its own evidence and notes in `asks` (the apply's [[AgreementStage.Pass]])
   *  every question it waits on. */
  private[agreement] val takes: Map[String, (AgreementStage.Unagreed, AgreementStage.Pass) => Option[ResolverDecision]] = Map(
    AgreementStage.PosterVoteTake -> ((u, _) => voted(u.decision, u.distances(None))),
    AgreementStage.BroadcastTake  -> ((u, asks) => broadcast(u.decision, u.id, u.digest, u.listings, asks.dating, asks.version, asks.asked)),
    AgreementStage.FillTake       -> ((u, asks) => filledTake(u.decision, u.verdict, asks.finding, u.distances,
      film => synopsisAgainst(u.id, u.digest, u.listings, film))),
    AgreementStage.CatalogueTake  -> ((u, asks) => catalogued(u.decision, u.listings, asks.asked, asks.finding, asks.catalogue)))
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

  /** The decision's cluster id, its listings sorted and their digest, kept while the model hands the same decision over. */
  private[agreement] def digestedOf(decision: ResolverDecision, listingOf: ListingKey => Option[Listing]): AgreementStage.Digested =
    Option(digested.get(decision)).getOrElse {
      val listings = sortedListings(decision, listingOf)
      val fresh    = AgreementStage.Digested(StoredFamily.idOf(decision.members), listings,
        AgreementStage.digest(Seq(digestOf(listings).toString, decision.leaning.toString, decision.candidate.toString,
          PosterEvidence.urls(listings).mkString("\u0001"))), voteOf(decision))
      digested.put(decision, fresh); fresh
    }

  /** TMDB's own vote on a no-match: the film it leans to, else the best-ranked candidate it weighed — its record as the
   *  model read it, so it links to a family's pick by facts as well as by ids. */
  private[agreement] def voteOf(decision: ResolverDecision): Option[SourceRecord] =
    decision.leaning.orElse(decision.candidate).map { vote =>
      val ids = AgreementStage.leanRecord(vote)
      venues.film(vote.film).toOption.flatten.fold(ids)(film => SourceRecord(film,
        ids.crossIds ++ IdentityMeasures.imdbId(film.imdbNumber).map("imdb" -> _)))
    }

  /** The listings as published, the people their own text names, and the venue detail page the picks read of each. */
  private[agreement] def digestOf(listings: Seq[Listing]): Long =
    AgreementStage.digest(listings.map(l => l.sortKey + l.screenedSeason.fold("")(season => s"\u0002$season") +
      (if (l.names.isEmpty) "" else s"\u0003${l.names.##}") +
      (if (venues.hasDetail(l)) "\u0001" + venues.detail(l) else "")))

  /** The cluster's verdict: the stored one while its listings and every answer it read stand, else the resolver's
   *  over the families' answers now — `None` while one of them is a gap. */
  private[agreement] def verdictOf(decision: ResolverDecision, id: String, listings: Seq[Listing], digest: Long, lean: Option[SourceRecord],
                        pass: AgreementStage.Pass): Option[StoredVerdict] = {
    val version = pass.version
    held.get(id).filter(v => v.listings == digest && v.rules == decidedUnder).filter { kept =>
      checked.get(id).contains((digest, version)) || untouched(id, kept, digest, version) || {
        val stands = kept.reads.forall { case (question, answered) => read(question).exists(answer => AgreementStage.digest(Seq(answer.toString)) == answered) }
        // read again because an answer was filed: a stale one among its reads is asked again too — a quiet tick reads none
        if (stands) { checked(id) = (digest, version); pass.asked ++= kept.reads.keys.flatMap(staleOf) }
        stands
      }
    }
    .orElse(waiting.get(id).filter(w => w.listings == digest && !due(w)) match {
      case Some(w) => pass.asked ++= w.gaps; None   // its questions not all answered yet: a resolve now could reach no verdict
      case None    => resolved(decision, id, digest, listings, lean, pass)
    })
  }

  /** Does a kept verdict still stand at `version` without reading its answers — checked at an earlier version, none of
   *  the answers it read filed again since ([[AnswerChanges]])? A filing elsewhere re-reads no other verdict. */
  private[agreement] def untouched(id: String, kept: StoredVerdict, digest: Long, version: Long): Boolean =
    checked.get(id).collect { case (listed, at) if listed == digest => at }
      .flatMap(changes.changedSince).exists(refiled => !kept.reads.keysIterator.exists(refiled)) && {
      checked(id) = (digest, version); true
    }

  /** Every family's verdict on `listings`, in the families' order — each question read noted in `reads`, each gap in
   *  `gaps`. */
  private[agreement] def verdictsOf(listings: Seq[Listing], gaps: mutable.Set[(VoterFamily, String)], reads: mutable.Map[String, Long]): Seq[Answer[FamilyVerdict]] =
    familiesInOrder.map { case (_, answers) =>
      Agreement.verdict(listings, new Recording(answers, gaps, reads), venues, normalizer, calibrations(answers.family))
    }

  /** The cluster's verdict, the resolver asked again over every family's answers now — `None`, and the cluster kept
   *  waiting on its questions, while one is a gap. */
  private[agreement] def resolved(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], lean: Option[SourceRecord],
                       pass: AgreementStage.Pass): Option[StoredVerdict] = {
    val version = pass.version
    pass.resolves += 1
    val reads = mutable.Map.empty[String, Long]
    val gaps  = mutable.Set.empty[(VoterFamily, String)]
    val verdicts = verdictsOf(listings, gaps, reads)
    pass.asked ++= gaps
    if (verdicts.contains(Answer.Unknown)) { waiting(id) = AgreementStage.Waiting(digest, gaps.toSet, clock.instant()); None }
    else {
      waiting -= id
      Some {
        val thisYear = java.time.LocalDate.ofInstant(clock.instant(), java.time.ZoneOffset.UTC).getYear
        val known    = verdicts.flatMap(_.toOption)
        val stated   = listings.map(asStated)
        val weighed  = Agreement.weighed(listings, known, lean, Some(thisYear), stated)
        val agreed   = weighed.agreed
        // an agreement only review sites make is taken as nothing ([[taken]]): the fill reads the cluster as one none agreed on
        val fill     = if (agreed.exists(_.families.exists(_.namesFilms))) Answer.Known(None) else filledOf(decision, listings, known, stated, thisYear, version, pass.asked)
        val verdict  = StoredVerdict(id, digest, reads.toMap, agreed, decidedUnder, fill.toOption.flatten, refused = weighed.refused)
        if (fill == Answer.Unknown) {
          // the fill met a TMDB question not answered yet: this pass reads the verdict, none keeps it — kept, it stood once
          // TMDB had answered (its reads are the families' only), and the fill was never read again (PL "Lalka (Dolly)" ×5)
          waiting(id) = AgreementStage.Waiting(digest, Set.empty, clock.instant(), blindAt = Some(version))
          checked -= id
        } else {
          if (!held.get(id).contains(verdict)) pass.moved += verdict
          checked(id) = (digest, version)
        }
        verdict
      }
    }
  }

  /** Has the family answered `question` since — any answer, fresh? Read per waiting cluster, per tick: an answer filed
   *  for another cluster's question resolves none of the rest again (prod PL 2026-10-04: re-resolving every waiting
   *  cluster on each filing ran a 24 s, 540 MB agreement phase back to back). */
  private[agreement] def answered(gap: (VoterFamily, String)): Boolean = families.get(gap._1).exists(_.fresh(gap._2))

  /** Is a waiting cluster due a resolve? Once every question it waits on is answered — any gap left, and the families
   *  could reach no verdict, so resolving on each answer as a backlog drains (every cluster gets one, every tick) only
   *  re-ran the same partial resolve: prod PL 2026-10-04, 25–48 s an agreement phase after the per-question fix — or once
   *  some are and it has waited [[AgreementStage.PartialAfter]], so a question never answered holds no cluster for ever. */
  private[agreement] def due(waiting: AgreementStage.Waiting): Boolean =
    // a fill read blind waits on no family's question but on TMDB's answer: due once something is filed, not every pass
    waiting.blindAt.fold(waiting.gaps.forall(answered))(_ != applying) ||
      (!clock.instant().isBefore(waiting.since.plusMillis(AgreementStage.PartialAfter.toMillis)) && waiting.gaps.exists(answered))

  /** A stored read whose answer is stale, as the question to ask again. */
  private[agreement] def staleOf(question: String): Option[(VoterFamily, String)] = question.split("\\|", 2) match {
    case Array(label, asked) => families.collectFirst { case (family, answers) if family.label == label && !answers.fresh(asked) => family -> asked }
    case _                   => None
  }

  /** A stored read's answer now: `<family>|<kind>|<text>`, as [[Recording]] notes it. */
  private[agreement] def read(question: String): Option[Answer[?]] = question.split("\\|", 3) match {
    case Array(label, kind, text) =>
      families.collectFirst { case (family, answers) if family.label == label => answers }.flatMap { answers =>
        kind match {
          case "title"    => Some(answers.titled(text))
          case "director" => Some(answers.directedBy(text))
          case "record"   => Some(answers.record(text))
          case "showing"  => Some(answers.showing(text))
          case _          => None
        }
      }
    case _ => None
  }

  /** The TMDB film a record names: its TMDB id, else the film TMDB finds by its IMDb id — `Unknown` while that find is
   *  not answered yet, the IMDb id then noted in `finding`. */
  private[agreement] def tmdbFilmOf(tmdb: Option[Int], imdb: Option[String], finding: mutable.Set[String]): Answer[Option[Int]] =
    tmdb.fold(imdb.fold[Answer[Option[Int]]](Answer.Known(None))(Agreement.tmdbFound(tmdbOf, _, finding)))(film => Answer.Known(Some(film)))

  /** A family's answers that note every question still a gap, and the digest of every answer read. */
  private[agreement] final class Recording(answers: FamilyAnswers, asked: mutable.Set[(VoterFamily, String)], reads: mutable.Map[String, Long]) extends FamilyAnswers {
    val family: VoterFamily = answers.family
    private def noted[A](question: String, answer: Answer[A]): Answer[A] = {
      if (answer == Answer.Unknown) asked += family -> question
      else {
        reads(s"${family.label}|$question") = AgreementStage.digest(Seq(answer.toString))
        if (!answers.fresh(question)) asked += family -> question   // read, and asked again
      }
      answer
    }
    def titled(text: String): Answer[Seq[SourceHit]]     = noted(s"title|$text", answers.titled(text))
    def directedBy(name: String): Answer[Seq[SourceHit]] = noted(s"director|$name", answers.directedBy(name))
    def record(id: String): Answer[Option[SourceRecord]] = noted(s"record|$id", answers.record(id))
    override def showing(venue: String): Answer[Seq[Showing]] = noted(s"showing|$venue", answers.showing(venue))
  }
}

object AgreementStage {
  /** A fallback film standing on `source`'s `id`: one standing on another database's id than IMDb's carries its `title`
   *  and `year`, that a link to its page is built of. */
  private[agreement] def fallbackOn(source: String, id: String, title: Option[String], year: Option[Int]): ResolverDecision.Fallback = {
    val named = source != VoterFamily.Imdb.database
    ResolverDecision.Fallback(source, id, 1.0, title.filter(_ => named), year.filter(_ => named))
  }

  /** 64 bits of `parts`' text, two MurmurHash3 seeds: stable across runs, as a stored digest must be. */
  def digest(parts: Seq[String]): Long = {
    val text = parts.mkString("\u0000")
    (scala.util.hashing.MurmurHash3.stringHash(text, 0x2f1d7a3b).toLong << 32) | (scala.util.hashing.MurmurHash3.stringHash(text, 0x6c8e9cf5).toLong & 0xffffffffL)
  }

  /** The questions an [[AgreementStage.apply]] met unanswered or stale, the agreed IMDb ids TMDB was not asked about, the
   *  posters not hashed yet, the catalogue questions, and the TMDB records to read again for their release day
   *  ([[wantedRecords]]). */
  final case class Open(questions: Set[(VoterFamily, String)], finds: Set[String], posters: Set[PosterQuestion] = Set.empty,
                        catalogue: Set[CatalogueQuestion] = Set.empty, records: Set[Int] = Set.empty) {
    /** These, but for those `other` holds. */
    def --(other: Open): Open = Open(questions -- other.questions, finds -- other.finds, posters -- other.posters,
      catalogue -- other.catalogue, records -- other.records)
    /** These and those `other` holds. */
    def ++(other: Open): Open = Open(questions ++ other.questions, finds ++ other.finds, posters ++ other.posters,
      catalogue ++ other.catalogue, records ++ other.records)
    def isEmpty: Boolean = questions.isEmpty && finds.isEmpty && posters.isEmpty && catalogue.isEmpty && records.isEmpty
  }
  object Open { val empty: Open = Open(Set.empty, Set.empty) }

  /** What a TMDB record read again for its day is filed as among the answers' changes ([[AnswerChanges]]). */
  def recordReadId(tmdbId: Int): String = s"tmdb|record|$tmdbId"
  /** How long the broadcast take waits on a record's read at most — the agreement's tasks run after every other. */
  val RecordWait: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(1, java.util.concurrent.TimeUnit.DAYS)

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
  /** One apply at the families' `version`: where its takes and corrections note what they wait on — the families'
   *  questions, the IMDb ids TMDB was not asked about, the posters, the catalogue questions and the records to read again
   *  for their day — with the verdicts it moved, the clusters and model takes it read, and its cost. */
  private[agreement] final class Pass(val version: Long, corrections: Int, val reading: Read) {
    val asked        = mutable.Set.empty[(VoterFamily, String)]
    val finding      = mutable.Set.empty[String]
    val postersAsked = mutable.Set.empty[PosterQuestion]
    val catalogue    = mutable.Set.empty[CatalogueQuestion]
    val dating       = mutable.Set.empty[Int]
    /** The posters the model takes' corrections wait on: handed to the queue a few at a time ([[wanted]]). */
    val correctionPosters = mutable.Set.empty[PosterQuestion]
    val moved        = mutable.ArrayBuffer.empty[StoredVerdict]
    /** The clusters read, by id, and the model takes read against the evidence, by their correction's id. */
    val seen         = mutable.Set.empty[String]
    val correcting   = mutable.Set.empty[String]
    /** The filings' ids since each version a stored correction was checked at, hashed once per apply. */
    val changedAt    = mutable.Map.empty[Long, Option[java.util.Set[java.lang.Long]]]
    /** How many clusters the resolver was asked about again — the apply's cost — and how many agreed films a poster vetoed. */
    var resolves     = 0
    var posterVetoed = 0
    private var budget = corrections
    /** May one more model take be read afresh ([[CorrectionsPerApply]])? Each asking spends one of the budget. */
    def fresh(): Boolean = { val left = budget > 0; budget -= 1; left }
    /** What the apply met unanswered: the corrections' posters only `postersAtOnce` at a time, the backfill of every
     *  take's posters draining behind every other task. */
    def wanted(postersAtOnce: Int): Open = {
      val capped = if (correctionPosters.sizeIs <= postersAtOnce) correctionPosters
        else correctionPosters.toSeq.sortBy(PosterAnswers.idOf).take(postersAtOnce)
      Open(asked.toSet, finding.toSet, (postersAsked ++ capped).toSet, catalogue.toSet, dating.toSet)
    }
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
  /** A model take's correction's id among the stored verdicts. */
  def correctionId(cluster: String): String = s"correction|$cluster"
  /** How many model takes one apply reads the evidence of afresh: a take not read yet beyond them stands as the model
   *  took it until a later apply — the first deploy's backfill of every take spread over applies, never one boot's spike. */
  val CorrectionsPerApply = 200
  /** What a model take's correction waits on: the families' questions, the IMDb ids TMDB was not asked about, the posters,
   *  and the TMDB records of a relay it would be switched to stored with their year alone ([[wantedRecords]]). */
  private[agreement] final case class CorrectionWaits(questions: Set[(VoterFamily, String)], finds: Set[String], posters: Set[PosterQuestion], records: Set[Int])
  /** How many of the posters the model takes' corrections wait on are handed to the queue at once: a one-off backfill of
   *  every take's posters (17.6k over the five corpora, 2026-10-06) drained a few at a time behind every other task. */
  val CorrectionPostersAtOnce = 64

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

  /** A model decision's cluster id, its listings sorted, their digest with TMDB's vote on it, and that vote's record. */
  private[agreement] final case class Digested(id: String, listings: Seq[Listing], digest: Long, lean: Option[SourceRecord])

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
   *  the catalogue questions still open, and the TMDB records the broadcast take waits on to read again ([[wantedRecords]]). */
  final case class Applied(waiting: Int, verdicts: Int, agreed: Int, takenTmdb: Int, takenFallback: Int, open: Map[VoterFamily, Int],
                           finds: Int, resolves: Int, seconds: Double, takenPoster: Int = 0, posterVetoed: Int = 0, posters: Int = 0,
                           takenBroadcast: Int = 0, takenFilled: Int = 0, takenCatalogue: Int = 0, catalogue: Int = 0, undated: Int = 0,
                           withdrawn: Int = 0, corrected: Int = 0, correcting: Int = 0, correctionPosters: Int = 0,
                           events: Int = 0)
  trait Metrics { def applied(applied: Applied): Unit }
  object Metrics { val Silent: Metrics = _ => () }

  /** A cluster waiting on families' answers: its listings' digest, the questions it waits on, and since when — none
   *  where only its fill's TMDB answers were missing, resolved again on the next pass. */
  private[agreement] final case class Waiting(listings: Long, gaps: Set[(VoterFamily, String)], since: java.time.Instant, blindAt: Option[Long] = None)
  /** A cluster's candidates read at its listings' `digest` — `blindAt` the version they were read at while a TMDB search had
   *  no answer, to be read again at another. */
  private[agreement] final case class CandidateFilms(digest: Long, films: Seq[Int], blindAt: Option[Long])
  /** How long a cluster with some of its questions answered waits for the rest before it is resolved on what came. */
  val PartialAfter: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, java.util.concurrent.TimeUnit.MINUTES)

  /** The rules the stage decides by: a digest of its code and all it reaches (`agreement-rules-version.txt`, generated by
   *  `build.sbt` from `IdentityRulesSources.AgreementRoots`). A stored verdict decided under others is decided again. */
  lazy val RulesVersion: String = services.identity.ClasspathResource.version("agreement-rules-version.txt")
}
