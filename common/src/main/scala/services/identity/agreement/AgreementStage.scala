package services.identity.agreement

import services.identity.{Answer, CandidateQuery, CatalogueAnswers, CatalogueQuestion, DetailFacts, Evidence, FallbackIds, Hit, IdentityCalibration, IdentityLookups,
  IdentityMeasures, IdentityResolver, Listing, PosterAnswers, PosterEvidence, PosterHash, Resolution, ResolverDecision, StoredFamily}
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
 *
 * A cluster neither took is read last by the days its venues screen it on ([[Broadcast]]): a stage relay screening on
 * the day one record of its work was broadcast is that production ([[ResolverDecision.Basis.Broadcast]]).
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
final class AgreementStage(families: Map[VoterFamily, FamilyAnswers], venues: IdentityLookups, normalizer: TitleNormalizer,
                           calibration: IdentityCalibration, tmdbOf: String => Answer[Option[Int]], stored: AgreementVerdicts,
                           ask: AgreementStage.Open => Unit = _ => (), metrics: AgreementStage.Metrics = AgreementStage.Metrics.Silent,
                           clock: java.time.Clock, changes: AnswerChanges = AnswerChanges.Unknown, posters: PosterAnswers = PosterAnswers.Silent,
                           tmdb: Option[IdentityLookups] = None, identities: Seq[VoterFamily] = Nil,
                           rules: services.identity.UnifiedRules = services.identity.UnifiedRules.resolver,
                           catalogue: CatalogueAnswers = CatalogueAnswers.Silent, listedOn: Option[VoterFamily] = None,
                           correctionPostersAtOnce: Int = AgreementStage.CorrectionPostersAtOnce) {

  /** What a stored verdict was decided under: the stage's code and the selected rules' version — a refit of the rules
   *  decides every verdict again, as a change of the code does. */
  private val decidedUnder: String = s"${AgreementStage.RulesVersion}|${rules.version}"

  @volatile private var gaps: Set[(VoterFamily, String)] = Set.empty
  @volatile private var finds: Set[String] = Set.empty
  @volatile private var posterGaps: Set[AgreementStage.PosterQuestion] = Set.empty
  @volatile private var catalogueGaps: Set[CatalogueQuestion] = Set.empty
  @volatile private var undated: Set[Int] = Set.empty
  /** The clusters' family questions no family has answered yet, as the last [[apply]] met them. */
  def wanted: Set[(VoterFamily, String)] = gaps
  /** The agreed IMDb ids TMDB was not asked about yet, as the last [[apply]] met them. */
  def wantedFinds: Set[String] = finds
  /** The posters not hashed yet that a cluster's take waits on, as the last [[apply]] met them. */
  def wantedPosters: Set[AgreementStage.PosterQuestion] = posterGaps
  /** The catalogue ids not mapped yet, and the venue pages whose links are not read yet, that a cluster's take waits on,
   *  as the last [[apply]] met them. */
  def wantedCatalogue: Set[CatalogueQuestion] = catalogueGaps
  /** The TMDB records a cluster's broadcast take waits on to read again — filed before records kept the whole release
   *  day ([[IdentityLookups.releaseDay]]) — as the last [[apply]] met them. */
  def wantedRecords: Set[Int] = undated

  /** Each family's calibration: TMDB's, its search priors' spread scaled by the family's [[VoterFamily.priorSpread]]. */
  private val calibrations: Map[VoterFamily, IdentityCalibration] = families.keys.map(family => family -> calibration.withPriorSpread(family.priorSpread)).toMap
  private lazy val held: TrieMap[String, StoredVerdict] = TrieMap.from(stored.all().map(v => v.id -> v))
  /** The `version` the stored verdicts were loaded at: what this worker's families answered cannot have moved while it
   *  was down — it alone files them — so a verdict kept under these rules, its listings as they were, stands then without
   *  a single answer read again (prod PL 2026-10-05: re-reading every verdict's answers at boot was a 40 s, 514 MB pass). */
  private var loadedAt: Option[Long] = None
  /** Each model decision the stage took, with what it took it as: handed back as that same object while neither moved,
   *  so the projection, which diffs decisions by identity, redrafts an agreed cluster only when its verdict moved. */
  private val takenAs = new java.util.IdentityHashMap[ResolverDecision, ResolverDecision]()
  /** Each model decision's cluster id, listings and their digest, kept while the decision is the same object: the model
   *  hands over a new decision when one of its listings or a fact it reads moves, so a quiet tick reads no listing again. */
  private val digested = new java.util.IdentityHashMap[ResolverDecision, AgreementStage.Digested]()
  /** The verdicts whose answers were last read at a `version`, with their listings' digest then. */
  private val checked = TrieMap.empty[String, (Long, Long)]
  /** The clusters a family has not answered for yet, at the `version` and listings' digest they were last resolved at,
   *  with the questions they wait on: not resolved again until one of those is answered or their listings move. */
  private val waiting = TrieMap.empty[String, AgreementStage.Waiting]
  /** The questions handed to `ask` since the families' answers last moved: a tick that filed nothing hands nothing. */
  private val handed      = mutable.Set.empty[(VoterFamily, String)]
  private val handedFinds = mutable.Set.empty[String]
  private val handedPosters = mutable.Set.empty[AgreementStage.PosterQuestion]
  private val handedCatalogue = mutable.Set.empty[CatalogueQuestion]
  /** Each TMDB record handed to `ask` to read again for its day, with the version and instant it was handed at — asked
   *  ONCE in the stage's life: a record still holding its year after the read was filed ([[AgreementStage.recordReadId]]),
   *  or after [[AgreementStage.RecordWait]], is waited on no more. One entry per stage-work record, a few hundred. */
  private val rereads = TrieMap.empty[Int, (Long, java.time.Instant)]
  /** Each cluster's TMDB candidates, none denied, at its listings' digest: what its posters are compared against. */
  private val candidateFilms = TrieMap.empty[String, (Long, Seq[Int])]
  /** Each model take's correction, by cluster id ([[AgreementStage.Corrected]]): kept while its listings stand and no
   *  answer it read is filed again — one small entry per take, its reads as 64-bit digests. */
  private val corrections = TrieMap.empty[String, AgreementStage.Corrected]
  /** How many clusters the current [[apply]] asked the resolver about again — the stage's cost, for [[metrics]]. */
  private var resolves    = 0
  private var handedAt    = -1L

  /** The last resolution applied to, at which `version`, and what it came to: a tick handed the same decisions while
   *  no answer was filed gets those again, nothing re-read. */
  private var last: Option[(Resolution, Long, Resolution)] = None

  /** `resolution` with every unmatched, fully answered cluster its families agree on taken as that film. */
  def apply(resolution: Resolution, listingOf: ListingKey => Option[Listing], version: Long): Resolution = synchronized {
    last.collect { case (was, at, came) if at == version && sameDecisions(was, resolution) => resolution.copy(decisions = came.decisions) }.getOrElse {
      val came = applied(resolution, listingOf, version)
      last = Some((resolution, version, came))
      came
    }
  }

  /** The same decisions, object for object: the model hands each unchanged decision over as the same object. */
  private def sameDecisions(a: Resolution, b: Resolution): Boolean = {
    val (x, y) = (a.decisions.iterator, b.decisions.iterator)
    while (x.hasNext && y.hasNext) if (!(x.next() eq y.next())) return false
    !x.hasNext && !y.hasNext
  }

  /** The posters' hashes and the venues' programmes as one [[apply]] reads them, each decoded once: the model's takes
   *  read the same venue's programme for each of its listings, the same film's posters for each take it is a candidate of. */
  private var reading = new AgreementStage.Read(posters, listedOn.flatMap(families.get))

  private def applied(resolution: Resolution, listingOf: ListingKey => Option[Listing], version: Long): Resolution = {
    reading = new AgreementStage.Read(posters, listedOn.flatMap(families.get))
    if (loadedAt.isEmpty) {   // first pass: every verdict kept under these rules stands at this version
      loadedAt = Some(version)
      held.valuesIterator.filter(_.rules == decidedUnder).foreach(v => checked(v.id) = (v.listings, version))
    }
    val started = tools.Stopwatch.start()
    resolves = 0
    val asked  = mutable.Set.empty[(VoterFamily, String)]
    val finding = mutable.Set.empty[String]
    val postersAsked = mutable.Set.empty[AgreementStage.PosterQuestion]
    val catalogueAsked = mutable.Set.empty[CatalogueQuestion]
    val dating = mutable.Set.empty[Int]
    val moved  = mutable.ArrayBuffer.empty[StoredVerdict]
    val seen   = mutable.Set.empty[String]
    var posterVetoed = 0
    val correctionPosters = mutable.Set.empty[AgreementStage.PosterQuestion]
    val correcting = mutable.Set.empty[String]
    val changedAt  = mutable.Map.empty[Long, Option[java.util.Set[java.lang.Long]]]
    val decisions = resolution.decisions.map { decision =>
      if (correctable(decision)) {
        val digest = digestedOf(decision, listingOf)
        correcting += digest.id
        corrected(decision, digest, version, asked, finding, correctionPosters, changedAt)
      }
      else if (decision.film.isDefined || decision.unanswered > 0 || decision.fallback.isDefined || decision.members.isEmpty) decision
      else {
        val AgreementStage.Digested(id, listings, digest, lean) = digestedOf(decision, listingOf)
        seen += id
        val verdict = verdictOf(decision, id, listings, digest, lean, version, asked, moved)
        // the venue posters' distances to the cluster's candidates and to the film the families agree on
        def distances(also: Option[Int]) = posterDistances(id, digest, listings, also, postersAsked)
        // the posters vote once the families reached a verdict that takes no film: none agreed, or a poster vetoed it
        val now = verdict.fold(decision) { v =>
          v.agreed.fold[AgreementStage.Take](AgreementStage.Take.Untaken)(taken(decision, _, finding, distances)) match {
            // a screen adaptation agreed for a listing naming a stage work ([[Agreement.agreed]]) yields to the relay its
            // screening days name: PL Kino Amok's bare "Manon" on the Met's broadcast day is the Met's, not Clouzot's
            case AgreementStage.Take.Taken(agreed) if listings.exists(Agreement.stagesAWork) => broadcast(decision, id, digest, listings, dating, version).getOrElse(agreed)
            case AgreementStage.Take.Taken(agreed) => agreed
            case AgreementStage.Take.Pending       => decision
            case AgreementStage.Take.Vetoed        => posterVetoed += 1; voted(decision, distances(None)).orElse(broadcast(decision, id, digest, listings, dating, version))
                                                        .orElse(filledTake(decision, v, finding, distances))
                                                        .orElse(catalogued(decision, listings, asked, finding, catalogueAsked)).getOrElse(decision)
            case AgreementStage.Take.Untaken       => voted(decision, distances(None)).orElse(broadcast(decision, id, digest, listings, dating, version))
                                                        .orElse(filledTake(decision, v, finding, distances))
                                                        .orElse(catalogued(decision, listings, asked, finding, catalogueAsked)).getOrElse(decision)
          }
        }
        if (now eq decision) decision else Option(takenAs.get(decision)).filter(_ == now).getOrElse { takenAs.put(decision, now); now }
      }
    }
    val removed = held.keySet.toSet -- seen
    waiting --= waiting.keySet.toSet -- seen
    candidateFilms --= candidateFilms.keySet.toSet -- seen -- correcting -- correcting.map(_ + "|far")
    corrections --= corrections.keySet.toSet -- correcting
    // the posters a correction waits on, a few at a time: the backfill of every take's posters drains behind every other task
    postersAsked ++= (if (correctionPosters.sizeIs <= correctionPostersAtOnce) correctionPosters
      else correctionPosters.toSeq.sortBy(PosterAnswers.idOf).take(correctionPostersAtOnce))
    if (removed.nonEmpty || moved.nonEmpty) {
      stored.replace(removed, moved.toSeq)
      held --= removed; checked --= removed
      moved.foreach(v => held(v.id) = v)
    }
    val current = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[ResolverDecision, java.lang.Boolean]())
    resolution.decisions.foreach(current.add)
    takenAs.keySet.retainAll(current)   // the model's decisions this resolution no longer holds
    digested.keySet.retainAll(current)
    gaps = asked.toSet
    finds = finding.toSet
    posterGaps = postersAsked.toSet
    catalogueGaps = catalogueAsked.toSet
    undated = dating.toSet
    if (handedAt != version) { handed.clear(); handedFinds.clear(); handedPosters.clear(); handedCatalogue.clear(); handedAt = version }
    val open = AgreementStage.Open(gaps -- handed, finds -- handedFinds, posterGaps -- handedPosters, catalogueGaps -- handedCatalogue,
      undated.filterNot(rereads.contains))
    if (open.questions.nonEmpty || open.finds.nonEmpty || open.posters.nonEmpty || open.catalogue.nonEmpty || open.records.nonEmpty) {
      ask(open); handed ++= open.questions; handedFinds ++= open.finds; handedPosters ++= open.posters; handedCatalogue ++= open.catalogue
      open.records.foreach(film => rereads(film) = (version, clock.instant()))
    }
    val agreedNow = decisions.filter(_.basis == ResolverDecision.Basis.Agreed)
    metrics.applied(AgreementStage.Applied(waiting = waiting.size, verdicts = held.size, agreed = held.valuesIterator.count(_.agreed.isDefined),
      takenTmdb = agreedNow.count(_.film.isDefined), takenFallback = agreedNow.count(_.fallback.isDefined),
      open = gaps.groupMapReduce(_._1)(_ => 1)(_ + _), finds = finds.size, resolves = resolves, seconds = started.seconds,
      takenPoster = decisions.count(_.basis == ResolverDecision.Basis.Poster), posterVetoed = posterVetoed, posters = posterGaps.size,
      takenBroadcast = decisions.count(_.basis == ResolverDecision.Basis.Broadcast), takenFilled = decisions.count(_.basis == ResolverDecision.Basis.Filled),
      takenCatalogue = decisions.count(_.basis == ResolverDecision.Basis.Catalogue), catalogue = catalogueGaps.size, undated = undated.size,
      withdrawn = decisions.count(_.basis == ResolverDecision.Basis.Withdrawn), corrected = decisions.count(_.basis == ResolverDecision.Basis.Corrected),
      correcting = correcting.size, correctionPosters = correctionPosters.size))
    reading = new AgreementStage.Read(posters, listedOn.flatMap(families.get))   // nothing read is held past the apply
    resolution.copy(decisions = decisions)
  }

  /** The decision's cluster id, its listings sorted and their digest, kept while the model hands the same decision over. */
  private def digestedOf(decision: ResolverDecision, listingOf: ListingKey => Option[Listing]): AgreementStage.Digested =
    Option(digested.get(decision)).getOrElse {
      val listings = decision.members.flatMap(listingOf).sortBy(_.key)(using ListingKey.ordering)
      val fresh    = AgreementStage.Digested(StoredFamily.idOf(decision.members), listings,
        AgreementStage.digest(Seq(digestOf(listings).toString, decision.leaning.toString, decision.candidate.toString,
          PosterEvidence.urls(listings).mkString("\u0001"))), voteOf(decision))
      digested.put(decision, fresh); fresh
    }

  /** A film the model took by its own rules — what [[Correction]] reads the evidence against. */
  private def correctable(decision: ResolverDecision): Boolean =
    decision.film.isDefined && decision.members.nonEmpty &&
      (decision.basis == ResolverDecision.Basis.OwnMatch || decision.basis == ResolverDecision.Basis.PooledMatch)

  /** The model's take, withdrawn or switched where the evidence against it says so ([[Correction.decide]]) — the kept
   *  correction while its listings stand and none of the answers it read was filed again, else read anew. What it waits
   *  on is noted to ask: the families' questions in `asked`, the IMDb ids in `finding`, the posters in `posters`. */
  private def corrected(decision: ResolverDecision, digested: AgreementStage.Digested, version: Long, asked: mutable.Set[(VoterFamily, String)],
                        finding: mutable.Set[String], posters: mutable.Set[AgreementStage.PosterQuestion],
                        changedAt: mutable.Map[Long, Option[java.util.Set[java.lang.Long]]]): ResolverDecision = {
    val kept = corrections.get(digested.id).filter(c => c.listings == digested.digest && (c.version == version || {
      // stands when none of its reads was filed since: the filings' ids hashed once per version a correction was kept at
      val refiled = changedAt.getOrElseUpdate(c.version, changes.changedSince(c.version).map { ids =>
        val hashed = new java.util.HashSet[java.lang.Long](ids.size * 2)
        ids.foreach(id => hashed.add(AgreementStage.digest(Seq(id))))
        hashed
      })
      refiled.exists(filed => !c.reads.exists(read => filed.contains(read))) && !c.waits.exists(_.finds.nonEmpty)
    }))
    val now = kept.map { c => c.version = version; c }.getOrElse {
      val fresh = correctionOf(decision, digested)
      corrections(digested.id) = fresh
      fresh.version = version
      fresh
    }
    now.waits.foreach { w => asked ++= w.questions; finding ++= w.finds; posters ++= w.posters }
    now.outcome.fold(decision) { outcome =>
      val explained = decision.explanation :+ outcome.line
      val taken = outcome.film.fold(decision.copy(film = None, basis = ResolverDecision.Basis.Withdrawn, explanation = explained)(decision.trace))(film =>
        decision.copy(film = Some(film), basis = ResolverDecision.Basis.Corrected, explanation = explained)(decision.trace))
      Option(takenAs.get(decision)).filter(_ == taken).getOrElse { takenAs.put(decision, taken); taken }
    }
  }

  /** The evidence against the model's take read now: the venue posters and — once one names another film — the
   *  venues' programmes and the families ([[Correction]]). A part still waiting on an answer decides nothing yet, but a
   *  part known may: a programme's contradiction withdraws the take while the families wait. */
  private def correctionOf(decision: ResolverDecision, digested: AgreementStage.Digested): AgreementStage.Corrected = {
    val listings = digested.listings
    val model    = decision.film.get
    val reads    = mutable.Map.empty[String, Long]
    val gaps     = mutable.Set.empty[(VoterFamily, String)]
    val posterQs = mutable.Set.empty[AgreementStage.PosterQuestion]
    val finding  = mutable.Set.empty[String]
    def named(id: Int) = venues.film(id).toOption.flatten.orElse(tmdb.flatMap(_.film(id).toOption.flatten))
    def recordOf(id: Int, film: IdentityMeasures.Film) =
      SourceRecord(film, Map("tmdb" -> id.toString) ++ Option.when(film.imdbNumber > 0)("imdb" -> f"tt${film.imdbNumber}%07d"))
    def title(id: Int) = named(id).fold(s"tmdb $id")(f => s"'${f.title}'${f.year.fold("")(y => s" ($y)")}")
    val outcome = named(model).flatMap { film =>
      val modelRecord = recordOf(model, film)
      // (a) a venue poster matching another candidate, not the taken film
      val vetoed = correctionPosters(digested, model, reads, posterQs).toOption.flatten
      // (e) the venues' own programmes on `listedOn`'s site, asked only of a take a poster questions: a few venues' a day
      val listed = reading.programmes.filter(_ => vetoed.isDefined).fold[Answer[Option[SourceRecord]]](Answer.Known(None))(answers =>
        VenueListings.listed(listings, new Recording(answers, gaps, reads)))
      val programme = listed.toOption.flatten.filter(record => !Agreement.sameFilm(record, modelRecord) && Correction.contradicts(record.film, film))
      // (b) the families, asked only of a take another kind of evidence questions
      // the TMDB films the take's evidence reaches besides it: what a family's or the programme's record is matched to
      lazy val reached = (vetoed.map(_._1).toSeq ++ candidatesOf(digested.id, digested.digest, listings)).distinct.filter(_ != model)
      val records = (id: Int) => named(id).map(recordOf(id, _))
      val against = if (programme.isEmpty && vetoed.isEmpty) None
        // beside the programmes, the family whose site lists them is no second kind of evidence: its take is not counted
        else familiesAgainst(listings, model, modelRecord, reached, records, programme.flatMap(_ => listedOn), reads, gaps, finding).toOption.flatten
      val evidence = programme.toSeq.map { record =>
        // the TMDB film the programme's record is: one the other evidence or the cluster's own candidates reach
        val same = (reached ++ against.map(_._1)).distinct.filter(id => records(id).exists(Agreement.sameFilm(record, _)))
        Correction.Against(Correction.Filmweb, same.headOption.filter(_ => same.sizeIs == 1),
          s"the venues' Filmweb programmes list '${record.film.title}'${record.film.year.fold("")(y => s" ($y)")} on their days")
      } ++ vetoed.toSeq.map { case (other, bits) =>
        Correction.Against(Correction.Poster, Some(other), s"a venue poster matches ${title(other)} ($bits bits), not the take")
      } ++ against.toSeq.map { case (other, says) => Correction.Against(Correction.Families, Some(other), says.replace(s"tmdb $other", title(other))) }
      val sharesDirector = (other: Int) => named(other).flatMap(o => Correction.shareDirector(o.directors.getOrElse(Nil), film.directors.getOrElse(Nil)))
      Correction.decide(title(model), evidence, sharesDirector)
    }
    val waits = Option.when(gaps.nonEmpty || posterQs.nonEmpty || finding.nonEmpty)(AgreementStage.CorrectionWaits(gaps.toSet, finding.toSet, posterQs.toSet))
    val read  = (reads.keysIterator ++ gaps.iterator.map { case (family, question) => s"${family.label}|$question" } ++
      posterQs.iterator.map(PosterAnswers.idOf)).map(id => AgreementStage.digest(Seq(id))).toArray
    new AgreementStage.Corrected(digested.digest, read, outcome, waits)
  }

  /** The candidate a venue poster matches against the model's take ([[PosterEvidence.veto]]) — read first against the
   *  take's own posters: a venue poster within [[PosterEvidence.VetoBits]] of one of them vetoes nothing, so the other
   *  candidates' posters are hashed only for a take none of them comes near. `Unknown` while a poster is not hashed. */
  private def correctionPosters(digested: AgreementStage.Digested, model: Int, reads: mutable.Map[String, Long],
                                asked: mutable.Set[AgreementStage.PosterQuestion]): Answer[Option[(Int, Int)]] = {
    val urls = PosterEvidence.urls(digested.listings)
    if (urls.isEmpty || tmdb.isEmpty) Answer.Known(None)
    else {
      def read[A](question: AgreementStage.PosterQuestion, answer: Answer[A]): Answer[A] = {
        reads(PosterAnswers.idOf(question)) = 0L
        if (answer == Answer.Unknown) asked += question
        answer
      }
      val venue = urls.map(url => url -> read(AgreementStage.PosterQuestion.Venue(url), reading.posters.venue(url)))
      val own   = read(AgreementStage.PosterQuestion.Film(model), reading.posters.film(model))
      if (venue.exists(_._2 == Answer.Unknown) || own == Answer.Unknown) Answer.Unknown
      else {
        val held = own.toOption.getOrElse(Nil)
        // the venue posters none of the take's own comes near: only they can name another film against it
        val far  = venue.collect { case (url, Answer.Known(Some(poster))) if !PosterEvidence.nearest(Seq(poster), held).exists(_ <= PosterEvidence.VetoBits) => url }.toSet
        if (far.isEmpty) Answer.Known(None)
        else {
          // the films one listing showing each such poster is searched by: a cluster billed at hundreds of venues is not
          // searched again whole for the few posters that might veto its take
          val showing = digested.listings.filter(listing => PosterEvidence.shows(listing) && listing.poster.exists(far)).distinctBy(_.poster)
          val id      = s"${digested.id}|far"
          titledFilms(id, digested.digest, showing).foreach(film => reads(PosterAnswers.idOf(AgreementStage.PosterQuestion.Film(film))) = 0L)
          posterDistances(id, digested.digest, showing, Some(model), asked, titledFilms) match {
            case Answer.Known(found) => Answer.Known(PosterEvidence.veto(Some(model), found))
            case Answer.Unknown      => Answer.Unknown
          }
        }
      }
    }
  }

  /** The film more of the families take than take the model's, by its TMDB id — a pick's TMDB or IMDb id, else the one
   *  film of `reached` its record is by its facts — with what they say, `uncounted`'s take aside; `Unknown` while a
   *  family's question, or TMDB's find of a taken IMDb id, is not answered yet. */
  private def familiesAgainst(listings: Seq[Listing], model: Int, modelRecord: SourceRecord, reached: => Seq[Int], records: Int => Option[SourceRecord],
                              uncounted: Option[VoterFamily], reads: mutable.Map[String, Long], gaps: mutable.Set[(VoterFamily, String)],
                              finding: mutable.Set[String]): Answer[Option[(Int, String)]] = {
    val verdicts = families.toSeq.sortBy(_._1.ordinal).map { case (_, answers) =>
      Agreement.verdict(listings, new Recording(answers, gaps, reads), venues, normalizer, calibrations(answers.family))
    }
    if (verdicts.contains(Answer.Unknown)) Answer.Unknown
    else {
      val mapped = verdicts.flatMap(_.toOption).flatMap(_.pick).filterNot(pick => uncounted.contains(pick.family)).map { pick =>
        val id: Answer[Option[Int]] = pick.record.crossIds.get("tmdb").flatMap(_.toIntOption) match {
          case Some(film) => Answer.Known(Some(film))
          case None       => pick.record.crossIds.get("imdb").fold[Answer[Option[Int]]](Answer.Known(None)) { imdb =>
            val found = tmdbOf(imdb)
            if (found == Answer.Unknown) finding += imdb
            found
          }
        }
        // a record linking no TMDB film (Filmweb's) is the one film the take's evidence reaches that its facts are
        pick -> (id match {
          case Answer.Known(None) => Answer.Known(reached.filter(film => records(film).exists(Agreement.sameFilm(pick.record, _))) match {
            case Seq(one) => Some(one)
            case _        => None
          })
          case found => found
        })
      }
      if (mapped.exists(_._2 == Answer.Unknown)) Answer.Unknown
      else {
        val forModel = mapped.collect { case (pick, id) if id.toOption.flatten.contains(model) || Agreement.sameFilm(pick.record, modelRecord) => pick.family }
        val others   = mapped.collect { case (pick, Answer.Known(Some(id))) if id != model && !forModel.contains(pick.family) => id -> pick.family }
          .groupMap(_._1)(_._2).toSeq.sortBy { case (id, takers) => (-takers.size, id) }
        Answer.Known(others.headOption.filter { case (_, takers) => takers.size > forModel.size && !others.drop(1).exists(_._2.size == takers.size) }
          .map { case (id, takers) =>
            id -> (s"${takers.map(_.label).sorted.mkString(", ")} take tmdb $id" +
              (if (forModel.isEmpty) ", none the take" else s", ${forModel.map(_.label).sorted.mkString(", ")} the take"))
          })
      }
    }
  }

  /** TMDB's own vote on a no-match: the film it leans to, else the best-ranked candidate it weighed — its record as the
   *  model read it, so it links to a family's pick by facts as well as by ids. */
  private def voteOf(decision: ResolverDecision): Option[SourceRecord] =
    decision.leaning.orElse(decision.candidate).map { vote =>
      val ids = AgreementStage.leanRecord(vote)
      venues.film(vote.film).toOption.flatten.fold(ids)(film => SourceRecord(film,
        ids.crossIds ++ Option.when(film.imdbNumber > 0)("imdb" -> f"tt${film.imdbNumber}%07d")))
    }

  /** The listings as published, and the venue detail page the picks read of each. */
  private def digestOf(listings: Seq[Listing]): Long =
    AgreementStage.digest(listings.map(l => l.sortKey + l.broadcastSeason.fold("")(season => s"\u0002$season") +
      (if (venues.hasDetail(l)) "\u0001" + venues.detail(l) else "")))

  /** The cluster's verdict: the stored one while its listings and every answer it read stand, else the resolver's
   *  over the families' answers now — `None` while one of them is a gap. */
  private def verdictOf(decision: ResolverDecision, id: String, listings: Seq[Listing], digest: Long, lean: Option[SourceRecord], version: Long,
                        asked: mutable.Set[(VoterFamily, String)], moved: mutable.ArrayBuffer[StoredVerdict]): Option[StoredVerdict] = {
    held.get(id).filter(v => v.listings == digest && v.rules == decidedUnder).filter { kept =>
      checked.get(id).contains((digest, version)) || untouched(id, kept, digest, version) || {
        val stands = kept.reads.forall { case (question, answered) => read(question).exists(answer => AgreementStage.digest(Seq(answer.toString)) == answered) }
        // read again because an answer was filed: a stale one among its reads is asked again too — a quiet tick reads none
        if (stands) { checked(id) = (digest, version); asked ++= kept.reads.keys.flatMap(staleOf) }
        stands
      }
    }
    .orElse(waiting.get(id).filter(w => w.listings == digest && !due(w)) match {
      case Some(w) => asked ++= w.gaps; None   // its questions not all answered yet: a resolve now could reach no verdict
      case None               => resolved(decision, id, digest, listings, lean, version, asked, moved)
    })
  }

  /** Does a kept verdict still stand at `version` without reading its answers — checked at an earlier version, none of
   *  the answers it read filed again since ([[AnswerChanges]])? A filing elsewhere re-reads no other verdict. */
  private def untouched(id: String, kept: StoredVerdict, digest: Long, version: Long): Boolean =
    checked.get(id).collect { case (listed, at) if listed == digest => at }
      .flatMap(changes.changedSince).exists(refiled => !kept.reads.keysIterator.exists(refiled)) && {
      checked(id) = (digest, version); true
    }

  /** The cluster's verdict, the resolver asked again over every family's answers now — `None`, and the cluster kept
   *  waiting on its questions, while one is a gap. */
  private def resolved(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], lean: Option[SourceRecord], version: Long,
                       asked: mutable.Set[(VoterFamily, String)], moved: mutable.ArrayBuffer[StoredVerdict]): Option[StoredVerdict] = {
    resolves += 1
    val reads = mutable.Map.empty[String, Long]
    val gaps  = mutable.Set.empty[(VoterFamily, String)]
    val verdicts = families.toSeq.sortBy(_._1.ordinal).map { case (_, answers) =>
      Agreement.verdict(listings, new Recording(answers, gaps, reads), venues, normalizer, calibrations(answers.family))
    }
    asked ++= gaps
    if (verdicts.contains(Answer.Unknown)) { waiting(id) = AgreementStage.Waiting(digest, gaps.toSet, clock.instant()); None }
    else {
      waiting -= id
      Some {
        val thisYear = java.time.LocalDate.ofInstant(clock.instant(), java.time.ZoneOffset.UTC).getYear
        val known    = verdicts.flatMap(_.toOption)
        val stated   = listings.map(asStated)
        val agreed   = Agreement.agreed(listings, known, lean, Some(thisYear), stated)
        val verdict  = StoredVerdict(id, digest, reads.toMap, agreed, decidedUnder,
          if (agreed.isDefined) None else filledOf(decision, listings, known, stated, thisYear))
        if (!held.get(id).contains(verdict)) moved += verdict
        checked(id) = (digest, version)
        verdict
      }
    }
  }

  /** The listing with the year, directors, running time, original title and countries its venue's own page states where
   *  the listing does not ([[services.identity.Evidence.of]], as the model reads it): what the agreement's listing votes
   *  and title check read, never what rules a film out ([[Agreement.agreed]]). PL "The Taxidermist | Splat!FilmFest" bills no director; its page credits Paulo Nascimento. */
  private def asStated(listing: Listing): Listing =
    if (!venues.hasDetail(listing)) listing
    else venues.detail(listing).toOption.flatten.fold(listing) { page =>
      val stated = services.identity.Evidence.of(listing, Some(page))
      listing.copy(year = stated.year, directors = stated.directors, runtime = stated.runtime, originalTitle = stated.originalTitle,
        countries = stated.countries)
    }

  /** The film the first selected FILL rule takes for a cluster the families agreed on nothing for
   *  ([[services.identity.UnifiedRules.filled]] over [[services.identity.UnifiedEvidence.contenders]]: the cluster's own
   *  TMDB candidates, none denied, and the films the families took or lean to) — none while a TMDB answer the cluster's
   *  candidates need is a gap. The venue posters' veto is read when it is taken ([[filledTake]]). */
  private def filledOf(decision: ResolverDecision, listings: Seq[Listing], verdicts: Seq[FamilyVerdict], stated: Seq[Listing],
                       thisYear: Int): Option[StoredFill] =
    tmdb.filter(_ => rules.fill.nonEmpty).flatMap { lookups =>
      val noting = new AgreementStage.UnknownNoting(lookups)
      val nodes  = IdentityResolver.evidenceOf(listings, noting, normalizer, calibration)(_ => true)
      Option.when(!noting.unknown) {
        val evidence = services.identity.UnifiedEvidence.ClusterEvidence(listings, decision, nodes, verdicts, Nil, _ => None, thisYear, stated,
          Some(listing => Evidence.of(listing, venues.detail(listing).toOption.flatten).measured))
        rules.filled(services.identity.UnifiedEvidence.contenders(evidence)).map { case (film, rule) =>
          StoredFill(rule, film.tmdb, film.imdb, rules.explain(film, rule)) }
      }.flatten
    }

  /** The decision with the film a fill rule took — pending while TMDB was not asked about its IMDb id yet or the venue
   *  posters are not all hashed, and not taken when a venue poster names another candidate ([[PosterEvidence.veto]]). */
  private def filledTake(decision: ResolverDecision, verdict: StoredVerdict, finding: mutable.Set[String],
                         distances: Option[Int] => Answer[Seq[Map[Int, Option[Int]]]]): Option[ResolverDecision] =
    verdict.filled.flatMap { fill =>
      val film: Answer[Option[Int]] = fill.tmdb.fold(fill.imdb.fold[Answer[Option[Int]]](Answer.Known(None))(tmdbOf))(id => Answer.Known(Some(id)))
      film match {
        case Answer.Unknown => fill.imdb.foreach(finding += _); None
        case Answer.Known(tmdbFilm) => distances(tmdbFilm).toOption.filter(found => PosterEvidence.veto(tmdbFilm, found).isEmpty).flatMap { _ =>
          val explained = decision.explanation :+ fill.line
          tmdbFilm.map(id => decision.copy(film = Some(id), basis = ResolverDecision.Basis.Filled, explanation = explained)(decision.trace))
            .orElse(fill.imdb.map(id => decision.copy(basis = ResolverDecision.Basis.Filled, explanation = explained,
              fallback = Some(ResolverDecision.Fallback(VoterFamily.Imdb.database, id, 1.0)))(decision.trace)))
        }
      }
    }

  /** Has the family answered `question` since — any answer, fresh? Read per waiting cluster, per tick: an answer filed
   *  for another cluster's question resolves none of the rest again (prod PL 2026-10-04: re-resolving every waiting
   *  cluster on each filing ran a 24 s, 540 MB agreement phase back to back). */
  private def answered(gap: (VoterFamily, String)): Boolean = families.get(gap._1).exists(_.fresh(gap._2))

  /** Is a waiting cluster due a resolve? Once every question it waits on is answered — any gap left, and the families
   *  could reach no verdict, so resolving on each answer as a backlog drains (every cluster gets one, every tick) only
   *  re-ran the same partial resolve: prod PL 2026-10-04, 25–48 s an agreement phase after the per-question fix — or once
   *  some are and it has waited [[AgreementStage.PartialAfter]], so a question never answered holds no cluster for ever. */
  private def due(waiting: AgreementStage.Waiting): Boolean =
    waiting.gaps.forall(answered) ||
      (!clock.instant().isBefore(waiting.since.plusMillis(AgreementStage.PartialAfter.toMillis)) && waiting.gaps.exists(answered))

  /** A stored read whose answer is stale, as the question to ask again. */
  private def staleOf(question: String): Option[(VoterFamily, String)] = question.split("\\|", 2) match {
    case Array(label, asked) => families.collectFirst { case (family, answers) if family.label == label && !answers.fresh(asked) => family -> asked }
    case _                   => None
  }

  /** A stored read's answer now: `<family>|<kind>|<text>`, as [[Recording]] notes it. */
  private def read(question: String): Option[Answer[?]] = question.split("\\|", 3) match {
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

  /** The decision with the agreed film taken — pending while TMDB was not asked about its IMDb id yet or the venue posters
   *  are not all hashed, and not taken when they VETO it ([[PosterEvidence.veto]]): a venue poster names another candidate. */
  private def taken(decision: ResolverDecision, agreed: AgreedFilm, finding: mutable.Set[String],
                    distances: Option[Int] => Answer[Seq[Map[Int, Option[Int]]]]): AgreementStage.Take = {
    val imdb = agreed.crossId("imdb")
    val tmdb: Answer[Option[Int]] = agreed.crossId("tmdb").flatMap(_.toIntOption) match {
      case Some(film) => Answer.Known(Some(film))
      case None       => imdb.fold[Answer[Option[Int]]](Answer.Known(None))(tmdbOf)
    }
    val leaning = Option.when(agreed.leaning.nonEmpty)(s", ${agreed.leaning.toSeq.map(_.label).sorted.mkString(", ")} leaning to it").getOrElse("") +
      Option.when(agreed.corroborated.nonEmpty)(s", corroborated by ${agreed.corroborated.toSeq.sorted.mkString(", ")}").getOrElse("")
    val line = s"${agreed.families.toSeq.map(_.label).sorted.mkString(", ")}$leaning agree on '${agreed.record.film.title}'" +
      agreed.record.film.year.fold("")(year => s" ($year)") + imdb.fold("")(id => s" $id")
    val ids  = agreed.ids.map { case (family, id) => family.label -> id }
    def unlessVetoed(film: Option[Int])(take: => ResolverDecision): AgreementStage.Take = distances(film) match {
      case Answer.Known(found) => if (PosterEvidence.veto(film, found).isEmpty) AgreementStage.Take.Taken(take) else AgreementStage.Take.Vetoed
      case Answer.Unknown      => AgreementStage.Take.Pending
    }
    // a film only review sites take — their pages name no film a card stands on, and match an event's namesake (DE "André
    // Rieus Weihnachtskonzert 2026: Let it Snow": Metacritic's and RT's 2019 "Let It Snow", the model's best candidate)
    if (!agreed.families.exists(_.namesFilms)) AgreementStage.Take.Untaken
    else tmdb match {
      case Answer.Unknown =>
        imdb.foreach(finding += _)
        AgreementStage.Take.Pending
      case Answer.Known(Some(film)) => unlessVetoed(Some(film))(
        decision.copy(film = Some(film), basis = ResolverDecision.Basis.Agreed, explanation = decision.explanation :+ line, agreed = ids)(decision.trace))
      case Answer.Known(None) => identityOf(agreed).fold[AgreementStage.Take](AgreementStage.Take.Untaken) { case (source, id) =>
        // a film standing on another database's id carries its title and year, that a link to its page is built of
        val named = Option.when(source != VoterFamily.Imdb.database)(agreed.record.film)
        unlessVetoed(None)(decision.copy(basis = ResolverDecision.Basis.Agreed, explanation = decision.explanation :+ line,
          fallback = Some(ResolverDecision.Fallback(source, id, 1.0, named.map(_.title), named.flatMap(_.year))), agreed = ids)(decision.trace)) }
    }
  }

  /** The id a film TMDB holds no record of stands on: its IMDb id, else the own id of the first of [[identities]] that
   *  took it or whose id an agreeing record links — as `(source, id)`, the source a fallback film names. */
  private def identityOf(agreed: AgreedFilm): Option[(String, String)] =
    (VoterFamily.Imdb +: identities).distinct.iterator.flatMap(family =>
      agreed.ids.get(family).orElse(agreed.crossId(family.database)).map(family.database -> _)).nextOption()

  /** The decision with the film its listings' catalogue ids name taken ([[Catalogue.take]]) — none while a question it
   *  needs is open, each noted to ask. */
  private def catalogued(decision: ResolverDecision, listings: Seq[Listing], asked: mutable.Set[(VoterFamily, String)], finding: mutable.Set[String],
                         catalogueAsked: mutable.Set[CatalogueQuestion]): Option[ResolverDecision] =
    Catalogue.take(listings, listings.map(asStated), catalogue, families, tmdbOf, catalogueAsked, asked, finding).toOption.flatten.map { taken =>
      val explained = decision.explanation :+ taken.line
      taken.film.fold(decision.copy(basis = ResolverDecision.Basis.Catalogue, explanation = explained,
        fallback = taken.fallback.map { case (source, id) =>
          // a film standing on Wikidata's item carries its title and year, that a link to its page is built of
          val named = source != VoterFamily.Imdb.database
          ResolverDecision.Fallback(source, id, 1.0, taken.title.filter(_ => named), taken.year.filter(_ => named)) })(decision.trace))(film =>
        decision.copy(film = Some(film), basis = ResolverDecision.Basis.Catalogue, explanation = explained)(decision.trace))
    }

  /** The decision with the one candidate the venue posters match taken ([[PosterEvidence.vote]]). */
  private def voted(decision: ResolverDecision, distances: Answer[Seq[Map[Int, Option[Int]]]]): Option[ResolverDecision] =
    distances.toOption.flatMap(PosterEvidence.vote).map { case (film, bits) =>
      val named = tmdb.flatMap(_.film(film).toOption.flatten).fold(s"tmdb $film")(f => s"'${f.title}'${f.year.fold("")(year => s" ($year)")}")
      decision.copy(film = Some(film), basis = ResolverDecision.Basis.Poster,
        explanation = decision.explanation :+ s"the venue's poster matches $named ($bits bits), no other candidate's within ${PosterEvidence.VoteBits}")(
        decision.trace)
    }

  /** The decision with the one record the cluster's screening days name taken ([[Broadcast]]): of the TMDB films its own
   *  evidence reaches, none denied — none while one of their records is not known yet, nor while a record of the billed
   *  work states no day only because the store filed it with its year alone: that record is noted in `dating`, to be
   *  read again ([[wantedRecords]]). */
  private def broadcast(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing], dating: mutable.Set[Int],
                        version: Long): Option[ResolverDecision] =
    tmdb.filter(_ => listings.exists(!_.screenings.isEmpty)).flatMap { lookups =>
      val records = candidatesOf(id, digest, listings).map(film => film -> lookups.film(film))
      Option.when(records.forall(_._2.isKnown))(records.flatMap { case (film, record) => record.toOption.flatten.map(film -> _) })
        .flatMap { known =>
          val measured = (listing: Listing) => Evidence.of(listing, venues.detail(listing).toOption.flatten).measured
          // a record of the billed work holding only its year is read again before the day decides: it might be the one
          Broadcast.takeOrWait(listings, measured, known)(film => !lookups.releaseDay(film).isKnown && waitsOn(film, version))
            .left.map(dating ++= _).toOption.flatten
        }
        .map(taken => decision.copy(film = Some(taken.film), basis = ResolverDecision.Basis.Broadcast,
          explanation = decision.explanation :+ taken.line)(decision.trace))
    }

  /** Does the take still wait on a record's day: never asked for again, or asked and its read neither filed since
   *  (`changes`; none it can tell of counts as filed) nor overdue? A record TMDB still dates by its year once read again
   *  is taken as dating none: one read, never a loop. */
  private def waitsOn(film: Int, version: Long): Boolean = rereads.get(film).forall { case (askedAt, since) =>
    !changes.changedSince(askedAt).forall(_(AgreementStage.recordReadId(film))) &&
      clock.instant().isBefore(since.plusMillis(AgreementStage.RecordWait.toMillis))
  }

  /** The nearest each of the cluster's candidates (and `also`, the film the families agree on) comes to each of its
   *  venue posters — none where no listing shows one; `Unknown`, each poster not hashed yet noted in `asked`, while one
   *  is not. A candidate numbering another edition than a listing showing a poster is not compared
   *  ([[PosterEvidence.editionsApart]]). */
  private def posterDistances(id: String, digest: Long, listings: Seq[Listing], also: Option[Int],
                              asked: mutable.Set[AgreementStage.PosterQuestion],
                              candidates: (String, Long, Seq[Listing]) => Seq[Int] = candidatesOf): Answer[Seq[Map[Int, Option[Int]]]] = {
    val urls = PosterEvidence.urls(listings)
    if (urls.isEmpty || tmdb.isEmpty) Answer.Known(Nil)
    else {
      val venue = urls.map(url => url -> reading.posters.venue(url))
      venue.collect { case (url, Answer.Unknown) => asked += AgreementStage.PosterQuestion.Venue(url) }
      if (venue.exists(_._2 == Answer.Unknown)) Answer.Unknown
      else {
        val shown: Seq[PosterHash] = venue.flatMap(_._2.toOption.flatten)
        if (shown.isEmpty) Answer.Known(Nil)
        else {
          val showing = listings.filter(listing => PosterEvidence.shows(listing) && listing.poster.isDefined)
          def apart(film: Int) = tmdb.flatMap(_.film(film).toOption.flatten).exists(record => showing.exists(PosterEvidence.editionsApart(_, record)))
          val films  = (candidates(id, digest, listings).filterNot(apart) ++ also.filterNot(FallbackIds.isFallback)).distinct
          val hashes = films.map(film => film -> reading.posters.film(film))
          hashes.collect { case (film, Answer.Unknown) => asked += AgreementStage.PosterQuestion.Film(film) }
          if (hashes.exists(_._2 == Answer.Unknown)) Answer.Unknown
          else Answer.Known(shown.map(poster => hashes.map { case (film, answer) => film -> PosterEvidence.nearest(Seq(poster), answer.toOption.getOrElse(Nil)) }.toMap))
        }
      }
    }
  }

  /** The TMDB films the cluster's own evidence reaches, none denied — read once per listings' digest; none while a TMDB
   *  question of theirs has no answer. */
  private def candidatesOf(id: String, digest: Long, listings: Seq[Listing]): Seq[Int] =
    candidateFilms.get(id).filter(_._1 == digest).map(_._2).getOrElse {
      val films = tmdb.fold(Seq.empty[Int]) { lookups =>
        val noting = new AgreementStage.UnknownNoting(lookups)
        val found  = IdentityResolver.candidatesOf(listings, noting, normalizer, calibration)(_ => true)
          .flatMap(_.candidates.filterNot(_.denied).map(_.tmdbId)).filterNot(FallbackIds.isFallback).distinct.sorted
        if (noting.unknown) Nil else found
      }
      candidateFilms(id) = (digest, films)
      films
    }

  /** The TMDB films the listings' own title searches find ([[services.identity.IdentityMeasures.searchQueries]], of TMDB
   *  and IMDb's titled lists) — the candidates a poster may name against a model take, read off the answers the model
   *  already holds without scoring them again: a resolver pass per take cost a cold projection 10 GB of allocation over
   *  the five corpora (2026-10-06). Read once per listings' digest; none while one of the searches has no answer. */
  private def titledFilms(id: String, digest: Long, listings: Seq[Listing]): Seq[Int] =
    candidateFilms.get(id).filter(_._1 == digest).map(_._2).getOrElse {
      val films = tmdb.fold(Seq.empty[Int]) { lookups =>
        val queries = listings.flatMap(listing => services.identity.IdentityMeasures.searchQueries(Evidence.of(listing, None).measured)).distinct
          .flatMap(text => Seq(CandidateQuery.Title(text), CandidateQuery.ImdbTitled(text)))
        val found = queries.map(lookups.candidates)
        if (found.contains(Answer.Unknown)) Nil
        else found.flatMap(_.toOption.getOrElse(Nil)).map(_.tmdbId).filterNot(FallbackIds.isFallback).distinct.sorted
      }
      candidateFilms(id) = (digest, films)
      films
    }

  /** A family's answers that note every question still a gap, and the digest of every answer read. */
  private final class Recording(answers: FamilyAnswers, asked: mutable.Set[(VoterFamily, String)], reads: mutable.Map[String, Long]) extends FamilyAnswers {
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
  /** 64 bits of `parts`' text, two MurmurHash3 seeds: stable across runs, as a stored digest must be. */
  def digest(parts: Seq[String]): Long = {
    val text = parts.mkString("\u0000")
    (scala.util.hashing.MurmurHash3.stringHash(text, 0x2f1d7a3b).toLong << 32) | (scala.util.hashing.MurmurHash3.stringHash(text, 0x6c8e9cf5).toLong & 0xffffffffL)
  }

  /** The questions an [[AgreementStage.apply]] met unanswered or stale, the agreed IMDb ids TMDB was not asked about, the
   *  posters not hashed yet, the catalogue questions, and the TMDB records to read again for their release day
   *  ([[wantedRecords]]). */
  final case class Open(questions: Set[(VoterFamily, String)], finds: Set[String], posters: Set[PosterQuestion] = Set.empty,
                        catalogue: Set[CatalogueQuestion] = Set.empty, records: Set[Int] = Set.empty)

  /** What a TMDB record read again for its day is filed as among the answers' changes ([[AnswerChanges]]). */
  def recordReadId(tmdbId: Int): String = s"tmdb|record|$tmdbId"
  /** How long the broadcast take waits on a record's read at most — the agreement's tasks run after every other. */
  val RecordWait: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(1, java.util.concurrent.TimeUnit.DAYS)

  /** What became of the film a cluster's families agree on: taken, pending an answer, vetoed by a venue poster, or none to take. */
  private enum Take {
    case Taken(decision: ResolverDecision)
    case Pending
    case Vetoed
    case Untaken
  }

  /** A poster to hash: a venue's, by its URL, or a TMDB film's. */
  enum PosterQuestion {
    case Venue(url: String)
    case Film(tmdbId: Int)
  }

  /** `inner`, noting whether any answer it gave was `Unknown`. */
  private[agreement] final class UnknownNoting(inner: IdentityLookups) extends IdentityLookups {
    @volatile var unknown = false
    private def noted[A](answer: Answer[A]): Answer[A] = { if (answer == Answer.Unknown) unknown = true; answer }
    def hasDetail(listing: Listing): Boolean                     = inner.hasDetail(listing)
    def detail(listing: Listing): Answer[Option[DetailFacts]]    = noted(inner.detail(listing))
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit = inner.prefetch(queries, films, details)
    override def prefetchAnswered(): Unit                        = inner.prefetchAnswered()
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]      = noted(inner.candidates(query))
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = noted(inner.film(tmdbId))
  }

  /** A model take's correction ([[Correction]]): its listings' digest and the `version` it was read (or last found
   *  standing) at, the digests of the answers it read — a filing of one decides it again — what it came to, and the
   *  questions it waits on. */
  private final class Corrected(val listings: Long, val reads: Array[Long], val outcome: Option[Correction.Outcome], val waits: Option[CorrectionWaits]) {
    @volatile var version: Long = -1L
  }
  private final case class CorrectionWaits(questions: Set[(VoterFamily, String)], finds: Set[String], posters: Set[PosterQuestion])
  /** How many of the posters the model takes' corrections wait on are handed to the queue at once: a one-off backfill of
   *  every take's posters (17.6k over the five corpora, 2026-10-06) drained a few at a time behind every other task. */
  val CorrectionPostersAtOnce = 64

  /** `posters` and `programmes` (the family whose site lists the venues' programmes), each answer decoded once: an
   *  apply's reads, dropped with it. */
  private final class Read(hashes: PosterAnswers, family: Option[FamilyAnswers]) {
    private val venues = mutable.HashMap.empty[String, Answer[Option[PosterHash]]]
    private val films  = mutable.HashMap.empty[Int, Answer[Seq[PosterHash]]]
    val posters: PosterAnswers = new PosterAnswers {
      def venue(url: String): Answer[Option[PosterHash]] = venues.getOrElseUpdate(url, hashes.venue(url))
      def film(tmdbId: Int): Answer[Seq[PosterHash]]     = films.getOrElseUpdate(tmdbId, hashes.film(tmdbId))
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
  private final case class Digested(id: String, listings: Seq[Listing], digest: Long, lean: Option[SourceRecord])

  /** The TMDB film a no-match leans to, as the agreement links it: by its TMDB and IMDb ids alone. */
  def leanRecord(lean: ResolverDecision.Leaning): SourceRecord =
    SourceRecord(services.identity.IdentityMeasures.Film(""), Map("tmdb" -> lean.film.toString) ++
      Option.when(lean.imdbNumber > 0)("imdb" -> f"tt${lean.imdbNumber}%07d"))

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
                           withdrawn: Int = 0, corrected: Int = 0, correcting: Int = 0, correctionPosters: Int = 0)
  trait Metrics { def applied(applied: Applied): Unit }
  object Metrics { val Silent: Metrics = _ => () }

  /** A cluster waiting on families' answers: its listings' digest, the questions it waits on, and since when. */
  private final case class Waiting(listings: Long, gaps: Set[(VoterFamily, String)], since: java.time.Instant)
  /** How long a cluster with some of its questions answered waits for the rest before it is resolved on what came. */
  val PartialAfter: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(10, java.util.concurrent.TimeUnit.MINUTES)

  /** The rules the stage decides by: a digest of its code and all it reaches (`agreement-rules-version.txt`, generated by
   *  `build.sbt` from `IdentityRulesSources.AgreementRoots`). A stored verdict decided under others is decided again. */
  lazy val RulesVersion: String =
    Option(getClass.getResourceAsStream("/agreement-rules-version.txt")).fold("unknown") { stream =>
      try new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8).trim finally stream.close()
    }
}
