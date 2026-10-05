package services.identity.agreement

import services.identity.{Answer, CandidateQuery, DetailFacts, FallbackIds, Hit, IdentityCalibration, IdentityLookups, IdentityMeasures, IdentityResolver,
  Listing, PosterAnswers, PosterEvidence, PosterHash, Resolution, ResolverDecision, StoredFamily}
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
 * an ordinary TMDB match the projection fetches details for; one only other databases hold is its IMDb fallback.
 * Every question a family could not answer yet is a gap — and so is an agreed IMDb id TMDB was not asked about yet:
 * the cluster stays as the model left it, and the stage hands the questions to `ask` (the queue) the moment it meets
 * them, with every stale answer it read — so a question is asked whenever an answer could be used: the projection runs
 * whenever the listings' facts move, and again once an answer it asked for is filed.
 *
 * Each cluster's verdict is kept in `stored` ([[AgreementVerdicts]]) with the digest of its listings and the film the model
 * leans to (which may complete an agreement), and of every answer it read, as the model keeps its families: it stands,
 * across restarts too, while none of those moved, and the resolver is asked again only for a cluster whose own listings,
 * lean or answers did. `version` (how many answers the
 * families filed) spares re-reading a standing verdict's answers while nothing was filed at all.
 *
 * The cluster's venue POSTERS ([[PosterEvidence]]) are read last, against the TMDB films the cluster's own evidence
 * reaches (`tmdb`'s candidates, none of them denied): a film the families agree on is not taken when a venue poster
 * matches another candidate and not it (the VETO), and a cluster nothing took takes the one candidate a venue poster
 * matches (the VOTE, [[ResolverDecision.Basis.Poster]]). A poster not hashed yet is a gap like a family's question:
 * the cluster waits as the model left it, and the poster is handed to `ask`. `version` must count the posters filed too.
 */
final class AgreementStage(families: Map[VoterFamily, FamilyAnswers], venues: IdentityLookups, normalizer: TitleNormalizer,
                           calibration: IdentityCalibration, tmdbOf: String => Answer[Option[Int]], stored: AgreementVerdicts,
                           ask: AgreementStage.Open => Unit = _ => (), metrics: AgreementStage.Metrics = AgreementStage.Metrics.Silent,
                           clock: java.time.Clock, changes: AnswerChanges = AnswerChanges.Unknown, posters: PosterAnswers = PosterAnswers.Silent,
                           tmdb: Option[IdentityLookups] = None) {

  @volatile private var gaps: Set[(VoterFamily, String)] = Set.empty
  @volatile private var finds: Set[String] = Set.empty
  @volatile private var posterGaps: Set[AgreementStage.PosterQuestion] = Set.empty
  /** The clusters' family questions no family has answered yet, as the last [[apply]] met them. */
  def wanted: Set[(VoterFamily, String)] = gaps
  /** The agreed IMDb ids TMDB was not asked about yet, as the last [[apply]] met them. */
  def wantedFinds: Set[String] = finds
  /** The posters not hashed yet that a cluster's take waits on, as the last [[apply]] met them. */
  def wantedPosters: Set[AgreementStage.PosterQuestion] = posterGaps

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
  /** Each cluster's TMDB candidates, none denied, at its listings' digest: what its posters are compared against. */
  private val candidateFilms = TrieMap.empty[String, (Long, Seq[Int])]
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

  private def applied(resolution: Resolution, listingOf: ListingKey => Option[Listing], version: Long): Resolution = {
    if (loadedAt.isEmpty) {   // first pass: every verdict kept under these rules stands at this version
      loadedAt = Some(version)
      held.valuesIterator.filter(_.rules == AgreementStage.RulesVersion).foreach(v => checked(v.id) = (v.listings, version))
    }
    val started = tools.Stopwatch.start()
    resolves = 0
    val asked  = mutable.Set.empty[(VoterFamily, String)]
    val finding = mutable.Set.empty[String]
    val postersAsked = mutable.Set.empty[AgreementStage.PosterQuestion]
    val moved  = mutable.ArrayBuffer.empty[StoredVerdict]
    val seen   = mutable.Set.empty[String]
    val decisions = resolution.decisions.map { decision =>
      if (decision.film.isDefined || decision.unanswered > 0 || decision.fallback.isDefined || decision.members.isEmpty) decision
      else {
        val AgreementStage.Digested(id, listings, digest, lean) = Option(digested.get(decision)).getOrElse {
          val listings = decision.members.flatMap(listingOf).sortBy(_.key)(using ListingKey.ordering)
          val fresh    = AgreementStage.Digested(StoredFamily.idOf(decision.members), listings,
            AgreementStage.digest(Seq(digestOf(listings).toString, decision.leaning.toString, PosterEvidence.urls(listings).mkString("\u0001"))),
            decision.leaning.map(AgreementStage.leanRecord))
          digested.put(decision, fresh); fresh
        }
        seen += id
        val verdict = verdictOf(id, listings, digest, lean, version, asked, moved)
        // the venue posters' distances to the cluster's candidates and to the film the families agree on
        def distances(also: Option[Int]) = posterDistances(id, digest, listings, also, postersAsked)
        // the posters vote once the families reached a verdict that takes no film: none agreed, or a poster vetoed it
        val now = verdict.fold(decision) { v =>
          v.agreed.fold[AgreementStage.Take](AgreementStage.Take.Vetoed)(taken(decision, _, finding, distances)) match {
            case AgreementStage.Take.Taken(agreed) => agreed
            case AgreementStage.Take.Pending       => decision
            case AgreementStage.Take.Vetoed        => voted(decision, distances(None)).getOrElse(decision)
          }
        }
        if (now eq decision) decision else Option(takenAs.get(decision)).filter(_ == now).getOrElse { takenAs.put(decision, now); now }
      }
    }
    val removed = held.keySet.toSet -- seen
    waiting --= waiting.keySet.toSet -- seen
    candidateFilms --= candidateFilms.keySet.toSet -- seen
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
    if (handedAt != version) { handed.clear(); handedFinds.clear(); handedPosters.clear(); handedAt = version }
    val open = AgreementStage.Open(gaps -- handed, finds -- handedFinds, posterGaps -- handedPosters)
    if (open.questions.nonEmpty || open.finds.nonEmpty || open.posters.nonEmpty) {
      ask(open); handed ++= open.questions; handedFinds ++= open.finds; handedPosters ++= open.posters
    }
    val agreedNow = decisions.filter(_.basis == ResolverDecision.Basis.Agreed)
    metrics.applied(AgreementStage.Applied(waiting = waiting.size, verdicts = held.size, agreed = held.valuesIterator.count(_.agreed.isDefined),
      takenTmdb = agreedNow.count(_.film.isDefined), takenFallback = agreedNow.count(_.fallback.isDefined),
      open = gaps.groupMapReduce(_._1)(_ => 1)(_ + _), finds = finds.size, resolves = resolves, seconds = started.seconds))
    resolution.copy(decisions = decisions)
  }

  /** The listings as published, and the venue detail page the picks read of each. */
  private def digestOf(listings: Seq[Listing]): Long =
    AgreementStage.digest(listings.map(l => l.sortKey + (if (venues.hasDetail(l)) "\u0001" + venues.detail(l) else "")))

  /** The cluster's verdict: the stored one while its listings and every answer it read stand, else the resolver's
   *  over the families' answers now — `None` while one of them is a gap. */
  private def verdictOf(id: String, listings: Seq[Listing], digest: Long, lean: Option[SourceRecord], version: Long, asked: mutable.Set[(VoterFamily, String)],
                        moved: mutable.ArrayBuffer[StoredVerdict]): Option[StoredVerdict] = {
    held.get(id).filter(v => v.listings == digest && v.rules == AgreementStage.RulesVersion).filter { kept =>
      checked.get(id).contains((digest, version)) || untouched(id, kept, digest, version) || {
        val stands = kept.reads.forall { case (question, answered) => read(question).exists(answer => AgreementStage.digest(Seq(answer.toString)) == answered) }
        // read again because an answer was filed: a stale one among its reads is asked again too — a quiet tick reads none
        if (stands) { checked(id) = (digest, version); asked ++= kept.reads.keys.flatMap(staleOf) }
        stands
      }
    }
    .orElse(waiting.get(id).filter(w => w.listings == digest && !due(w)) match {
      case Some(w) => asked ++= w.gaps; None   // its questions not all answered yet: a resolve now could reach no verdict
      case None               => resolved(id, digest, listings, lean, version, asked, moved)
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
  private def resolved(id: String, digest: Long, listings: Seq[Listing], lean: Option[SourceRecord], version: Long, asked: mutable.Set[(VoterFamily, String)],
                       moved: mutable.ArrayBuffer[StoredVerdict]): Option[StoredVerdict] = {
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
        val verdict = StoredVerdict(id, digest, reads.toMap, Agreement.agreed(listings, verdicts.flatMap(_.toOption), lean,
          Some(java.time.LocalDate.ofInstant(clock.instant(), java.time.ZoneOffset.UTC).getYear)), AgreementStage.RulesVersion)
        if (!held.get(id).contains(verdict)) moved += verdict
        checked(id) = (digest, version)
        verdict
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
          case _          => None
        }
      }
    case _ => None
  }

  /** The decision with the agreed film taken — pending while TMDB was not asked about its IMDb id yet or the venue posters
   *  are not all hashed, and not taken when they VETO it ([[PosterEvidence.veto]]): a venue poster names another candidate. */
  private def taken(decision: ResolverDecision, agreed: AgreedFilm, finding: mutable.Set[String],
                    distances: Option[Int] => Answer[Map[Int, Option[Int]]]): AgreementStage.Take = {
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
    tmdb match {
      case Answer.Unknown =>
        imdb.foreach(finding += _)
        AgreementStage.Take.Pending
      case Answer.Known(Some(film)) => unlessVetoed(Some(film))(
        decision.copy(film = Some(film), basis = ResolverDecision.Basis.Agreed, explanation = decision.explanation :+ line, agreed = ids)(decision.trace))
      case Answer.Known(None) => imdb.fold[AgreementStage.Take](AgreementStage.Take.Vetoed)(id => unlessVetoed(None)(decision.copy(basis = ResolverDecision.Basis.Agreed,
        explanation = decision.explanation :+ line, fallback = Some(ResolverDecision.Fallback("imdb", id, 1.0)), agreed = ids)(decision.trace)))
    }
  }

  /** The decision with the one candidate the venue posters match taken ([[PosterEvidence.vote]]). */
  private def voted(decision: ResolverDecision, distances: Answer[Map[Int, Option[Int]]]): Option[ResolverDecision] =
    distances.toOption.flatMap(PosterEvidence.vote).map { case (film, bits) =>
      val named = tmdb.flatMap(_.film(film).toOption.flatten).fold(s"tmdb $film")(f => s"'${f.title}'${f.year.fold("")(year => s" ($year)")}")
      decision.copy(film = Some(film), basis = ResolverDecision.Basis.Poster,
        explanation = decision.explanation :+ s"the venue's poster matches $named ($bits bits), no other candidate's within ${PosterEvidence.VoteBits}")(
        decision.trace)
    }

  /** The nearest each of the cluster's candidates (and `also`, the film the families agree on) comes to its venue
   *  posters — empty where no listing shows one; `Unknown`, each poster not hashed yet noted in `asked`, while one is not. */
  private def posterDistances(id: String, digest: Long, listings: Seq[Listing], also: Option[Int],
                              asked: mutable.Set[AgreementStage.PosterQuestion]): Answer[Map[Int, Option[Int]]] = {
    val urls = PosterEvidence.urls(listings)
    if (urls.isEmpty || tmdb.isEmpty) Answer.Known(Map.empty)
    else {
      val venue = urls.map(url => url -> posters.venue(url))
      venue.collect { case (url, Answer.Unknown) => asked += AgreementStage.PosterQuestion.Venue(url) }
      if (venue.exists(_._2 == Answer.Unknown)) Answer.Unknown
      else {
        val shown: Seq[PosterHash] = venue.flatMap(_._2.toOption.flatten)
        if (shown.isEmpty) Answer.Known(Map.empty)
        else {
          val films  = (candidatesOf(id, digest, listings) ++ also.filterNot(FallbackIds.isFallback)).distinct
          val hashes = films.map(film => film -> posters.film(film))
          hashes.collect { case (film, Answer.Unknown) => asked += AgreementStage.PosterQuestion.Film(film) }
          if (hashes.exists(_._2 == Answer.Unknown)) Answer.Unknown
          else Answer.Known(hashes.map { case (film, answer) => film -> PosterEvidence.nearest(shown, answer.toOption.getOrElse(Nil)) }.toMap)
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
  }
}

object AgreementStage {
  /** 64 bits of `parts`' text, two MurmurHash3 seeds: stable across runs, as a stored digest must be. */
  def digest(parts: Seq[String]): Long = {
    val text = parts.mkString("\u0000")
    (scala.util.hashing.MurmurHash3.stringHash(text, 0x2f1d7a3b).toLong << 32) | (scala.util.hashing.MurmurHash3.stringHash(text, 0x6c8e9cf5).toLong & 0xffffffffL)
  }

  /** The questions an [[AgreementStage.apply]] met unanswered or stale, the agreed IMDb ids TMDB was not asked about, and
   *  the posters not hashed yet. */
  final case class Open(questions: Set[(VoterFamily, String)], finds: Set[String], posters: Set[PosterQuestion] = Set.empty)

  /** What became of the film a cluster's families agree on: taken, pending an answer, or not taken (vetoed, or nothing to take). */
  private enum Take {
    case Taken(decision: ResolverDecision)
    case Pending
    case Vetoed
  }

  /** A poster to hash: a venue's, by its URL, or a TMDB film's. */
  enum PosterQuestion {
    case Venue(url: String)
    case Film(tmdbId: Int)
  }

  /** `inner`, noting whether any answer it gave was `Unknown`. */
  private final class UnknownNoting(inner: IdentityLookups) extends IdentityLookups {
    @volatile var unknown = false
    private def noted[A](answer: Answer[A]): Answer[A] = { if (answer == Answer.Unknown) unknown = true; answer }
    def hasDetail(listing: Listing): Boolean                     = inner.hasDetail(listing)
    def detail(listing: Listing): Answer[Option[DetailFacts]]    = noted(inner.detail(listing))
    override def prefetch(queries: Iterable[CandidateQuery], films: Iterable[Int], details: Iterable[Listing]): Unit = inner.prefetch(queries, films, details)
    override def prefetchAnswered(): Unit                        = inner.prefetchAnswered()
    def candidates(query: CandidateQuery): Answer[Seq[Hit]]      = noted(inner.candidates(query))
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = noted(inner.film(tmdbId))
  }

  /** A model decision's cluster id, its listings sorted, their digest with the film it leans to, and that film. */
  private final case class Digested(id: String, listings: Seq[Listing], digest: Long, lean: Option[SourceRecord])

  /** The TMDB film a no-match leans to, as the agreement links it: by its TMDB and IMDb ids alone. */
  def leanRecord(lean: ResolverDecision.Leaning): SourceRecord =
    SourceRecord(services.identity.IdentityMeasures.Film(""), Map("tmdb" -> lean.film.toString, "imdb" -> f"tt${lean.imdbNumber}%07d"))

  /** What one [[AgreementStage.apply]] that read anything came to: the clusters waiting on a family's answer, the verdicts
   *  kept and how many agreed, the decisions taken as a TMDB film or an IMDb fallback, the questions still open per family
   *  and TMDB finds, how many clusters it resolved again, and how long it took. */
  final case class Applied(waiting: Int, verdicts: Int, agreed: Int, takenTmdb: Int, takenFallback: Int, open: Map[VoterFamily, Int],
                           finds: Int, resolves: Int, seconds: Double)
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
