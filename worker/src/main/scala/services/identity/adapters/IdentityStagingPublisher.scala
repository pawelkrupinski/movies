package services.identity
package adapters

import play.api.Logging
import services.Stoppable
import services.identity.staging.{ChangeFeed, IdentityStaging, StagedCluster, StagedFact, StagedListing, StagedQuestion, StagedTake, StagingBeat, StagingEvent}
import services.movies.ListingKey
import tools.{DaemonExecutors, ScheduledTick, TickRecorder}

import java.time.Clock
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
 * Publishes what this country's identity takes still wait on, and what they decided, for the staging screen
 * (`/debug/identity/staging`, docs/design/identity-questions.md "The staging screen"): the stage's waits live in the
 * worker's memory ([[agreement.AgreementStage.waitingOn]]), not in Mongo, so nothing else can show them.
 *
 * What HAPPENED comes as events (`changes`, drained from a [[ChangeFeed]], [[StagingEvent]]): the agreement stage's
 * waits starting and ending, as they do, and each written projection's change list ([[TakeChange]]). Each [[publish]]
 * reads them with the takes the projection last wrote (`taken`, shared with the projection, nothing copied) and what each
 * waits on now: a cluster whose [[WaitingOn]] is not settled STAGES — since its wait began — with each open question and
 * what the question memory says of its last ask (`outcome`), rewritten as it changes and deleted once it stops. What
 * RESOLVED since the last publish is written once, each its own document kept for `window` (the database deletes it): a
 * cluster that staged and waits on nothing now — `settled` with the take it got, or `regrouped` when its listings no
 * longer form it — one `settled` after a wait that began and ended between two publishes, never seen staging, with when
 * and on what; and every change of the projections' change lists, each in exactly one document ([[changesOf]]: decided,
 * joined, gained, lost, changed). Nothing is capped: what the feed lets go, or a run of failed publishes, is counted in
 * the beat (`dropped`) and `takeMetrics`. A quiet country writes its beat and nothing else; every publish reports its
 * takes by basis to `takeMetrics`.
 *
 * Off the projection's thread, on its own schedule (`start`): the projection's quiet tick allocates nothing for it.
 */
final class IdentityStagingPublisher(country: String, staging: IdentityStaging, taken: () => Option[Resolution],
                                     changes: () => ChangeFeed.Drained[StagingEvent], waitingOn: ResolverDecision => WaitingOn,
                                     outcomes: Set[Question] => Map[Question, QuestionMemory.Outcome], clock: Clock,
                                     window: FiniteDuration = IdentityStagingPublisher.Window,
                                     stagingCap: Int = IdentityStagingPublisher.StagingCap,
                                     ticks: TickRecorder = TickRecorder.Unrecorded,
                                     takeMetrics: IdentityStagingPublisher.TakeMetrics = IdentityStagingPublisher.TakeMetrics.Unrecorded,
                                     /** A TMDB film's record where the resolution holds none: a film only a later stage took
                                      *  (an agreement), or the one a moved listing held, read from the TMDB store, on this
                                      *  publisher's thread. */
                                     storedFilm: Int => Option[IdentityMeasures.Film] = _ => None,
                                     /** The listings the projection last read, for their facts: walked on this publisher's
                                      *  thread ([[IdentityProjection.Projected.read]]). */
                                     read: () => Iterator[Listing] = () => Iterator.empty,
                                     /** A cluster's key: the agreement stage's ([[agreement.AgreementStage.keyOf]]), which
                                      *  a listing sorting first, or leaving, does not move. */
                                     clusterKey: ResolverDecision => String = decision => StoredFamily.idOf(decision.members))
    extends Stoppable with Logging {
  import IdentityStagingPublisher.*

  /** A cluster's key on the screen: its key ([[clusterKey]]), hashed short. */
  private def keyOf(decision: ResolverDecision): String = keyOfId(clusterKey(decision))


  /** What the last publish left in the store, by key; read back from it once, on the first publish after a boot. */
  private var published: Option[Map[String, StagedCluster]] = None
  /** The events drained but not yet written: a publish that failed writes them with the next. */
  private var unwritten = Vector.empty[StagingEvent]
  /** The events let go since the last written beat: by the feed, full, or past [[UnwrittenCap]] while publishes fail. */
  private var dropped = 0L
  private lazy val scheduler = DaemonExecutors.scheduler(s"identity-staging-$country")

  def start(interval: FiniteDuration = Interval): Unit = {
    ticks.started(TickName, interval, interval)
    scheduler.scheduleWithFixedDelay(() => ScheduledTick.recorded(TickName, interval, logger, ticks)(publish()),
      interval.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Stops the schedule, and publishes once more: what happened since the last publish is written, not lost with the
   *  worker's memory. Only a worker killed outright loses the events of its last publish interval. */
  override def stop(): Unit = {
    scheduler.shutdown()
    try publish() catch { case NonFatal(e) => logger.warn(s"identity staging ($country): the last publish before stopping failed", e) }
  }

  /** One snapshot: staging and recently resolved clusters worked out, the changed ones written. Nothing before the
   *  projection's first written take. */
  def publish(): Unit = synchronized {
    taken().foreach { resolution =>
      val now    = clock.millis()
      val before = published.getOrElse(staging.ofCountry(country).map(c => c.key -> c).toMap)
      val wasStaging = before
      val events = unwritten ++ drained()
      // Each cluster's first wait started and last wait ended since: when it began waiting (and on what), and when it
      // stopped — most new clusters wait only inside one projection (their families asked inline), never seen staging.
      val startedOf = events.foldLeft(Map.empty[String, StagingEvent.Waiting]) {
        case (m, w: StagingEvent.Waiting) => val key = keyOfId(w.cluster); if (m.contains(key)) m else m.updated(key, w)
        case (m, _)                       => m
      }
      val endedOf = events.collect { case StagingEvent.Settled(cluster, at) => keyOfId(cluster) -> at }.toMap
      // the clusters each merged cluster absorbed: a wait one of them began is the merged cluster's, since it began
      val absorbedBy = events.collect { case StagingEvent.Merged(cluster, absorbed, _) => keyOfId(cluster) -> keyOfId(absorbed) }.groupMap(_._1)(_._2)
      val changed = events.collect { case c: StagingEvent.Changed => c }
      // each listing's facts: as a change carried it, else as the projection last read it, walked only when asked
      val carried = changed.iterator.flatMap(c => c.change.published.map(c.change.listing -> _)).toMap
      lazy val lastRead = read().map(l => l.key -> l).toMap
      val reading = new Reading(resolution, storedFilm, key => carried.get(key).orElse(lastRead.get(key)).fold(Nil)(StagedEvidence.listing))

      val waiting  = Seq.newBuilder[(String, ResolverDecision, WaitingOn)]
      val settled  = Map.newBuilder[String, ResolverDecision]
      val between  = Seq.newBuilder[(String, ResolverDecision, StagingEvent.Waiting)]
      val clustersBy   = scala.collection.mutable.HashMap.empty[String, Int]
      val listingsBy   = scala.collection.mutable.HashMap.empty[String, Int]
      // each changed listing's cluster now: only those changed are looked up
      val changedListings = changed.iterator.map(_.change.listing).toSet
      val clusterOf       = scala.collection.mutable.HashMap.empty[ListingKey, ResolverDecision]
      resolution.decisions.foreach { decision =>
        if (decision.members.nonEmpty) {
          val key   = keyOf(decision)
          val waits = waitingOn(decision)
          val basis = decision.basis.toString
          clustersBy(basis) = clustersBy.getOrElse(basis, 0) + 1
          listingsBy(basis) = listingsBy.getOrElse(basis, 0) + decision.members.size
          if (changedListings.nonEmpty) decision.members.foreach(m => if (changedListings(m)) clusterOf(m) = decision)
          if (!waits.settled) waiting += ((key, decision, waits))
          else if (wasStaging.contains(key)) settled += key -> decision
          // a wait begun and ended between two publishes: since it began, when it ended, and on what
          else startedOf.get(key).foreach(w => between += ((key, decision, w)))
        }
      }
      val waitingNow = waiting.result()
      val betweenNow = between.result()
      // every question this tick shows, its last ask's outcome read from the memory at once
      val resolvedQuestions = wasStaging.valuesIterator.flatMap(_.questions).flatMap(q => Question.parse(q.text)).toSet
      val known = outcomes(waitingNow.iterator.flatMap(_._3.questions).toSet ++ betweenNow.iterator.flatMap(_._3.questions) ++
        resolvedQuestions)
      val nowStaging = waitingNow.map { case (key, decision, waits) =>
        val since = (wasStaging.get(key).map(_.since).orElse(startedOf.get(key).map(_.at)) ++
          absorbedBy.getOrElse(key, Nil).flatMap(wasStaging.get).map(_.since)).minOption.getOrElse(now)
        key -> stagedOf(key, decision, reading, waits, since, known, wasStaging.get(key))
      }.toMap
      val settledNow = settled.result()
      val changeDocs = changesOf(changed, clusterOf, reading)
      val changedClusters = changeDocs.groupBy(_.key.takeWhile(_ != '@'))
      // A cluster that stopped waiting and whose listings' take the projection changed is that change — gained, lost,
      // changed, decided or joined — after its wait (since it began, with its questions); `settled` only when no take
      // moved. Each written once, under its own key (the cluster's and when it was published, or what changed).
      val waited = wasStaging.collect { case (key, was) if !nowStaging.contains(key) =>
        resolvedOf(was, settledNow.get(key), reading, endedOf.getOrElse(key, now), known)
      } ++ betweenNow.map { case (key, decision, w) =>
        // never seen waiting: its take, and the questions it waited on as the memory tells them now
        stagedOf(key, decision, reading, WaitingOn(0, w.questions, onTmdb = false), w.at, known, None)
          .copy(resolvedAt = Some(endedOf.getOrElse(key, now)), resolution = Some(StagedCluster.Settled))
      }
      val (waitedThenChanged, settledAlone) = waited.toSeq.partition(c => c.resolution.contains(StagedCluster.Settled) && changedClusters.contains(c.key))
      val afterWaits = waitedThenChanged.iterator.map(c => c.key -> c).toMap
      val resolvedNow = settledAlone.map(c => c.copy(key = s"${c.key}@$now")) ++ changeDocs.map { doc =>
        afterWaits.get(doc.key.takeWhile(_ != '@')).fold(doc)(w => doc.copy(since = w.since, questions = w.questions,
          questionCount = w.questionCount, attempted = w.attempted))
      }
      val shownStaging = nowStaging.values.toSeq.sortBy(c => (c.since, c.key)).take(stagingCap)

      val next     = shownStaging.map(c => c.key -> c).toMap
      val upserts  = next.values.filterNot(c => before.get(c.key).contains(c)).toSeq
      val removed  = before.keySet -- next.keySet
      val open     = nowStaging.values.toSeq.flatMap(_.questions)
      val beat     = StagingBeat(at = now, staging = nowStaging.size, shown = shownStaging.size, resolved = resolvedNow.size,
        openByKind = open.filter(_.outcome != StagedQuestion.Answered).groupMapReduce(_.kind)(_ => 1)(_ + _),
        modelGaps = nowStaging.valuesIterator.map(_.modelGaps).sum, dropped = Option.when(dropped > 0)(dropped))
      takeMetrics.held(clustersBy.toMap, listingsBy.toMap)
      try {
        staging.publish(country, upserts, removed, beat, resolvedNow.sortBy(_.key), java.time.Duration.ofMillis(window.toMillis))
        published = Some(next)
        unwritten = Vector.empty
        dropped = 0
      } catch {
        // the next publish diffs against the store again, what it holds unknown now, and writes these events with its own
        case NonFatal(e) =>
          published = None
          unwritten = events.takeRight(UnwrittenCap)
          letGo(events.size - unwritten.size)
          throw e
      }
    }
  }

  /** What happened since the last drain; what the feed let go is counted ([[letGo]]). */
  private def drained(): Vector[StagingEvent] = {
    val feed = changes()
    letGo(feed.dropped)
    feed.changes
  }

  /** `n` events let go before a publish wrote them: counted in the next beat and the metric, and logged. */
  private def letGo(n: Long): Unit = if (n > 0) {
    dropped += n
    takeMetrics.dropped(n)
    logger.warn(s"identity staging ($country): $n staging events let go before a publish wrote them")
  }

  /** The projections' change lists as RESOLUTIONS, every change in exactly one: one document per projection, cluster,
   *  kind and film before — a cluster `decided` (its listings all new), listings that `joined` a cluster already on the
   *  films, or listings whose film was `gained`, `lost` or `changed` (`was` the film they held) — listing the listings
   *  that changed (the first [[ListingsShown]], `listingCount` all of them), its cluster's take as it stands now and,
   *  for a move, the take before ([[priorOf]]). A
   *  listing in no cluster now is a document of its own, its take the change's film. Keyed by the projection and what
   *  changed, so a publish that fails and writes them with the next writes the same documents. */
  private def changesOf(changed: Seq[StagingEvent.Changed], clusterOf: collection.Map[ListingKey, ResolverDecision],
                        reading: Reading): Seq[StagedCluster] =
    changed.groupBy { c =>
      val change = c.change
      (c.projection, clusterOf.get(change.listing).fold(ListingKey.serialised(change.listing))(clusterKey),
        change.kind, change.before)
    }.toSeq.map { case ((projection, cluster, kind, was), changes) =>
      val at       = changes.head.at
      val listings = changes.map(_.change.listing).distinct.sorted
      val shown    = listings.take(ListingsShown)
      val take     = clusterOf.get(listings.head).fold {
        val first = changes.head.change
        StagedTake(first.after, first.after.flatMap(reading.title), first.basisAfter.fold("none")(_.toString), 0.0, None, None, None,
          filmFacts = reading.filmFacts(first.after))
      }(reading.take(_, shown))
      val key      = keyOfId(cluster)
      val priors   = changes.flatMap(c => c.change.prior.map(c.change.listing -> _)).distinctBy(_._1)
      StagedCluster(s"$key@$projection/${kind.label}${was.fold("")("/" + _)}", reading.listings(shown), listings.size, take, 0,
        onTmdb = false, Nil, 0, 0, at, resolvedAt = Some(at), resolution = Some(kind.label), was = was,
        before = Option.when(changes.head.change.moved)(reading.prior(was, priors, take.film)))
    }

  /** `earlier`: the cluster as the last publish staged it, whose listings' facts stand for those the projection did not
   *  read this time (a scoped one's, outside its scope). */
  private def stagedOf(key: String, decision: ResolverDecision, reading: Reading, waits: WaitingOn, since: Long,
                       known: Map[Question, QuestionMemory.Outcome], earlier: Option[StagedCluster]): StagedCluster = {
    val questions = waits.questions.toSeq.map(questionOf(_, known)).sortBy(q => (StagedQuestion.Outcomes.indexOf(q.outcome), q.kind, q.text))
    val shown     = reading.shown(decision.members)
    val heldFacts = earlier.fold(Map.empty[(String, String), Seq[StagedFact]])(_.listings.map(l => (l.venue, l.rawTitle) -> l.facts).toMap)
    val listings  = reading.listings(shown).map(l => if (l.facts.nonEmpty) l else l.copy(facts = heldFacts.getOrElse((l.venue, l.rawTitle), Nil)))
    StagedCluster(key, listings, decision.members.size, reading.take(decision, shown), waits.modelGaps, waits.onTmdb,
      questions.take(QuestionsShown), questions.size, questions.count(_.outcome != StagedQuestion.NotAttempted), since)
  }

  /** A cluster that stopped waiting: its questions as the memory last tells them — answered, or left out. */
  private def resolvedOf(was: StagedCluster, settled: Option[ResolverDecision], reading: Reading, now: Long,
                         known: Map[Question, QuestionMemory.Outcome]): StagedCluster = {
    val questions = was.questions.map(q => Question.parse(q.text).fold(q)(questionOf(_, known)))
    was.copy(take = settled.fold(was.take)(d => reading.take(d, reading.shown(d.members))), questions = questions,
      attempted = was.attempted.max(questions.count(_.outcome != StagedQuestion.NotAttempted)),
      resolvedAt = Some(now), resolution = Some(if (settled.isDefined) StagedCluster.Settled else StagedCluster.Regrouped))
  }

  private def questionOf(question: Question, known: Map[Question, QuestionMemory.Outcome]): StagedQuestion = {
    val base = StagedQuestion(question.kind.toString, askerOf(question), textOf(question), StagedQuestion.NotAttempted)
    known.get(question).fold(base) {
      case QuestionMemory.Outcome.Answered => base.copy(outcome = StagedQuestion.Answered)
      case QuestionMemory.Outcome.Absent   => base.copy(outcome = StagedQuestion.Absent)
      case QuestionMemory.Outcome.Failed(attempts, nextRetryAt, host) =>
        base.copy(outcome = StagedQuestion.Failed, attempts = attempts, nextRetryAt = Some(nextRetryAt.toEpochMilli), host = host)
    }
  }
}

object IdentityStagingPublisher {
  /** Where every publish reports the takes it read, by basis (`ResolverDecision.Basis`): the clusters and listings each
   *  basis holds now, and the events it was never told. What changed is the projection's to count
   *  (`IdentityProjectionMetrics.takesChanged`). */
  trait TakeMetrics {
    def held(clusters: Map[String, Int], listings: Map[String, Int]): Unit
    /** `events` the staging screen was not told: let go by a full feed, or past what failed publishes keep. */
    def dropped(events: Long): Unit
  }
  object TakeMetrics {
    val Unrecorded: TakeMetrics = new TakeMetrics {
      def held(clusters: Map[String, Int], listings: Map[String, Int]): Unit = ()
      def dropped(events: Long): Unit                                        = ()
    }
  }

  /** How often a country publishes. */
  val Interval: FiniteDuration = 20.seconds
  /** How long a resolved cluster stays on the screen: the database deletes it then (its `expireAt`). */
  val Window: FiniteDuration = 7.days
  /** The most changes a failed publish keeps for the next: the newest. */
  val UnwrittenCap = 20000
  /** How many changes the projection's feed holds for the publisher between two publishes; the oldest go past it. */
  val FeedCapacity = 20000
  /** The most staging clusters a country lists, the longest waiting; its beat counts them all. */
  val StagingCap = 500
  /** The most questions a cluster lists; it counts them all. */
  val QuestionsShown = 60
  /** The most listings a cluster lists; it counts them all. */
  val ListingsShown = 12
  /** The most explanation lines a take keeps, the most telling first; a last line counts the rest. */
  val ExplanationShown = 6
  private val TickName = "identity-staging"

  /** The screen's key of the cluster whose key ([[agreement.AgreementStage.keyOf]]) is `id`: hashed short. */
  def keyOfId(id: String): String = java.lang.Long.toHexString(agreement.AgreementStage.digest(Seq(id)))


  /** What one publish reads its takes against: the films of the model's resolution, else of the TMDB store
   *  (`storedFilm`), and each listing's facts (`factsOf`, none when it is not known). */
  private final class Reading(resolution: Resolution, storedFilm: Int => Option[IdentityMeasures.Film],
                              factsOf: ListingKey => Seq[StagedFact]) {
    def film(id: Int): Option[IdentityMeasures.Film] = resolution.films.get(id).orElse(storedFilm(id))
    def title(id: Int): Option[String]                = film(id).map(_.title)
    def filmFacts(id: Option[Int]): Seq[StagedFact]   = id.flatMap(i => film(i).map(StagedEvidence.film(i, _))).getOrElse(Nil)

    /** The listings a cluster lists of `keys`: one per venue and title, the first [[ListingsShown]]. */
    def shown(keys: Seq[ListingKey]): Seq[ListingKey] =
      keys.distinctBy(k => (k.venue, k.rawTitle)).sortBy(k => (k.venue, k.rawTitle)).take(ListingsShown)
    def listings(keys: Seq[ListingKey]): Seq[StagedListing] = keys.map(k => StagedListing(k.venue, k.rawTitle, factsOf(k)))

    /** `decision`'s take as the screen shows it: its films titled, the film's facts, and its trace of each of `shown`. */
    def take(decision: ResolverDecision, shown: Seq[ListingKey]): StagedTake = {
      val leaning = decision.leaning.orElse(decision.candidate).map(_.film)
      StagedTake(decision.film, decision.film.flatMap(title), decision.basis.toString, decision.confidence,
        leaning, leaning.flatMap(title),
        decision.fallback.map(f => s"${f.source}:${f.id}${f.title.fold("")(" " + _)}"), decision.explanationShown(ExplanationShown),
        filmFacts = filmFacts(decision.film.orElse(leaning)),
        nodes = shown.flatMap(k => decision.trace.nodes.get(k).map(StagedEvidence.node(k, _, title))))
    }

    /** The take moved listings held: film `was`, and the decision it came from (`priors`, each listing's, when the
     *  projection saw it) — its basis and confidence; as why its last reason, the rule that decided it and the runner-up
     *  it weighed beside it, said to be the film taken `now` when that one is; that film's facts, and each listing's
     *  trace against it. */
    def prior(was: Option[Int], priors: Seq[(ListingKey, TakeChange.Prior)], now: Option[Int]): StagedTake = {
      val first    = priors.headOption.map(_._2)
      val runnerUp = first.flatMap(_.runnerUp).map { id =>
        s"runner-up then: tmdb $id${title(id).fold("")(" " + _)}" + (if (now.contains(id)) " — the film taken now" else "")
      }
      StagedTake(was, was.flatMap(title), first.fold("unknown")(_.basis.toString), first.fold(0.0)(_.confidence),
        explanation = first.toSeq.flatMap(p => p.reason.toSeq ++ p.decidedBy.map("decided by " + _)) ++ runnerUp,
        filmFacts = filmFacts(was),
        nodes = priors.take(ListingsShown).flatMap { case (k, p) => p.node.map(StagedEvidence.node(k, _, title)) })
    }
  }

  /** Who a question is asked of: the family it names, else the part of the identity system that asks it. */
  private def askerOf(question: Question): String = question match {
    case Question.Family(family, _) => family.label
    case Question.VenuePage(_)      => "venue"
    case Question.Poster(_)         => "poster"
    case Question.Catalogue(_)      => "catalogue"
    case _                          => "tmdb"
  }

  /** A question as the screen names it: an agreement question by the id it is asked under ([[Question.id]], read back
   *  by [[Question.parse]]), a model question by what it asks. */
  def textOf(question: Question): String = question.id.getOrElse(question match {
    case Question.Candidate(query) => s"${query.productPrefix.toLowerCase(java.util.Locale.ROOT)}|${query.productElement(0)}"
    case Question.FilmRecord(id)   => s"film|$id"
    case Question.Person(id)       => s"person|$id"
    case Question.VenuePage(key)   => s"page|${key.venue}|${key.rawTitle}"
    case other                     => other.toString
  })
}
