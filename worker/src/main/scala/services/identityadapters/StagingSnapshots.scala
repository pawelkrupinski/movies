package services.identityadapters

import services.identity.*
import services.identity.staging.{StagedCluster, StagedFact, StagedQuestion, StagedTake, StagingBeat, StagingEvent}
import models.ListingKey

/**
 * One staging-screen snapshot ([[IdentityStagingPublisher]]), worked out from the takes the projection last wrote, the
 * events since the last written publish and what the last publish left staged: the clusters staging now, the ones that
 * RESOLVED since, and the beat. Pure but for its collaborators' reads: `waitingOn` (what a cluster waits on now),
 * `outcomes` (the question memory's last ask of each question), `storedFilm` and `read` (the facts of films and
 * listings the resolution does not hold) and `clusterKey` (a cluster's key, [[agreement.AgreementStage.keyOf]]).
 */
final class StagingSnapshots(waitingOn: ResolverDecision => WaitingOn,
                             outcomes: Set[Question] => Map[Question, QuestionMemory.Outcome],
                             /** A TMDB film's record where the resolution holds none: a film only a later stage took
                              *  (an agreement), or the one a moved listing held, read from the TMDB store, on the
                              *  publisher's thread. */
                             storedFilm: Int => Option[IdentityMeasures.Film] = _ => None,
                             /** The listings the projection last read, for their facts: walked on the publisher's
                              *  thread ([[IdentityProjection.Projected.read]]). */
                             read: () => Iterator[Listing] = () => Iterator.empty,
                             /** A cluster's key: the agreement stage's ([[agreement.AgreementStage.keyOf]]), which
                              *  a listing sorting first, or leaving, does not move. */
                             clusterKey: ResolverDecision => String = decision => StoredFamily.idOf(decision.members),
                             stagingCap: Int = StagingSnapshots.StagingCap) {
  import StagingSnapshots.*

  /** A cluster's key on the screen: its key ([[clusterKey]]), hashed short. */
  private def keyOf(decision: ResolverDecision): String = keyOfId(clusterKey(decision))

  /** The snapshot of `resolution`, against `wasStaging` (what the last publish left in the store, by key), over `events`
   *  (those since the last written publish), at `now`; `dropped`: the events let go since the last written beat. */
  def of(resolution: Resolution, wasStaging: Map[String, StagedCluster], events: Vector[StagingEvent], now: Long, dropped: Long): Snapshot = {
    val digest  = EventDigest.of(events)
    val reading = readingOf(resolution, digest.changed)
    val sorted  = sort(resolution, wasStaging, digest)
    // every question this tick shows, its last ask's outcome read from the memory at once
    val resolvedQuestions = wasStaging.valuesIterator.flatMap(_.questions).flatMap(q => Question.parse(q.text)).toSet
    val known = outcomes(sorted.waiting.iterator.flatMap(_._3.questions).toSet ++ sorted.between.iterator.flatMap(_._3.questions) ++
      resolvedQuestions)
    val nowStaging = sorted.waiting.map { case (key, decision, waits) =>
      val since = (wasStaging.get(key).map(_.since).orElse(digest.startedOf.get(key).map(_.at)) ++
        digest.absorbedBy.getOrElse(key, Nil).flatMap(wasStaging.get).map(_.since)).minOption.getOrElse(now)
      key -> stagedOf(key, decision, reading, waits, since, known, wasStaging.get(key))
    }.toMap
    val resolvedNow  = resolvedSince(wasStaging, nowStaging, sorted, digest, reading, known, now)
    val shownStaging = nowStaging.values.toSeq.sortBy(c => (c.since, c.key)).take(stagingCap)
    val open         = nowStaging.values.toSeq.flatMap(_.questions)
    val beat         = StagingBeat(at = now, staging = nowStaging.size, shown = shownStaging.size, resolved = resolvedNow.size,
      openByKind = open.filter(_.outcome != StagedQuestion.Answered).groupMapReduce(_.kind)(_ => 1)(_ + _),
      modelGaps = nowStaging.valuesIterator.map(_.modelGaps).sum, dropped = Option.when(dropped > 0)(dropped))
    Snapshot(shownStaging.map(c => c.key -> c).toMap, resolvedNow.sortBy(_.key), beat, sorted.clustersBy, sorted.listingsBy)
  }

  /** What a publish reads its takes against: each listing's facts as a change carried them, else as the projection last
   *  read them, walked only when asked. */
  private def readingOf(resolution: Resolution, changed: Seq[StagingEvent.Changed]): StagingReading = {
    val carried = changed.iterator.flatMap(c => c.change.published.map(c.change.listing -> _)).toMap
    lazy val lastRead = read().map(l => l.key -> l).toMap
    new StagingReading(resolution, storedFilm, key => carried.get(key).orElse(lastRead.get(key)).fold(Nil)(StagedEvidence.listing))
  }

  /** Each cluster of `resolution` sorted: waiting now, settled after staging, or a wait begun and ended between two
   *  publishes; the clusters and listings each basis holds; and each changed listing's cluster now. */
  private def sort(resolution: Resolution, wasStaging: Map[String, StagedCluster], digest: EventDigest): Sorted = {
    val waiting  = Seq.newBuilder[(String, ResolverDecision, WaitingOn)]
    val settled  = Map.newBuilder[String, ResolverDecision]
    val between  = Seq.newBuilder[(String, ResolverDecision, StagingEvent.Waiting)]
    val clustersBy   = scala.collection.mutable.HashMap.empty[String, Int]
    val listingsBy   = scala.collection.mutable.HashMap.empty[String, Int]
    // each changed listing's cluster now: only those changed are looked up
    val changedListings = digest.changed.iterator.map(_.change.listing).toSet
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
        else digest.startedOf.get(key).foreach(w => between += ((key, decision, w)))
      }
    }
    Sorted(waiting.result(), settled.result(), between.result(), clustersBy.toMap, listingsBy.toMap, clusterOf)
  }

  /** What resolved since the last publish. A cluster that stopped waiting and whose listings' take the projection changed
   *  is that change — gained, lost, changed, decided or joined — after its wait (since it began, with its questions);
   *  `settled` only when no take moved. Each written once, under its own key (the cluster's and when it was published,
   *  or what changed). */
  private def resolvedSince(wasStaging: Map[String, StagedCluster], nowStaging: Map[String, StagedCluster], sorted: Sorted,
                            digest: EventDigest, reading: StagingReading, known: Map[Question, QuestionMemory.Outcome], now: Long): Seq[StagedCluster] = {
    val changeDocs = changesOf(digest.changed, sorted.clusterOf, reading)
    val changedClusters = changeDocs.groupBy(_.key.takeWhile(_ != '@'))
    val waited = wasStaging.collect { case (key, was) if !nowStaging.contains(key) =>
      resolvedOf(was, sorted.settled.get(key), reading, digest.endedOf.getOrElse(key, now), known)
    } ++ sorted.between.map { case (key, decision, w) =>
      // never seen waiting: its take, and the questions it waited on as the memory tells them now
      stagedOf(key, decision, reading, WaitingOn(0, w.questions, onTmdb = false), w.at, known, None)
        .copy(resolvedAt = Some(digest.endedOf.getOrElse(key, now)), resolution = Some(StagedCluster.Settled))
    }
    val (waitedThenChanged, settledAlone) = waited.toSeq.partition(c => c.resolution.contains(StagedCluster.Settled) && changedClusters.contains(c.key))
    val afterWaits = waitedThenChanged.iterator.map(c => c.key -> c).toMap
    settledAlone.map(c => c.copy(key = s"${c.key}@$now")) ++ changeDocs.map { doc =>
      afterWaits.get(doc.key.takeWhile(_ != '@')).fold(doc)(w => doc.copy(since = w.since, questions = w.questions,
        questionCount = w.questionCount, attempted = w.attempted))
    }
  }

  /** The projections' change lists as RESOLUTIONS, every change in exactly one: one document per projection, cluster,
   *  kind and film before — a cluster `decided` (its listings all new), listings that `joined` a cluster already on the
   *  films, or listings whose film was `gained`, `lost` or `changed` (`was` the film they held) — listing the listings
   *  that changed (the first [[ListingsShown]], `listingCount` all of them), its cluster's take as it stands now and,
   *  for a move, the take before ([[priorOf]]). A
   *  listing in no cluster now is a document of its own, its take the change's film. Keyed by the projection and what
   *  changed, so a publish that fails and writes them with the next writes the same documents. */
  private def changesOf(changed: Seq[StagingEvent.Changed], clusterOf: collection.Map[ListingKey, ResolverDecision],
                        reading: StagingReading): Seq[StagedCluster] =
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
      val formerly = changes.iterator.flatMap(c => c.change.formerly.map(c.change.listing -> _)).toMap
      val listed   = shown.zip(reading.listings(shown)).map { case (k, l) => formerly.get(k).fold(l)(f => l.copy(formerly = StagedEvidence.listing(f))) }
      StagedCluster(s"$key@$projection/${kind.label}${was.fold("")("/" + _)}", listed, listings.size, take, 0,
        onTmdb = false, Nil, 0, 0, at, resolvedAt = Some(at), resolution = Some(kind.label), was = was,
        before = Option.when(changes.head.change.moved)(reading.prior(was, priors, take.film)), trigger = changes.head.change.trigger)
    }

  /** `earlier`: the cluster as the last publish staged it, whose listings' facts stand for those the projection did not
   *  read this time (a scoped one's, outside its scope). */
  private def stagedOf(key: String, decision: ResolverDecision, reading: StagingReading, waits: WaitingOn, since: Long,
                       known: Map[Question, QuestionMemory.Outcome], earlier: Option[StagedCluster]): StagedCluster = {
    val questions = waits.questions.toSeq.map(StagedQuestions.of(_, known)).sortBy(q => (StagedQuestion.Outcomes.indexOf(q.outcome), q.kind, q.text))
    val shown     = reading.shown(decision.members)
    val heldFacts = earlier.fold(Map.empty[(String, String), Seq[StagedFact]])(_.listings.map(l => (l.venue, l.rawTitle) -> l.facts).toMap)
    val listings  = reading.listings(shown).map(l => if (l.facts.nonEmpty) l else l.copy(facts = heldFacts.getOrElse((l.venue, l.rawTitle), Nil)))
    StagedCluster(key, listings, decision.members.size, reading.take(decision, shown), waits.modelGaps, waits.onTmdb,
      questions.take(QuestionsShown), questions.size, questions.count(_.outcome != StagedQuestion.NotAttempted), since)
  }

  /** A cluster that stopped waiting: its questions as the memory last tells them — answered, or left out. */
  private def resolvedOf(was: StagedCluster, settled: Option[ResolverDecision], reading: StagingReading, now: Long,
                         known: Map[Question, QuestionMemory.Outcome]): StagedCluster = {
    val questions = was.questions.map(q => Question.parse(q.text).fold(q)(StagedQuestions.of(_, known)))
    was.copy(take = settled.fold(was.take)(d => reading.take(d, reading.shown(d.members))), questions = questions,
      attempted = was.attempted.max(questions.count(_.outcome != StagedQuestion.NotAttempted)),
      resolvedAt = Some(now), resolution = Some(if (settled.isDefined) StagedCluster.Settled else StagedCluster.Regrouped))
  }
}

object StagingSnapshots {
  /** One publish's snapshot: the staging clusters shown (`next`, by key, the longest waiting), what resolved since (by
   *  key), the beat, and the clusters and listings each basis holds. */
  final case class Snapshot(next: Map[String, StagedCluster], resolved: Seq[StagedCluster], beat: StagingBeat,
                            clustersBy: Map[String, Int], listingsBy: Map[String, Int])

  /** What a publish reads of its events: each cluster's first wait started and last wait ended since — when it began
   *  waiting (and on what), and when it stopped; most new clusters wait only inside one projection (their families asked
   *  inline), never seen staging — the clusters each merged cluster absorbed (a wait one of them began is the merged
   *  cluster's, since it began), and the projections' changes. */
  private final case class EventDigest(startedOf: Map[String, StagingEvent.Waiting], endedOf: Map[String, Long],
                                       absorbedBy: Map[String, Seq[String]], changed: Seq[StagingEvent.Changed])
  private object EventDigest {
    def of(events: Seq[StagingEvent]): EventDigest = EventDigest(
      events.foldLeft(Map.empty[String, StagingEvent.Waiting]) {
        case (m, w: StagingEvent.Waiting) => val key = keyOfId(w.cluster); if (m.contains(key)) m else m.updated(key, w)
        case (m, _)                       => m
      },
      events.collect { case StagingEvent.Settled(cluster, at) => keyOfId(cluster) -> at }.toMap,
      events.collect { case StagingEvent.Merged(cluster, absorbed, _) => keyOfId(cluster) -> keyOfId(absorbed) }.groupMap(_._1)(_._2),
      events.collect { case c: StagingEvent.Changed => c })
  }

  /** The clusters of a resolution, sorted ([[StagingSnapshots.sort]]). */
  private final case class Sorted(waiting: Seq[(String, ResolverDecision, WaitingOn)], settled: Map[String, ResolverDecision],
                                  between: Seq[(String, ResolverDecision, StagingEvent.Waiting)],
                                  clustersBy: Map[String, Int], listingsBy: Map[String, Int],
                                  clusterOf: collection.Map[ListingKey, ResolverDecision])

  /** The most staging clusters a country lists, the longest waiting; its beat counts them all. */
  val StagingCap = 500
  /** The most questions a cluster lists; it counts them all. */
  val QuestionsShown = 60
  /** The most listings a cluster lists; it counts them all. */
  val ListingsShown = 12
  /** The most explanation lines a take keeps, the most telling first; a last line counts the rest. */
  val ExplanationShown = 6
  /** The screen's key of the cluster whose key ([[agreement.AgreementStage.keyOf]]) is `id`: hashed short. */
  def keyOfId(id: String): String = java.lang.Long.toHexString(agreement.AgreementStage.digest(Seq(id)))
}
