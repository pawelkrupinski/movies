package services.identity
package adapters

import play.api.Logging
import services.Stoppable
import services.identity.staging.{IdentityStaging, StagedCluster, StagedListing, StagedQuestion, StagedTake, StagingBeat}
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
 * Each [[publish]] reads the takes the projection last wrote (`taken`, shared with the projection, nothing copied) and
 * what each waits on: a cluster whose [[WaitingOn]] is not settled STAGES, with each open question and what the
 * question memory says of its last ask (`outcome`), rewritten as it changes and deleted once it stops. What RESOLVED
 * since the last publish is written once, each its own document kept for `window` (the database deletes it): a
 * cluster that staged and waits on nothing now — `settled` with the take it got, or `regrouped` when its listings no
 * longer form it — and, from each cluster's film as the last publish saw it, one `decided` between two publishes
 * (most new films, asked inline within one projection) or whose film `moved`. At most `resolvedCap` a publish. A quiet
 * country writes its beat and nothing else; every publish reports its takes by basis to `takeMetrics`.
 *
 * Off the projection's thread, on its own schedule (`start`): the projection's quiet tick allocates nothing for it.
 */
final class IdentityStagingPublisher(country: String, staging: IdentityStaging, taken: () => Option[Resolution],
                                     waitingOn: ResolverDecision => WaitingOn,
                                     outcomes: Set[Question] => Map[Question, QuestionMemory.Outcome], clock: Clock,
                                     window: FiniteDuration = IdentityStagingPublisher.Window,
                                     resolvedCap: Int = IdentityStagingPublisher.ResolvedCap,
                                     stagingCap: Int = IdentityStagingPublisher.StagingCap,
                                     ticks: TickRecorder = TickRecorder.Unrecorded,
                                     takeMetrics: IdentityStagingPublisher.TakeMetrics = IdentityStagingPublisher.TakeMetrics.Unrecorded)
    extends Stoppable with Logging {
  import IdentityStagingPublisher.*

  /** What the last publish left in the store, by key; read back from it once, on the first publish after a boot. */
  private var published: Option[Map[String, StagedCluster]] = None
  /** Each listing's film as the last publish saw it: what tells a cluster decided (every listing new) or moved (a
   *  listing's film changed) since — most settle inside one projection (a new film's questions are asked inline), never
   *  seen staging. By listing, not by cluster: a cluster's key moves when a new listing sorts first or clusters merge,
   *  which changes no listing's film. None until the first publish after a boot, which only records them. */
  private var lastFilmOf: Option[Map[ListingKey, Option[Int]]] = None
  private lazy val scheduler = DaemonExecutors.scheduler(s"identity-staging-$country")

  def start(interval: FiniteDuration = Interval): Unit = {
    ticks.started(TickName, interval, interval)
    scheduler.scheduleWithFixedDelay(() => ScheduledTick.recorded(TickName, interval, logger, ticks)(publish()),
      interval.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  override def stop(): Unit = { scheduler.shutdown(); () }

  /** One snapshot: staging and recently resolved clusters worked out, the changed ones written. Nothing before the
   *  projection's first written take. */
  def publish(): Unit = synchronized {
    taken().foreach { resolution =>
      val now    = clock.millis()
      val before = published.getOrElse(staging.ofCountry(country).map(c => c.key -> c).toMap)
      val wasStaging = before

      val waiting  = Seq.newBuilder[(String, ResolverDecision, WaitingOn)]
      val settled  = Map.newBuilder[String, ResolverDecision]
      // key, decision, waits, how (decided, or its film gained, lost or changed), and the film its listings held before
      val decided  = Seq.newBuilder[(String, ResolverDecision, WaitingOn, String, Option[Int])]
      val filmOfNow  = Map.newBuilder[ListingKey, Option[Int]]
      val clustersBy   = scala.collection.mutable.HashMap.empty[String, Int]
      val listingsBy   = scala.collection.mutable.HashMap.empty[String, Int]
      resolution.decisions.foreach { decision =>
        if (decision.members.nonEmpty) {
          val key   = keyOf(decision)
          val waits = waitingOn(decision)
          val basis = decision.basis.toString
          clustersBy(basis) = clustersBy.getOrElse(basis, 0) + 1
          listingsBy(basis) = listingsBy.getOrElse(basis, 0) + decision.members.size
          decision.members.foreach(member => filmOfNow += member -> decision.film)
          if (!waits.settled) waiting += ((key, decision, waits))
          else if (wasStaging.contains(key)) settled += key -> decision
          else lastFilmOf.foreach { last =>
            val previous = decision.members.flatMap(last.get)
            if (previous.isEmpty) decided += ((key, decision, waits, StagedCluster.Decided, None))
            else previous.find(_ != decision.film).foreach(was => decided += ((key, decision, waits, changeOf(was, decision.film), was)))
          }
        }
      }
      val waitingNow = waiting.result()
      val decidedNow = decided.result()
      // every question this tick shows, its last ask's outcome read from the memory at once
      val resolvedQuestions = wasStaging.valuesIterator.flatMap(_.questions).flatMap(q => Question.parse(q.text)).toSet
      val known = outcomes(waitingNow.iterator.flatMap(_._3.questions).toSet ++ decidedNow.iterator.flatMap(_._3.questions) ++
        resolvedQuestions)
      val nowStaging = waitingNow.map { case (key, decision, waits) =>
        key -> stagedOf(key, decision, resolution, waits, wasStaging.get(key).fold(now)(_.since), known)
      }.toMap
      val settledNow = settled.result()
      // each written once, under its own key (the cluster's and when it resolved): a cluster that moves twice shows twice
      val resolvedNow = (wasStaging.collect { case (key, was) if !nowStaging.contains(key) =>
        resolvedOf(was, settledNow.get(key), resolution, now, known)
      } ++ decidedNow.map { case (key, decision, waits, how, was) =>
        // decided or moved between two publishes, never seen waiting: its take, and no open question (it settled)
        stagedOf(key, decision, resolution, waits, now, known).copy(resolvedAt = Some(now), resolution = Some(how), was = was)
      }).toSeq.sortBy(_.key).take(resolvedCap).map(c => c.copy(key = s"${c.key}@$now"))
      val shownStaging = nowStaging.values.toSeq.sortBy(c => (c.since, c.key)).take(stagingCap)

      val next     = shownStaging.map(c => c.key -> c).toMap
      val upserts  = next.values.filterNot(c => before.get(c.key).contains(c)).toSeq
      val removed  = before.keySet -- next.keySet
      val open     = nowStaging.values.toSeq.flatMap(_.questions)
      val beat     = StagingBeat(at = now, staging = nowStaging.size, shown = shownStaging.size, resolved = resolvedNow.size,
        openByKind = open.filter(_.outcome != StagedQuestion.Answered).groupMapReduce(_.kind)(_ => 1)(_ + _),
        modelGaps = nowStaging.valuesIterator.map(_.modelGaps).sum)
      takeMetrics.held(clustersBy.toMap, listingsBy.toMap)
      try {
        staging.publish(country, upserts, removed, beat, resolvedNow, java.time.Duration.ofMillis(window.toMillis))
        published = Some(next)
        lastFilmOf = Some(filmOfNow.result())
        // counted once written: a failed publish diffs again, and would count them twice
        decidedNow.foreach { case (_, decision, _, how, _) => takeMetrics.changed(decision.basis.toString, how) }
        settledNow.valuesIterator.foreach(decision => takeMetrics.changed(decision.basis.toString, StagedCluster.Settled))
      } catch {
        // the next publish diffs against the store again: what it holds is unknown now
        case NonFatal(e) => published = None; throw e
      }
    }
  }

  private def stagedOf(key: String, decision: ResolverDecision, resolution: Resolution, waits: WaitingOn, since: Long,
                       known: Map[Question, QuestionMemory.Outcome]): StagedCluster = {
    val questions = waits.questions.toSeq.map(questionOf(_, known)).sortBy(q => (StagedQuestion.Outcomes.indexOf(q.outcome), q.kind, q.text))
    StagedCluster(key, listingsOf(decision), decision.members.size, takeOf(decision, resolution), waits.modelGaps, waits.onTmdb,
      questions.take(QuestionsShown), questions.size, questions.count(_.outcome != StagedQuestion.NotAttempted), since)
  }

  /** A cluster that stopped waiting: its questions as the memory last tells them — answered, or left out. */
  private def resolvedOf(was: StagedCluster, settled: Option[ResolverDecision], resolution: Resolution, now: Long,
                         known: Map[Question, QuestionMemory.Outcome]): StagedCluster = {
    val questions = was.questions.map(q => Question.parse(q.text).fold(q)(questionOf(_, known)))
    was.copy(take = settled.fold(was.take)(takeOf(_, resolution)), questions = questions,
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
  /** A settled cluster's film moving, as the metric names it: an unmatched one `gained` a film, a matched one `lost` its
   *  film, or `changed` it for another. */
  def changeOf(was: Option[Int], now: Option[Int]): String = (was, now) match {
    case (None, Some(_)) => StagedCluster.Gained
    case (Some(_), None) => StagedCluster.Lost
    case _               => StagedCluster.Changed
  }

  /** Where every publish reports the takes it read, by basis (`ResolverDecision.Basis`): the clusters and listings each
   *  basis holds now, and each cluster since the last publish `decided` (new), `settled` (seen waiting), or whose film
   *  moved ([[changeOf]]: `gained`, `lost`, `changed`). */
  trait TakeMetrics {
    def held(clusters: Map[String, Int], listings: Map[String, Int]): Unit
    def changed(basis: String, change: String): Unit
  }
  object TakeMetrics {
    val Unrecorded: TakeMetrics = new TakeMetrics {
      def held(clusters: Map[String, Int], listings: Map[String, Int]): Unit = ()
      def changed(basis: String, change: String): Unit                       = ()
    }
  }

  /** How often a country publishes. */
  val Interval: FiniteDuration = 20.seconds
  /** How long a resolved cluster stays on the screen: the database deletes it then (its `expireAt`). */
  val Window: FiniteDuration = 7.days
  /** The most clusters one publish writes as resolved: a regrouping projection's flood stays bounded. */
  val ResolvedCap = 500
  /** The most staging clusters a country lists, the longest waiting; its beat counts them all. */
  val StagingCap = 500
  /** The most questions a cluster lists; it counts them all. */
  val QuestionsShown = 60
  /** The most listings a cluster lists; it counts them all. */
  val ListingsShown = 12
  private val TickName = "identity-staging"

  /** A cluster's key on the screen: its id ([[StoredFamily.idOf]], its smallest listing), hashed short. */
  def keyOf(decision: ResolverDecision): String =
    java.lang.Long.toHexString(agreement.AgreementStage.digest(Seq(StoredFamily.idOf(decision.members))))

  private def listingsOf(decision: ResolverDecision): Seq[StagedListing] =
    decision.members.iterator.map(key => StagedListing(key.venue, key.rawTitle)).toSeq.distinct.sortBy(l => (l.venue, l.rawTitle)).take(ListingsShown)

  private def takeOf(decision: ResolverDecision, resolution: Resolution): StagedTake = {
    val leaning = decision.leaning.orElse(decision.candidate).map(_.film)
    StagedTake(decision.film, decision.film.flatMap(resolution.films.get).map(_.title), decision.basis.toString, decision.confidence,
      leaning, leaning.flatMap(resolution.films.get).map(_.title),
      decision.fallback.map(f => s"${f.source}:${f.id}${f.title.fold("")(" " + _)}"))
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
