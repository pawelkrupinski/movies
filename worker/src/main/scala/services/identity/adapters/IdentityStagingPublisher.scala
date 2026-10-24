package services.identity
package adapters

import play.api.Logging
import services.Stoppable
import services.identity.staging.{IdentityStaging, StagedCluster, StagedListing, StagedQuestion, StagedTake, StagingBeat}
import tools.{DaemonExecutors, ScheduledTick, TickRecorder}

import java.time.Clock
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
 * Publishes what this country's identity takes still wait on, for the staging screen (`/admin/identity/staging`,
 * docs/design/identity-questions.md "The staging screen"): the stage's waits live in the worker's memory
 * ([[agreement.AgreementStage.waitingOn]]), not in Mongo, so nothing else can show them.
 *
 * Each [[publish]] reads the takes the projection last wrote (`taken`, shared with the projection, nothing copied) and
 * what each waits on: a cluster whose [[WaitingOn]] is not settled STAGES, with each open question and what the
 * question memory says of its last ask (`outcome`); a cluster that staged on the last publish and waits on nothing now
 * is RESOLVED — `settled` with the take it got, or `regrouped` when its listings no longer form it — and kept for
 * `window`, the newest `resolvedCap` of them. Only clusters that changed since the last publish are written, and those
 * gone deleted: a quiet country writes its beat and nothing else.
 *
 * Off the projection's thread, on its own schedule (`start`): the projection's quiet tick allocates nothing for it.
 */
final class IdentityStagingPublisher(country: String, staging: IdentityStaging, taken: () => Option[Resolution],
                                     waitingOn: ResolverDecision => WaitingOn,
                                     outcomes: Set[Question] => Map[Question, QuestionMemory.Outcome], clock: Clock,
                                     window: FiniteDuration = IdentityStagingPublisher.Window,
                                     resolvedCap: Int = IdentityStagingPublisher.ResolvedCap,
                                     stagingCap: Int = IdentityStagingPublisher.StagingCap,
                                     ticks: TickRecorder = TickRecorder.Unrecorded) extends Stoppable with Logging {
  import IdentityStagingPublisher.*

  /** What the last publish left in the store, by key; read back from it once, on the first publish after a boot. */
  private var published: Option[Map[String, StagedCluster]] = None
  /** Each cluster's film as the last publish saw it, by key: what tells a cluster decided or moved since — most settle
   *  inside one projection (a new film's questions are asked inline), never seen staging. None until the first publish
   *  after a boot, which only records them: everything is new to a worker just started. */
  private var lastTakes: Option[Map[String, Option[Int]]] = None
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
      val wasStaging = before.valuesIterator.filter(_.staging).map(c => c.key -> c).toMap

      val waiting  = Seq.newBuilder[(String, ResolverDecision, WaitingOn)]
      val settled  = Map.newBuilder[String, ResolverDecision]
      val decided  = Seq.newBuilder[(String, ResolverDecision, WaitingOn, String)]
      val takesNow = Map.newBuilder[String, Option[Int]]
      resolution.decisions.foreach { decision =>
        if (decision.members.nonEmpty) {
          val key   = keyOf(decision)
          val waits = waitingOn(decision)
          takesNow += key -> decision.film
          if (!waits.settled) waiting += ((key, decision, waits))
          else if (wasStaging.contains(key)) settled += key -> decision
          else lastTakes.foreach(_.get(key) match {
            case None                                => decided += ((key, decision, waits, StagedCluster.Decided))
            case Some(film) if film != decision.film => decided += ((key, decision, waits, StagedCluster.Moved))
            case _                                   => ()
          })
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
      val resolvedNow = wasStaging.collect { case (key, was) if !nowStaging.contains(key) =>
        key -> resolvedOf(was, settledNow.get(key), resolution, now, known)
      } ++ decidedNow.map { case (key, decision, waits, how) =>
        // decided between two publishes, never seen waiting: its take, and no open question (it settled)
        key -> stagedOf(key, decision, resolution, waits, now, known).copy(resolvedAt = Some(now), resolution = Some(how))
      }
      val keptResolved = (before.valuesIterator.filter(c => !c.staging && !nowStaging.contains(c.key)).map(c => c.key -> c).toMap ++ resolvedNow)
        .values.toSeq.filter(_.resolvedAt.exists(_ > now - window.toMillis)).sortBy(c => -c.resolvedAt.getOrElse(0L)).take(resolvedCap)
      val shownStaging = nowStaging.values.toSeq.sortBy(c => (c.since, c.key)).take(stagingCap)

      val next     = (shownStaging ++ keptResolved).map(c => c.key -> c).toMap
      val upserts  = next.values.filterNot(c => before.get(c.key).contains(c)).toSeq
      val removed  = before.keySet -- next.keySet
      val open     = nowStaging.values.toSeq.flatMap(_.questions)
      val beat     = StagingBeat(at = now, staging = nowStaging.size, shown = shownStaging.size, resolved = keptResolved.size,
        openByKind = open.filter(_.outcome != StagedQuestion.Answered).groupMapReduce(_.kind)(_ => 1)(_ + _),
        modelGaps = nowStaging.valuesIterator.map(_.modelGaps).sum)
      try {
        staging.publish(country, upserts, removed, beat)
        published = Some(next)
        lastTakes = Some(takesNow.result())
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
  /** How often a country publishes. */
  val Interval: FiniteDuration = 20.seconds
  /** How long a resolved cluster stays on the screen. */
  val Window: FiniteDuration = 6.hours
  /** The most resolved clusters a country keeps, the newest. */
  val ResolvedCap = 200
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
