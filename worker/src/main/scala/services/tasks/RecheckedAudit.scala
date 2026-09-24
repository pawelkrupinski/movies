package services.tasks

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import play.api.Logging

import java.time.Clock
import scala.concurrent.duration.*
import scala.util.Random

/**
 * A RUNTIME INVARIANT, CHECKED BY SAMPLE: every run draws `sampleSize` ids at random, asks `check`
 * whether each still holds, and counts a violation only once it has SURVIVED A RE-CHECK
 * `recheckAfter` later.
 *
 * The re-check is what lets the alert on it be strict. Each invariant here compares two things
 * the pipeline writes a moment apart — a source row and the card projected from it, a rendered
 * file and the `web_movies` field naming it — so a sample landing between the two writes sees a
 * difference that is simply in flight. Seen again a quarter of an hour later, it is not in flight.
 *
 * THE RE-CHECK IS A TASK, NOT A MEMORY. The recurring run is enqueued by one replica's claimed
 * tick but executed by whichever replica's TaskWorker claims it, so suspects held in memory would
 * be lost half the time. Carried in a delayed task's payload, they are re-checked by whoever runs
 * next, and a restart in between loses nothing.
 *
 * `check` answers None when the id cannot be judged at all (a read failed, or the id is no longer
 * one the invariant is about) — that is not a pass, and it is not counted — `Some(Nil)` when the
 * invariant holds, and `Some(problems)` naming what differs when it does not.
 */
final class RecheckedAudit(
  name:         String,
  taskType:     TaskType,
  queue:        TaskQueue,
  metrics:      RecheckedAudit.Metrics,
  clock:        Clock,
  sampleSize:   Int,
  recheckAfter: FiniteDuration = RecheckedAudit.RecheckAfter,
  random:       Random         = new Random()
)(check: String => Option[Seq[String]]) extends Logging {
  import RecheckedAudit.*

  /** The recurring run: sample `ids`, and queue a re-check of whatever violated. Returns the ids
   *  queued for the re-check. */
  def sample(ids: Seq[String]): Seq[String] = {
    // Twice the sample drawn at most, so a run over ids that mostly cannot be judged still ends.
    val judged   = random.shuffle(ids).iterator.take(sampleSize * 2).flatMap(id => check(id).map(id -> _)).take(sampleSize).toSeq
    val suspects = judged.collect { case (id, problems) if problems.nonEmpty => id }
    metrics.audited(judged.size)
    metrics.suspected(suspects.size)
    if (suspects.nonEmpty) {
      // Each sample's suspects under a field of their own, so a re-check still WAITING (one
      // dedup key per audit) takes them on beside its own instead of the enqueue deduping them
      // away unchecked.
      val dedupKey = s"$name-recheck"
      val fields   = Map(s"$IdsKey.${java.util.UUID.randomUUID()}" -> suspects.mkString(Separator))
      val queued = queue.enqueue(taskType, dedupKey, fields,
        submittedAt = clock.instant(), notBefore = Some(clock.instant().plusMillis(recheckAfter.toMillis))) match {
        case EnqueueResult.Duplicate if queue.amendWaiting(dedupKey, fields) => "joined the waiting re-check"
        case EnqueueResult.Duplicate => "NOT queued: the re-check is running"
        case other                   => other.toString
      }
      logger.info(s"$name audit: ${suspects.size} of ${judged.size} sampled differ, re-checked in ${recheckAfter.toMinutes}m " +
        s"($queued): ${suspects.take(LoggedIds).mkString(", ")}")
    }
    suspects
  }

  /** The re-check: every id that still violates is CONFIRMED — counted, and logged with what
   *  differs. Returns the confirmed ids with their problems. */
  def recheck(ids: Seq[String]): Seq[(String, Seq[String])] = {
    val confirmed = ids.flatMap(id => check(id).filter(_.nonEmpty).map(id -> _))
    metrics.confirmed(confirmed.size)
    confirmed.take(LoggedIds).foreach { case (id, problems) =>
      logger.warn(s"$name audit: $id still differs ${recheckAfter.toMinutes}m after it was first seen: ${problems.mkString("; ")}")
    }
    if (confirmed.sizeIs > LoggedIds) logger.warn(s"$name audit: ${confirmed.size - LoggedIds} more confirmed, not listed.")
    confirmed
  }

  /** Run whichever of the two a task asks for: a re-check when it carries ids, else a sample of
   *  what `ids` returns (None when those could not be read completely — nothing is sampled). */
  def handle(task: Task, ids: () => Option[Seq[String]]): HandlerOutcome =
    carriedIds(task.payload) match {
      case Some(carried) => recheck(carried); HandlerOutcome.Done
      case None => ids() match {
        case Some(all) => sample(all); HandlerOutcome.Done
        case None =>
          logger.warn(s"$name audit: the id read did not complete — nothing sampled this run.")
          HandlerOutcome.Skipped
      }
    }
}

object RecheckedAudit {
  /** The ids a re-check task carries — every sample's that joined it — or None for a sample task. */
  private def carriedIds(payload: Map[String, String]): Option[Seq[String]] = {
    val fields = payload.collect { case (k, v) if k == IdsKey || k.startsWith(s"$IdsKey.") => v }
    Option.when(fields.nonEmpty)(fields.iterator.flatMap(_.split(Separator)).filter(_.nonEmpty).toSeq.distinct)
  }

  /** Longer than any in-flight gap it has to see past: the change stream applies a write in
   *  seconds (its apply-lag alert is at ten minutes), a finished render re-projects its film at
   *  once. A difference still there after fifteen minutes is not in flight. */
  val RecheckAfter: FiniteDuration = 15.minutes
  val IdsKey    = "ids"
  private val Separator = "\n"
  private val LoggedIds = 20

  /** What an audit counts: ids judged by a sample, those that differed on it, and those that
   *  still differed at their re-check. The rule reads confirmed over audited. */
  trait Metrics {
    def audited(n: Int): Unit
    def suspected(n: Int): Unit
    def confirmed(n: Int): Unit
  }

  /** One audit's three counters, `<prefix>_audited_total`, `<prefix>_audit_suspects_total` and
   *  `<prefix>_audit_confirmed_total`, registered once with a leading `country` label (see
   *  [[services.metrics.WorkerMetrics]]). Counters, not a gauge of the last run's findings: a run
   *  executes on whichever replica claims it, and a gauge would freeze at its last value on the
   *  replica that stopped running it. Counters from every replica `sum` correctly. */
  final class Series(prefix: String, invariant: String, countryCodes: Seq[String], registry: PrometheusRegistry) {
    private val auditedCounter = Counter.builder().name(s"${prefix}_audited")
      .help(s"Ids sampled and judged by the $invariant audit.").labelNames("country").register(registry)
    private val suspectsCounter = Counter.builder().name(s"${prefix}_audit_suspects")
      .help(s"Sampled ids that broke the $invariant invariant when sampled, queued for a re-check fifteen minutes later.")
      .labelNames("country").register(registry)
    private val confirmedCounter = Counter.builder().name(s"${prefix}_audit_confirmed")
      .help(s"Ids that STILL broke the $invariant invariant at their re-check: a violation, not a write in flight.")
      .labelNames("country").register(registry)
    countryCodes.foreach { c => auditedCounter.labelValues(c); suspectsCounter.labelValues(c); confirmedCounter.labelValues(c) }

    def forCountry(code: String): Metrics = new Metrics {
      def audited(n: Int): Unit   = if (n > 0) auditedCounter.labelValues(code).inc(n.toDouble)
      def suspected(n: Int): Unit = if (n > 0) suspectsCounter.labelValues(code).inc(n.toDouble)
      def confirmed(n: Int): Unit = if (n > 0) confirmedCounter.labelValues(code).inc(n.toDouble)
    }

    /** Test seam: (audited, suspects, confirmed) so far for `code`. */
    def counts(code: String): (Double, Double, Double) =
      (auditedCounter.labelValues(code).get(), suspectsCounter.labelValues(code).get(), confirmedCounter.labelValues(code).get())
  }
}

/** Runs one [[RecheckedAudit]]'s task: a sample over what `ids` reads, or the re-check a sample
 *  queued. */
final class RecheckedAuditHandler(val taskType: TaskType, audit: RecheckedAudit, ids: () => Option[Seq[String]]) extends TaskHandler {
  def handle(task: Task): HandlerOutcome = audit.handle(task, ids)
}
