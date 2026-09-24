package modules.wiring

import modules.WorkerWiring
import services.readmodel.ReadModelContentAudit
import services.sharecards.ShareCardAudit
import services.tasks.{ClaimedEnqueueReaper, RecheckedAudit, RecheckedAuditHandler, TaskHandler, TaskType}
import tools.Env

import scala.concurrent.duration.*

/** ── Runtime invariants checked by sample ─────────────────────────────────────
 *  Two hourly audits on the task queue, each a claimed recurring enqueue (one replica per window)
 *  whose finds are re-checked fifteen minutes later before they count — see [[RecheckedAudit]]:
 *   - READ-MODEL CONTENT: sampled cards against what the projection derives from their source
 *     rows now ([[ReadModelContentAudit]]);
 *   - SHARE CARDS: sampled `web_movies.shareCard` pointers against the card directory
 *     ([[ShareCardAudit]]) — only where this worker has the directory. */
trait InvariantAuditWiring { self: WorkerWiring =>

  lazy val readModelContentAudit: RecheckedAudit =
    new RecheckedAudit("read-model-content", TaskType.AuditReadModelContent, taskQueue,
      workerMetrics.readModelContentAudit.forCountry(country.code), clock,
      sampleSize = Env.positiveInt("KINOWO_READMODEL_AUDIT_SAMPLE", 50))(
      ReadModelContentAudit.differences(_, movieRepository, readModelRepository))

  lazy val shareCardAudit: RecheckedAudit =
    new RecheckedAudit("share-card", TaskType.AuditShareCards, taskQueue,
      workerMetrics.shareCardAudit.forCountry(country.code), clock,
      sampleSize = Env.positiveInt("KINOWO_SHARE_CARD_AUDIT_SAMPLE", 50))(
      ShareCardAudit.check(_, readModelRepository, shareCardStore))

  lazy val auditHandlers: Seq[TaskHandler] =
    Seq(new RecheckedAuditHandler(TaskType.AuditReadModelContent, readModelContentAudit, () => {
      val (ids, complete) = readModelRepository.findAllMovieIdsChecked()
      Option.when(complete)(ids)
    })) ++
    Option.when(shareCardsEnabled)(
      new RecheckedAuditHandler(TaskType.AuditShareCards, shareCardAudit, () => ShareCardAudit.ids(readModelRepository)))

  /** Hourly each, the first well after boot so it never lands on the boot heal. */
  lazy val auditReapers: Seq[ClaimedEnqueueReaper] = {
    def enqueue(taskType: TaskType, key: String): () => Unit =
      () => { taskQueue.enqueue(taskType, key, submittedAt = clock.instant()); () }
    Seq(new ClaimedEnqueueReaper("read-model-content-audit",
      enqueue(TaskType.AuditReadModelContent, "read-model-content-audit"), 1.hour, 20.minutes, scheduledRunStore, clock)) ++
    Option.when(shareCardsEnabled)(new ClaimedEnqueueReaper("share-card-audit",
      enqueue(TaskType.AuditShareCards, "share-card-audit"), 1.hour, 25.minutes, scheduledRunStore, clock))
  }
}
