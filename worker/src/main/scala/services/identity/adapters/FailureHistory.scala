package services.identity
package adapters

import tools.HttpStatusException

import scala.util.{Failure, Try}

/** Where a TMDB or IMDb read that filed no answer — a 429, a timeout, the circuit open — is kept ([[TmdbNormalizer]]):
 *  the answer a take went without. A durable non-answer (a 404) is an answer, filed by the store, never kept here. */
trait FailureHistory {
  def kept(url: String, outcome: Try[String]): Unit
}

object FailureHistory {
  val Discard: FailureHistory = (_, _) => ()
  /** How often one URL's failure of one kind is kept at most: an outage's retries are one entry a minute, not each. */
  val Every: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(1, "minute")
  /** How many URLs' last failures are remembered before the old ones are forgotten. */
  private[adapters] val Held = 10000
}

/** Each failure into `history` ([[HistoryEntry.Failure]], by its redacted URL and what failed), once a minute per URL and
 *  kind of failure ([[FailureHistory.Every]]). */
final class KeptFailures(history: IdentityHistory, clock: java.time.Clock, build: String) extends FailureHistory {
  private val failedAt = new java.util.concurrent.ConcurrentHashMap[(String, String), java.lang.Long]()

  def kept(url: String, outcome: Try[String]): Unit = outcome match {
    case Failure(e) if !tools.ReadOutcome.isAbsent(e) =>
      val now  = clock.millis()
      val kind = e match {
        case status: HttpStatusException => s"http ${status.code}"
        case other                       => other.getClass.getSimpleName
      }
      val key  = (tools.RedactedUrl(url), kind)
      val last = failedAt.put(key, now)
      if (last == null || now - last >= FailureHistory.Every.toMillis) {
        if (failedAt.size > FailureHistory.Held) failedAt.entrySet.removeIf(_.getValue < now - FailureHistory.Every.toMillis)
        history.append(Seq(HistoryEntry(HistoryEntry.Failure, key._1, now, None, None, build, Some(kind))))
      }
    case _ => ()
  }
}
