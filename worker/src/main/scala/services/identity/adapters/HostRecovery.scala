package services.identity
package adapters

import play.api.Logging
import tools.HostCircuitBreakerHttpFetch.Transition

/**
 * Asks again, at once, the identity questions that failed against a host that is back: what the owner asked for on
 * 2026-10-24, so the matches waiting on an IMDb, Wikidata, Filmweb, RT, Metacritic or poster host that went away update
 * as soon as it answers again, not after their failure wait (5 minutes, doubling to 6 hours) runs out.
 *
 * The enrichment chain's host breakers say when a host is back ([[Transition]], heard through [[heard]]): its breaker
 * closed after an outage, or it answered after at least [[QuestionMemory.MinOutageReads]] failures in a row that never
 * opened it — fewer is a question failing alone, which keeps its own backoff, the same line the fill's rounds draw
 * ([[QuestionMemory.roundEnded]]). On either, the memory marks the host recovered ([[QuestionMemory.recovered]]): every
 * failure marked against it before is due again. The open agreement questions (`open`, the stage's last apply's) whose
 * last ask failed against it are handed to `askAgain`, which queues them — the stage hands a question over once per
 * answers' version, so without this they waited for the next version — and the fill is asked for a round
 * (`fill`), which asks the model's due gaps at its own rate — those whose read failed against the host among them,
 * marked against it as the agreement's are.
 *
 * Nothing here sends a request: the queue and the fill ask at their own pace, through the same 429 gate, breaker and
 * fleet pace as before, so a host that flaps re-queues its questions at most once per breaker cooldown, each queued once.
 */
final class HostRecovery(memory: QuestionMemory, open: () => Set[Question], askAgain: Set[Question] => Unit, fill: () => Unit,
                         report: String => Unit = HostRecovery.log) {

  /** A host's breaker changed state: a host back is [[recovered]], any other transition nothing. */
  def heard(host: String, transition: Transition): Unit = if (HostRecovery.recovers(transition)) { recovered(host); () }

  /** `host` is back: its failures released and the open questions that failed against it queued again — those queued. */
  def recovered(host: String): Set[Question] = {
    val again = memory.failedAgainst(host, open())
    memory.recovered(host)
    if (again.nonEmpty) askAgain(again)
    fill()
    report(s"$host back: ${again.size} questions asked again")
    again
  }
}

object HostRecovery extends Logging {
  /** Whether `transition` is a host coming back: a breaker closing, or an answer after a run of failures long enough to
   *  call an outage. Read on the request's thread, before anything is handed off. */
  def recovers(transition: Transition): Boolean = transition match {
    case Transition.Closed             => true
    case Transition.Answered(failures) => failures >= QuestionMemory.MinOutageReads
    case Transition.Opened             => false
  }

  private def log(line: String): Unit = logger.info(s"identity questions: $line")
}
