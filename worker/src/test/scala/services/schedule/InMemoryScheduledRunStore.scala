package services.schedule

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/** In-memory [[ScheduledRunStore]] with real claim-once-per-id semantics — the
 *  first `claim` for an id wins, repeats lose. The single-machine analogue used
 *  to exercise the reaper gating without Mongo. */
class InMemoryScheduledRunStore extends ScheduledRunStore {
  private val claimed = ConcurrentHashMap.newKeySet[String]()
  def claim(occurrenceId: String): Boolean = claimed.add(occurrenceId)
  def claimedIds: Set[String] = claimed.asScala.toSet
}

/** [[ScheduledRunStore]] that never grants a claim — stands in for "another
 *  machine already owns every occurrence", so the caller always skips. */
object NeverClaimScheduledRunStore extends ScheduledRunStore {
  def claim(occurrenceId: String): Boolean = false
}
