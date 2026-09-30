package services.movies

import org.bson.BsonDocument

/**
 * One change-stream cursor's delivered-but-not-yet-applied events, in the order the cursor
 * delivered them — and the one position it may persist: the LAST event every one of whose
 * predecessors has been applied too.
 *
 * A cursor has ONE resume position, and a restart replays only what lies after it. Moving it to
 * each event as its apply finished was safe only while applies finished in delivery order; a
 * film whose re-read waits out its burst ([[MovieChangeStream]]'s debounce), or rides another
 * event's re-read, finishes out of that order — and moving the position to a later event then
 * moved it past an earlier one still waiting, which a crash lost. Here the position moves only
 * over a contiguous run of applied events, so a restart replays every event still waiting: a
 * harmless re-read of the film's current state.
 */
final class AppliedPrefix(advance: (BsonDocument, Long) => Unit) {
  private final class Entry(val token: BsonDocument, val generation: Long) { var done = false }
  private val delivered = scala.collection.mutable.Queue.empty[Entry]

  /** Record an event as its cursor delivers it — call in delivery order. The returned
   *  acknowledgement marks it applied, and may be called from any thread, any number of times. */
  def deliver(token: BsonDocument, generation: Long): () => Unit = {
    val entry = new Entry(token, generation)
    synchronized(delivered.enqueue(entry))
    () => applied(entry)
  }

  // The position is advanced under the same lock that orders the queue, so two acknowledgements
  // racing on different threads can never move it backwards.
  private def applied(entry: Entry): Unit = synchronized {
    entry.done = true
    var last = Option.empty[Entry]
    while (delivered.headOption.exists(_.done)) last = Some(delivered.dequeue())
    last.foreach(e => advance(e.token, e.generation))
  }

  /** How many delivered events still wait on an apply. */
  def waiting: Int = synchronized(delivered.count(!_.done))
}
