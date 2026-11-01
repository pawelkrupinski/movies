package services.identity.staging

/**
 * What the identity system tells the staging screen as it happens, held until its publisher drains it: a bounded
 * first-in-first-out buffer between the threads that change takes (the projection) and the one that writes them out
 * (`adapters.IdentityStagingPublisher`). Full, it lets the oldest go and counts them: a publisher that stopped draining
 * never holds the worker's memory, and the next drain says how many it missed.
 */
final class ChangeFeed[A](capacity: Int) {
  require(capacity > 0, "a feed holds at least one change")
  private val held    = new java.util.ArrayDeque[A]()
  private var dropped = 0L

  /** `changes` appended, the oldest let go past the capacity. */
  def offer(changes: Iterable[A]): Unit = synchronized {
    changes.foreach { change =>
      if (held.size == capacity) { held.pollFirst(); dropped += 1 }
      held.addLast(change)
    }
  }

  /** Everything held, oldest first, and how many were let go since the last drain; the feed empty after. */
  def drain(): ChangeFeed.Drained[A] = synchronized {
    val all = Vector.newBuilder[A]
    while (!held.isEmpty) all += held.pollFirst()
    val drained = ChangeFeed.Drained(all.result(), dropped)
    dropped = 0
    drained
  }
}

object ChangeFeed {
  /** One drain: the changes, oldest first, and how many were let go before it could read them. */
  final case class Drained[A](changes: Vector[A], dropped: Long)
}
