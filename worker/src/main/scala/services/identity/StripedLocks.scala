package services.identity

import java.util.concurrent.locks.ReentrantLock

/** A lock per key, striped over `stripes` locks: work on one key is serial, work on different keys
 *  proceeds side by side (bar a stripe they happen to share). Several keys are locked in one order,
 *  so two holders never deadlock. */
private[identity] final class StripedLocks(stripes: Int = StripedLocks.DefaultStripes) {
  private val locks = Array.fill(stripes)(new ReentrantLock())

  /** `body` holding the locks of every key in `keys`. */
  def locking[A](keys: Iterable[String])(body: => A): A = {
    val held = keys.iterator.map(key => Math.floorMod(key.hashCode, stripes)).toSeq.distinct.sorted.map(locks)
    held.foreach(_.lock())
    try body finally held.reverseIterator.foreach(_.unlock())
  }
}

private[identity] object StripedLocks {
  /** Enough that the threads a take-up or a scrape walk runs on rarely meet on one stripe. */
  val DefaultStripes = 1024
}
