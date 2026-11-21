package tools

import java.time.{Clock, Instant}

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

import play.api.Logging

/** A lookup read once and kept for the process's life, like a lazy val — except that a read that FAILS is not kept:
 *  `fallback` stands in, the failure is logged, and the read is tried again on the first ask `retryAfter` later. For a
 *  boot-time lookup whose one failed read (a network hiccup) must not stand for the life of the process. Callers
 *  asking while a read runs wait for it, as on a lazy val. */
final class RetriedLookup[A](label: String, read: () => A, fallback: A, retryAfter: FiniteDuration, clock: Clock) extends Logging {
  @volatile private var held: Option[A] = None
  private var nextTry: Instant = Instant.MIN

  /** The value read, or `fallback` while no read has succeeded. */
  def current: A = held.getOrElse(synchronized(held.getOrElse(attempt())))

  private def attempt(): A = {
    val now = clock.instant()
    if (now.isBefore(nextTry)) fallback
    else
      try { val value = read(); held = Some(value); value }
      catch {
        case NonFatal(exception) =>
          nextTry = now.plusMillis(retryAfter.toMillis)
          logger.warn(s"$label failed (${exception.getMessage}) — trying again after $nextTry.")
          fallback
      }
  }
}
