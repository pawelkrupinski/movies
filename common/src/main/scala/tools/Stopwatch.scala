package tools

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
 * How long code takes, on a MONOTONIC clock (`System.nanoTime` by default): the wall clock can
 * step under NTP and give a negative or inflated duration. One place for the
 * `val t0 = System.nanoTime(); …; (System.nanoTime() - t0) / 1e9` shape, in its three uses:
 *
 *  - [[start]] — a reading to ask later: a failure path, an async callback;
 *  - [[timed]] — a block's value and how long it took;
 *  - [[total]] — the time every run of a block took, summed from any thread (a prefetch's
 *    seconds across a take-up).
 *
 * `nanoTime` is injected so a spec drives the durations; production uses [[Stopwatch.System]].
 */
final class Stopwatch(nanoTime: () => Long) {
  def start(): Stopwatch.Started = new Stopwatch.Started(nanoTime(), nanoTime)

  def timed[A](body: => A): Stopwatch.Timed[A] = {
    val started = start()
    val value   = body
    Stopwatch.Timed(value, started.elapsed)
  }

  def total(): Stopwatch.Total = new Stopwatch.Total(this)
}

object Stopwatch {
  val System: Stopwatch = new Stopwatch(() => java.lang.System.nanoTime())

  def start(): Started              = System.start()
  def timed[A](body: => A): Timed[A] = System.timed(body)
  def total(): Total                 = System.total()

  /** A reading taken at the start; each accessor reads the clock again. */
  final class Started private[Stopwatch] (startNanos: Long, nanoTime: () => Long) {
    def elapsed: FiniteDuration = Duration.fromNanos(nanoTime() - startNanos)
    def seconds: Double         = Stopwatch.seconds(elapsed)
    def millis: Long            = elapsed.toMillis
  }

  final case class Timed[A](value: A, elapsed: FiniteDuration) {
    def seconds: Double = Stopwatch.seconds(elapsed)
    def millis: Long    = elapsed.toMillis
  }

  /** The time every run of a block took, and how many runs — from any thread, a failed run included. */
  final class Total private[Stopwatch] (stopwatch: Stopwatch) {
    private val nanos = new AtomicLong
    private val runs  = new AtomicLong

    def apply[A](body: => A): A = {
      val started = stopwatch.start()
      try body finally { nanos.addAndGet(started.elapsed.toNanos); runs.incrementAndGet(); () }
    }

    def elapsed: FiniteDuration = Duration.fromNanos(nanos.get)
    def seconds: Double         = Stopwatch.seconds(elapsed)
    def count: Long             = runs.get
  }

  private def seconds(elapsed: FiniteDuration): Double = elapsed.toNanos / 1e9
}
