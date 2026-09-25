package tools

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

/**
 * At most one log line per `intervalNanos`, for a caller that would otherwise flood the log with
 * the same kind of line. An instance, owned by whoever logs — never a global — so two owners (or
 * two specs) never spend each other's allowance.
 *
 * `nanoTime` is monotonic nanoseconds, injected so a spec can step it.
 */
final class LogThrottle(intervalNanos: Long, nanoTime: () => Long = () => System.nanoTime()) {
  private val lastAdmitted = new AtomicLong(Long.MinValue)
  private val suppressed   = new AtomicInteger(0)

  /** `Some(n)` when this call may log — `n` being how many calls were held back since the last
   *  admitted one — else `None`. The first call is always admitted. */
  def admit(): Option[Int] = {
    val now  = nanoTime()
    val last = lastAdmitted.get()
    if ((last == Long.MinValue || now - last >= intervalNanos) && lastAdmitted.compareAndSet(last, now))
      Some(suppressed.getAndSet(0))
    else { suppressed.incrementAndGet(); None }
  }
}
