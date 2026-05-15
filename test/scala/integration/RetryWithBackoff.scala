package integration

import scala.concurrent.duration._

object RetryWithBackoff {

  def apply[T](
    totalBudget: FiniteDuration = 10.seconds,
    initialDelay: FiniteDuration = 100.millis,
    sleep: Long => Unit = Thread.sleep,
    now: () => Long = () => System.currentTimeMillis(),
  )(block: => T): T = {
    val deadline = now() + totalBudget.toMillis
    var delayMs = initialDelay.toMillis
    while (true) {
      try return block
      catch {
        case t: Throwable =>
          val remaining = deadline - now()
          if (remaining <= 0) throw t
          sleep(math.min(delayMs, remaining))
          delayMs *= 2
      }
    }
    throw new IllegalStateException("unreachable")
  }
}
