package integration

import scala.concurrent.duration._

object RetryWithBackoff {

  // 30s default: the live MC/RT probes (MovieSitesIntegrationSpec) hit Cloudflare-
  // fronted sites that 502 / serve a JS challenge in bursts lasting longer than the
  // former 10s budget — `canonicalUrl` swallows a non-2xx as `None`, so a sustained
  // burst collapsed a real `Some(url)` to `None` for the whole window and flaked the
  // build (Metacritic, "Yu-Gi-Oh! The Dark Side of Dimensions"). 30s clears the
  // typical burst while still failing fast on a genuine slug-convention regression.
  def apply[T](
    totalBudget: FiniteDuration = 30.seconds,
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
