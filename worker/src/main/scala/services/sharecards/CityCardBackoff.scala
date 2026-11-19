package services.sharecards

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap

/**
 * The cities whose last render failed or was refused, each held out of the reaper's ticks until its
 * wait is over: [[CityCardService.FirstFailureRetry]] after the first miss, doubling per further
 * miss up to [[CityCardService.MaxFailureRetry]]. A drawn card forgets the city. In memory, per
 * service: a restart asks every held city once more, which costs one render each.
 */
final class CityCardBackoff(clock: Clock) {
  import CityCardService.{FirstFailureRetry, MaxFailureRetry}
  private final case class Miss(at: Instant, waitMillis: Long)
  private val misses = new ConcurrentHashMap[String, Miss]()

  /** True while the city's latest miss is inside its wait. */
  def holds(slug: String, now: Instant): Boolean =
    Option(misses.get(slug)).exists(miss => now.isBefore(miss.at.plusMillis(miss.waitMillis)))

  def missed(slug: String): Unit = {
    misses.compute(slug, (_, previous) => {
      val waitMillis = Option(previous).fold(FirstFailureRetry.toMillis)(p => (p.waitMillis * 2).min(MaxFailureRetry.toMillis))
      Miss(clock.instant(), waitMillis)
    })
    ()
  }

  def drawn(slug: String): Unit = { misses.remove(slug); () }
}
