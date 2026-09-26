package services.observations

import services.cadence.RatingCadence
import services.freshness.{Freshness, FreshnessKind}

import scala.concurrent.duration._

/**
 * How long an observation lives — one number, DERIVED from the pipeline rather than chosen
 * (docs/design/identity-resolver.md, "Phase 1: retention").
 *
 * The rule, applied by [[ObservationStore]] to every observation alike:
 *
 *  - the CURRENT observation of a key expires [[Window]] after it was last observed or last
 *    read. Every periodic job re-asks its keys at least once per [[LongestReaskPeriod]], and the
 *    shadow resolver re-reads every live family's lookups each tick, so a key that goes a whole
 *    window untouched belongs to nothing still listed;
 *  - a SUPERSEDED observation expires [[Window]] after the observation that replaced it, so a
 *    change can always be compared with what it replaced for at least one full re-ask cycle.
 *
 * Twice the longest re-ask period, so one missed cycle (a worker down for a deploy, a breaker
 * open) never expires a live key. The period is the longest of the freshness windows and the
 * rating cadence's ceiling; raising either raises this with it.
 */
object ObservationRetention {

  val LongestReaskPeriod: FiniteDuration =
    (RatingCadence.MaxInterval +: FreshnessKind.all.flatMap(Freshness.ttlFor)).max

  val Window: FiniteDuration = LongestReaskPeriod * 2
}
