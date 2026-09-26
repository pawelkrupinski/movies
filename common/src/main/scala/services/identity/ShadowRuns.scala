package services.identity

import java.time.{Clock, Instant}
import scala.concurrent.duration.FiniteDuration

/** Where curation reads the resolver's latest decisions and its labelled shadow diff — the read
 *  side of [[ShadowRunStore]], which the shadow run (`ShadowIdentityReaper`) writes. */
trait ShadowDecisions {
  /** Every decision of the latest shadow resolve. */
  def latest(): Seq[Decision]
  /** The latest shadow diff's decisions whose correctness is known — the pipeline agreeing, or
   *  naming another film for exactly the same listings ([[ShadowCluster.agrees]]) — as the
   *  calibration input of the admin view's cut. */
  def verdicts(): Seq[ConfidenceCalibration.Sample]
}

/** One shadow resolve, as persisted: when it ran, every cluster with its relation to the
 *  pipeline's films, and the families where the two part ways. */
final case class ShadowRun(at: Instant, clusters: Seq[ShadowCluster], families: Seq[ShadowFamily])

/** How long a shadow run stays readable after it ran. The shadow run's own clock is the
 *  observations' retention (`ObservationRetention.Window`, §9a): a run older than that was
 *  computed from answers that may since have expired, and a stopped reaper's output ages out
 *  instead of posing as current. */
final case class ShadowRetention(value: FiniteDuration) extends AnyVal

/**
 * The storage seam of the shadow runs, and nothing else: keep one run (with the instant it
 * expires), hand back the latest. `MongoShadowRunBackend` keeps it in `identity_shadow_decisions`
 * and `identity_shadow_diff`; [[InMemoryShadowRunBackend]] in a field. Neither decides anything —
 * expiry and verdicts are [[ShadowRunStore]]'s.
 */
trait ShadowRunBackend {
  /** Keep `run` as the latest, expiring at `expireAt`, and drop every earlier run. */
  def replace(run: ShadowRun, expireAt: Instant): Unit
  /** The latest run kept, with its expiry, expired or not — the store filters. */
  def latest(): Option[(ShadowRun, Instant)]
}

final class InMemoryShadowRunBackend extends ShadowRunBackend {
  @volatile private var kept: Option[(ShadowRun, Instant)] = None
  def replace(run: ShadowRun, expireAt: Instant): Unit = kept = Some(run -> expireAt)
  def latest(): Option[(ShadowRun, Instant)]           = kept
}

/**
 * The shadow resolver's persisted output (docs/design/identity-resolver.md §8 and §13.4) — every
 * rule over the [[ShadowRunBackend]] seam, so the Mongo and in-memory backends cannot disagree:
 *
 *  - only the LATEST run is kept: each run replaces the previous one whole, so a reader never
 *    mixes two resolves' decisions;
 *  - a run is readable for [[ShadowRetention]] after it ran and not a moment longer: reads
 *    filter by the stamp, so the backend's deletion lag (Mongo's TTL monitor) is invisible;
 *  - the verdicts are the clusters whose correctness the pipeline settles ([[ShadowCluster.agrees]]).
 *
 * Written only by the worker's shadow run; nothing that serves reads it — the admin view is a
 * diagnostic.
 */
final class ShadowRunStore(backend: ShadowRunBackend, clock: Clock) extends ShadowDecisions {

  def record(run: ShadowRun, retention: ShadowRetention): Unit =
    backend.replace(run, run.at.plusMillis(retention.value.toMillis))

  /** The latest run, while it is live. */
  def latestRun(): Option[ShadowRun] = backend.latest().collect { case (run, expireAt) if expireAt.isAfter(clock.instant()) => run }

  def latest(): Seq[Decision] = latestRun().toSeq.flatMap(_.clusters.map(_.decision))

  def verdicts(): Seq[ConfidenceCalibration.Sample] =
    latestRun().toSeq.flatMap(_.clusters.flatMap(c => c.agrees.map(ConfidenceCalibration.Sample(c.decision.confidence, _))))
}

object ShadowRunStore {
  def inMemory(clock: Clock): ShadowRunStore = new ShadowRunStore(new InMemoryShadowRunBackend, clock)

  /** The shadow collections. Nothing serving reads them. */
  val DecisionsCollection = "identity_shadow_decisions"
  val DiffCollection      = "identity_shadow_diff"
}
