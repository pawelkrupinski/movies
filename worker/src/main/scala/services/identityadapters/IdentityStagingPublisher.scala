package services.identityadapters

import services.identity.measures.Resolution
import play.api.Logging
import tools.Stoppable
import services.identity.staging.{ChangeFeed, IdentityStaging, StagedCluster, StagingEvent}
import tools.{ThreadPools, ScheduledTick, TickRecorder}

import java.time.Clock
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*
import scala.util.control.NonFatal

/**
 * Publishes what this country's identity takes still wait on, and what they decided, for the staging screen
 * (`/debug/identity/staging`, docs/design/identity-questions.md "The staging screen"): the stage's waits live in the
 * worker's memory ([[agreement.AgreementStage.waitingOn]]), not in Mongo, so nothing else can show them.
 *
 * What HAPPENED comes as events (`changes`, drained from a [[ChangeFeed]], [[StagingEvent]]): the agreement stage's
 * waits starting and ending, as they do, and each written projection's change list ([[TakeChange]]). Each [[publish]]
 * reads them with the takes the projection last wrote (`taken`, shared with the projection, nothing copied) and what each
 * waits on now: a cluster whose [[WaitingOn]] is not settled STAGES — since its wait began — with each open question and
 * what the question memory says of its last ask (`outcome`), rewritten as it changes and deleted once it stops. What
 * RESOLVED since the last publish is written once, each its own document kept for `window` (the database deletes it): a
 * cluster that staged and waits on nothing now — `settled` with the take it got, or `regrouped` when its listings no
 * longer form it — one `settled` after a wait that began and ended between two publishes, never seen staging, with when
 * and on what; and every change of the projections' change lists, each in exactly one document ([[StagingSnapshots]]: decided,
 * joined, gained, lost, changed). Nothing is capped: what the feed lets go, or a run of failed publishes, is counted in
 * the beat (`dropped`) and `takeMetrics`. A quiet country writes its beat and nothing else; every publish reports its
 * takes by basis to `takeMetrics`.
 *
 * Off the projection's thread, on its own schedule (`start`): the projection's quiet tick allocates nothing for it.
 */
final class IdentityStagingPublisher(country: String, staging: IdentityStaging, taken: () => Option[Resolution],
                                     changes: () => ChangeFeed.Drained[StagingEvent], snapshots: StagingSnapshots, clock: Clock,
                                     window: FiniteDuration = IdentityStagingPublisher.Window,
                                     ticks: TickRecorder = TickRecorder.Unrecorded,
                                     takeMetrics: IdentityStagingPublisher.TakeMetrics = IdentityStagingPublisher.TakeMetrics.Unrecorded,
                                     threadPools: ThreadPools)
    extends Stoppable with Logging {
  import IdentityStagingPublisher.*

  /** What the last publish left in the store, by key; read back from it once, on the first publish after a boot. */
  private var published: Option[Map[String, StagedCluster]] = None
  /** The events drained but not yet written: a publish that failed writes them with the next. */
  private var unwritten = Vector.empty[StagingEvent]
  /** The events let go since the last written beat: by the feed, full, or past [[UnwrittenCap]] while publishes fail. */
  private var dropped = 0L
  private lazy val scheduler = threadPools.scheduler(s"identity-staging-$country")

  def start(interval: FiniteDuration = Interval): Unit = {
    ticks.started(TickName, interval, interval)
    scheduler.scheduleWithFixedDelay(() => ScheduledTick.recorded(TickName, interval, logger, ticks)(publish()),
      interval.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  /** Stops the schedule, and publishes once more: what happened since the last publish is written, not lost with the
   *  worker's memory. Only a worker killed outright loses the events of its last publish interval. */
  override def stop(): Unit = {
    scheduler.shutdown()
    try publish() catch { case NonFatal(e) => logger.warn(s"identity staging ($country): the last publish before stopping failed", e) }
  }

  /** One snapshot: staging and recently resolved clusters worked out, the changed ones written. Nothing before the
   *  projection's first written take. */
  def publish(): Unit = synchronized {
    taken().foreach { resolution =>
      val now      = clock.millis()
      val before   = published.getOrElse(staging.ofCountry(country).map(c => c.key -> c).toMap)
      val events   = unwritten ++ drained()
      val snapshot = snapshots.of(resolution, before, events, now, dropped)
      takeMetrics.held(snapshot.clustersBy, snapshot.listingsBy)
      write(before, snapshot, events)
    }
  }

  /** `snapshot` written over `before`: its changed staging clusters upserted, the gone ones removed, what resolved and the
   *  beat. A failed write keeps the newest events for the next publish, which diffs against the store again. */
  private def write(before: Map[String, StagedCluster], snapshot: StagingSnapshots.Snapshot, events: Vector[StagingEvent]): Unit = {
    val upserts = snapshot.next.values.filterNot(c => before.get(c.key).contains(c)).toSeq
    val removed = before.keySet -- snapshot.next.keySet
    try {
      staging.publish(country, upserts, removed, snapshot.beat, snapshot.resolved, java.time.Duration.ofMillis(window.toMillis))
      published = Some(snapshot.next)
      unwritten = Vector.empty
      dropped = 0
    } catch {
      // the next publish diffs against the store again, what it holds unknown now, and writes these events with its own
      case NonFatal(e) =>
        published = None
        unwritten = events.takeRight(UnwrittenCap)
        letGo(events.size - unwritten.size)
        throw e
    }
  }

  /** What happened since the last drain; what the feed let go is counted ([[letGo]]). */
  private def drained(): Vector[StagingEvent] = {
    val feed = changes()
    letGo(feed.dropped)
    feed.changes
  }

  /** `n` events let go before a publish wrote them: counted in the next beat and the metric, and logged. */
  private def letGo(n: Long): Unit = if (n > 0) {
    dropped += n
    takeMetrics.dropped(n)
    logger.warn(s"identity staging ($country): $n staging events let go before a publish wrote them")
  }
}

object IdentityStagingPublisher {
  /** Where every publish reports the takes it read, by basis (`ResolverDecision.Basis`): the clusters and listings each
   *  basis holds now, and the events it was never told. What changed is the projection's to count
   *  (`IdentityProjectionMetrics.takesChanged`). */
  trait TakeMetrics {
    def held(clusters: Map[String, Int], listings: Map[String, Int]): Unit
    /** `events` the staging screen was not told: let go by a full feed, or past what failed publishes keep. */
    def dropped(events: Long): Unit
  }
  object TakeMetrics {
    val Unrecorded: TakeMetrics = new TakeMetrics {
      def held(clusters: Map[String, Int], listings: Map[String, Int]): Unit = ()
      def dropped(events: Long): Unit                                        = ()
    }
  }

  /** How often a country publishes. */
  val Interval: FiniteDuration = 20.seconds
  /** How long a resolved cluster stays on the screen: the database deletes it then (its `expireAt`). */
  val Window: FiniteDuration = 7.days
  /** The most changes a failed publish keeps for the next: the newest. */
  val UnwrittenCap = 20000
  /** How many changes the projection's feed holds for the publisher between two publishes; the oldest go past it. */
  val FeedCapacity = 20000
  private val TickName = "identity-staging"
}
