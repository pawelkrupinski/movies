package services

import org.mongodb.scala.{Document, MongoCollection}
import services.UptimeMonitor.{PollIntervalMs, ScheduledJob, TagReloadIntervalMs}

/** Which side of the shared `uptimeBuckets` collection a process is on — picked at the composition
 *  root, one per tier, never derived from anything else.
 *
 *  Both tiers write `uptimeBuckets` and both authenticate as the app's Mongo user, so the serving
 *  app is perfectly able to drop an index the worker depends on. It has nothing to gain by doing
 *  so: both compute the expiry from the same constant, so a disagreement the reader can see is one
 *  the owner sees too, rebuilds, and reports if it cannot. Hence one owner ([[UptimeTier.Owner]],
 *  `reconcile`) and one reader that only creates the index when absent ([[UptimeTier.Surfacing]],
 *  `ensure`) — `UptimeMonitor.ensureIndexes` makes the call each tier is owed. */
sealed trait UptimeTier {

  /** The READ jobs this tier runs beside the flusher every tier runs. */
  private[services] def readerJobs(
    buckets: Option[MongoCollection[Document]],
    tags: Option[MongoCollection[Document]],
    sync: UptimeSync,
    serviceTags: ServiceTags
  ): Seq[ScheduledJob]

  /** What the monitor logs once its background work is scheduled, if anything. */
  private[services] def startupNote: Option[String]
}

object UptimeTier {

  /** The worker: it records every scraper + enrichment outcome, OWNS the collection (rebuilds a TTL
   *  index that disagrees) and reads nothing back — nothing reads its in-memory map. */
  case object Owner extends UptimeTier {
    private[services] def readerJobs(buckets: Option[MongoCollection[Document]], tags: Option[MongoCollection[Document]],
                                     sync: UptimeSync, serviceTags: ServiceTags): Seq[ScheduledJob] = Seq.empty
    private[services] def startupNote: Option[String] = None
  }

  /** The serving app: after hydrating from Mongo it POLLS `uptimeBuckets` every `PollIntervalMs` and
   *  merges the current snapshot — including buckets written by OTHER processes — into its in-memory
   *  map, firing listeners for the ones that changed so the live /uptime SSE reflects them, and
   *  re-reads the service tags every `tagReloadIntervalMs`. Each on its OWN period, because the two
   *  reads cost wildly different amounts (see `TagReloadIntervalMs`). It does not own the TTL index. */
  final case class Surfacing(tagReloadIntervalMs: Long = TagReloadIntervalMs) extends UptimeTier {
    private[services] def readerJobs(buckets: Option[MongoCollection[Document]], tags: Option[MongoCollection[Document]],
                                     sync: UptimeSync, serviceTags: ServiceTags): Seq[ScheduledJob] =
      Seq(
        buckets.map(c => ScheduledJob("poll-buckets", PollIntervalMs, () => sync.poll(c))),
        // Re-read the tag collection so a cinema tagged by the worker after this
        // app booted still surfaces — but on its OWN, far slower period. The read
        // is unfiltered and the collection is no longer small (see
        // `TagReloadIntervalMs`), so it must not ride the 10s bucket poll.
        tags.map(tc => ScheduledJob("reload-tags", tagReloadIntervalMs, () => serviceTags.load(tc))),
      ).flatten
    private[services] def startupNote: Option[String] =
      Some(s"UptimeMonitor: polling uptimeBuckets every ${PollIntervalMs / 1000}s, reloading uptimeServiceTags every ${tagReloadIntervalMs / 1000}s for cross-process updates.")
  }
}
