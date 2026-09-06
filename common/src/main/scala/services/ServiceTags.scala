package services

import com.mongodb.client.model.{IndexOptions => JIndexOptions, UpdateOptions}
import org.mongodb.scala.{Document, MongoCollection, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Indexes, Sorts, Updates}
import play.api.Logging
import services.movies.KeysetScan

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Per-SERVICE (per-row, not per-bucket) labels — a generic tag mechanism the
 *  /uptime page renders as chips next to a row. Static metadata, decoupled from
 *  [[UptimeMonitor]]'s time-series buckets: the worker (which alone knows the
 *  scraper catalog) tags each cinema with its client kind via `tagService`; the
 *  serving app reads them through the same Mongo channel it polls the buckets on.
 *
 *  Tags live in their own collection, `uptimeServiceTags` (no TTL — they're static
 *  config, unlike the 24h-expiring buckets), one document per service keyed by
 *  `service`. `collection` is that handle for the upsert `tagService` makes;
 *  `None` keeps everything in memory (tests, and a process without Mongo). */
final class ServiceTags(collection: Option[MongoCollection[Document]]) extends Logging {

  private val tags = new ConcurrentHashMap[String, Set[String]]()

  /** `uptimeServiceTags` is keyed by `service` and queried by NOTHING ELSE — the upsert in
   *  `tagService` and the whole-collection `load` are its only readers. It had no index
   *  on it at all, because the monitor's `ensureIndexes` is only ever handed `uptimeBuckets`.
   *
   *  The cost of that omission was the single largest source of work on the database. Every
   *  one of the ~8,300 upserts per scrape cycle, per country, was a COLLSCAN of the ~8,300
   *  documents already there: 244,569,244 documents examined and 85 minutes of query time in
   *  one two-day window, ~93% of ALL document scanning on the server. Measured again after
   *  the fact — `explain` on `find({service})` reported COLLSCAN, `docsExamined: 8315`,
   *  `keysExamined: 0`, in all five databases.
   *
   *  UNIQUE, because `service` IS the key: there are no duplicates today, and the constraint
   *  also closes the upsert-under-concurrency race that could create one. Isolated in its own
   *  `Try` like the bucket indexes — an existing non-unique index would make this throw, and a
   *  collection that cannot be indexed must not stop the monitor from running. */
  def ensureIndex(c: MongoCollection[Document]): Unit =
    Try {
      Await.result(c.createIndex(Indexes.ascending("service"), new JIndexOptions().unique(true)).toFuture(), 10.seconds)
    }.recover { case exception => logger.warn(s"Uptime tag index creation failed: ${exception.getMessage}") }

  /** Attach `tags` to `service`, replacing any existing set. Updates the in-memory map and,
   *  WHEN THE VALUE ACTUALLY CHANGED, best-effort upserts the one tag document for the
   *  service so other processes (the serving app) pick it up. Caller-supplied empty `tags`
   *  clears the row's tags. Returns whether a write was made — the only reason it returns
   *  anything is that the skip is worth being able to assert on.
   *
   *  It used to write unconditionally, and the comment here used to say so ("a no-op set is
   *  still written (idempotent `$set`)"), which was true and expensive. Tags are static
   *  config re-asserted once per cinema per scrape cycle: of 35,883 slow tag updates in one
   *  two-day window, 35,882 reported `nModified: 0`. Mongo does not collapse those for us —
   *  each still had to FIND the row it then did not change, and until `ensureIndex` that
   *  was a full scan of the collection. */
  def tagService(service: String, serviceTags: Set[String]): Boolean = {
    // `put` RETURNS the value it replaced, so the guard costs neither a read nor extra state.
    // A first call in a fresh process has no previous value and writes once, which is what
    // reconciles a tag that changed while this process was not running.
    val previous = Option(tags.put(service, serviceTags))
    val changed  = !previous.contains(serviceTags)
    if (changed) collection.foreach { c =>
      Try {
        c.updateOne(
          Filters.eq("service", service),
          Updates.combine(Updates.set("service", service), Updates.set("tags", serviceTags.toList.asJava)),
          new UpdateOptions().upsert(true)
        ).subscribe(
          (_: org.mongodb.scala.result.UpdateResult) => (),
          (exception: Throwable) => writeFailed(service, serviceTags, previous, exception)
        )
      }.recover { case exception => writeFailed(service, serviceTags, previous, exception) }.getOrElse(())
    }
    changed
  }

  /** Undo the optimistic in-memory `put` when its Mongo write did not land.
   *
   *  The skip-if-unchanged guard reads the in-memory map, so leaving the new value there
   *  after a failed write would make every later call report "unchanged" and never retry —
   *  Mongo would stay on the old tags until the process restarted. The unconditional write
   *  this guard replaced healed that on the next scrape cycle; rolling back keeps it doing so.
   *
   *  Both rollbacks are CONDITIONAL on the value still being the one we wrote, because the
   *  failure arrives asynchronously and a newer `tagService` may already have overwritten it —
   *  that newer value is the one Mongo will be asked for next, so it must not be clobbered. */
  private def writeFailed(service: String, serviceTags: Set[String], previous: Option[Set[String]], exception: Throwable): Unit = {
    previous match {
      case Some(value) => tags.replace(service, serviceTags, value)
      case None        => tags.remove(service, serviceTags)
    }
    logger.debug(s"Uptime tag write failed: ${exception.getMessage}")
  }

  /** Current per-service tags, for the page render. Returns the in-memory view,
   *  populated from Mongo at boot + on each reload (serving app) or directly by
   *  `tagService` (worker). */
  def snapshot(): Map[String, Set[String]] = tags.asScala.toMap

  /** Load all service tags from Mongo into the in-memory map. This is an
   *  UNFILTERED read of the entire collection — NOT cheap: one document per tagged
   *  service used to mean a handful, but it is 2,687 documents for Poland alone
   *  (measured 2026-07-18) and grows with every cinema in every country. That is
   *  why the reader schedules it on `UptimeMonitor.TagReloadIntervalMs` (5 min)
   *  rather than the 10s bucket poll it once shared. `$set` semantics make
   *  re-loading idempotent, so a slower cadence only delays a newly tagged cinema
   *  appearing, never corrupts the map. */
  def load(c: MongoCollection[Document]): Unit = Try {
    // Keyset-paged rather than one cursor. The comment above already counts 2,687
    // documents for Poland alone and says it grows with every cinema in every
    // country — which is precisely the size at which an unbounded `find()` stops
    // being merely slow and recurses the async driver into `StackOverflowError` on
    // an I/O thread (see services.movies.KeysetScan). This runs every 5 minutes,
    // forever, in BOTH the web and the worker, so it is the highest-frequency
    // unbounded read we had.
    //
    // `$set`-style application per document means a partial load is harmless: tags
    // that did not arrive are simply not refreshed this cycle, and the next one
    // picks them up. That is why an incomplete scan stays at debug here rather than
    // warning like the staging read, where a short result silences an alarm.
    KeysetScan.scan[Document](
      label          = "UptimeMonitor tag load",
      batchSize      = 1000,
      maxAttempts    = 2,
      initialBackoff = 500.millis,
      keyOf          = _.getString("_id"),
      fetchPage      = (afterId, limit) => {
        val find = afterId.fold(c.find())(a => c.find(Filters.gt("_id", a)))
        Await.result(find.sort(Sorts.ascending("_id")).limit(limit).toFuture(), UptimeMonitor.HydrateTimeout)
      },
      onIncomplete   = exception => logger.debug(s"Uptime tag load incomplete: ${exception.getMessage}")
    ) { documents =>
      documents.foreach { document =>
        Option(document.getString("service")).foreach { service =>
          val loaded = Try(document.getList("tags", classOf[String])).toOption.flatMap(Option(_))
            .map(_.asScala.toSet).getOrElse(Set.empty[String])
          if (loaded.nonEmpty) tags.put(service, loaded) else tags.remove(service)
        }
      }
    }
    ()
  }.recover { case exception => logger.debug(s"Uptime tag load failed: ${exception.getMessage}") }
}
