package services.freshness

import com.mongodb.client.model.UpdateOptions
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.{Filters, Updates}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import scala.util.Try

/**
 * Records when each unit of network-touching work (a cinema scrape, a movie's
 * detail fetch, a rating refresh) last completed, keyed by a caller-chosen
 * string. A scheduler reads [[isFresh]] before enqueuing or doing work and skips
 * it when it was done inside the kind's TTL.
 *
 * The TTL/skip logic lives here as a default method so both the real store and
 * the test fake share it by construction — they differ only in *where* the
 * timestamps are kept. Keys are the canonical dedup strings the producers build,
 * e.g. `scrape|<cinema>`, `detail|<chain>|<documentId>`, `imdb|<documentId>`.
 */
trait FreshnessStore {
  /** When `key` was last marked fresh, or None if never. */
  def lastFetchedAt(key: String): Option[Instant]

  /** Record that the work behind `key` (of the given `kind`) just completed.
   *  Call only on SUCCESS, so a failed fetch stays stale and retries. */
  def markFresh(key: String, kind: FreshnessKind, at: Instant = Instant.now()): Unit

  /** True when `key` was marked fresh within `kind`'s TTL — so the work may be
   *  skipped. A permanent kind (None TTL) is fresh whenever any timestamp
   *  exists; an unknown key is always stale. */
  final def isFresh(key: String, kind: FreshnessKind, now: Instant = Instant.now()): Boolean =
    lastFetchedAt(key) match {
      case None => false
      case Some(t) =>
        Freshness.ttlFor(kind) match {
          case None      => true
          case Some(ttl) => t.isAfter(now.minusMillis(ttl.toMillis))
        }
    }

  /** Completes once the mirror holds every persisted stamp of `kind`, so
   *  `isFresh(_, kind)` can't read a not-yet-loaded key as stale. A scheduler
   *  awaits this before its first tick, so a slow boot hydrate doesn't make
   *  every unit of work look stale and enqueue it all at once — the boot storm
   *  (see [[services.tasks.ScrapeReaper]]). Stores with nothing to load
   *  (in-memory, Mongo-less dev) are ready immediately. */
  def whenReady(kind: FreshnessKind): Future[Unit] = Future.unit

  def close(): Unit = ()
}

/** In-memory `FreshnessStore` for tests and Mongo-less local dev. */
class InMemoryFreshnessStore extends FreshnessStore {
  private val stamps = new ConcurrentHashMap[String, Instant]()
  override def lastFetchedAt(key: String): Option[Instant] = Option(stamps.get(key))
  override def markFresh(key: String, kind: FreshnessKind, at: Instant): Unit = { stamps.put(key, at); () }
}

/**
 * Mongo-backed `FreshnessStore`, collection `freshness` with documents
 * `{ _id: <key>, kind: <label>, lastFetchedAt: ISODate }`.
 *
 * Reads are served from an in-memory mirror, not Mongo: the enrichment reaper
 * checks freshness for every cached row × every rating kind on each tick, which
 * would otherwise be hundreds of `findOne`s per tick. The mirror is hydrated
 * once at boot (in a daemon thread, the `UptimeMonitor` idiom) and kept current
 * by `markFresh` writing through to both the map and Mongo. Persisting to Mongo
 * means a worker restart doesn't forget what was fresh and re-fetch everything.
 *
 * Single-writer by design (one worker process). A `null` db disables
 * persistence — the mirror still works, it just doesn't survive a restart.
 */
class MongoFreshnessStore(db: Option[MongoDatabase] = None) extends FreshnessStore with Logging {
  private val mirror = new ConcurrentHashMap[String, Instant]()
  // Relaxed `{w:1, j:false}` like the task queue: freshness stamps are a cache
  // (a lost write just means a redundant re-fetch later), so the journal/majority
  // wait per stamp is wasted cost on the shared-cpu Mongo. Reuse the queue's concern.
  private val coll: Option[MongoCollection[Document]] =
    db.map(_.getCollection("freshness").withWriteConcern(services.tasks.MongoTaskQueue.QueueWriteConcern))

  // Signals that the scrape stamps are in the mirror. The ScrapeReaper awaits this
  // before its first tick, so a cinema is never read as stale merely because the
  // boot hydrate hasn't reached its stamp yet. Completed by the scrape phase of
  // `hydrate`, or immediately when there's nothing to load (Mongo-less dev).
  private val scrapeReady = Promise[Unit]()

  coll match {
    case Some(c) =>
      val t = new Thread(() => hydrate(c), "freshness-init")
      t.setDaemon(true)
      t.start()
    case None =>
      scrapeReady.trySuccess(())
  }

  override def whenReady(kind: FreshnessKind): Future[Unit] = kind match {
    case FreshnessKind.CinemaScrape => scrapeReady.future
    case _                          => Future.unit
  }

  override def lastFetchedAt(key: String): Option[Instant] = Option(mirror.get(key))

  override def markFresh(key: String, kind: FreshnessKind, at: Instant): Unit = {
    mirror.put(key, at)
    coll.foreach { c =>
      // Best-effort, fire-and-forget: a write failure must never break the
      // caller's work loop. The mirror already has the value; the next restart
      // just won't see this stamp. Synchronous build at .subscribe can throw on
      // a closed client (shutdown race), so wrap in Try like UptimeMonitor.
      Try {
        c.updateOne(
          Filters.eq("_id", key),
          Updates.combine(
            Updates.set("kind", kind.label),
            Updates.set("lastFetchedAt", new java.util.Date(at.toEpochMilli))
          ),
          new UpdateOptions().upsert(true)
        ).subscribe(
          (_: org.mongodb.scala.result.UpdateResult) => (),
          (ex: Throwable) => logger.debug(s"Freshness write failed for $key: ${ex.getMessage}")
        )
      }.recover { case ex => logger.debug(s"Freshness write failed for $key: ${ex.getMessage}") }
    }
  }

  // Hydrate in two phases. The whole `freshness` collection is the ~300 scrape
  // stamps plus thousands of detail/rating stamps (one per film per source), and
  // a single-cursor read of all of it brushes a multi-second deadline as the
  // corpus grows. The ScrapeReaper only needs the scrape stamps, so load THOSE
  // first as a small filtered query and signal `scrapeReady` the moment they
  // land; the rest hydrates afterwards, off the boot-storm critical path.
  private def hydrate(c: MongoCollection[Document]): Unit = {
    val scrapeLabel = FreshnessKind.CinemaScrape.label
    MongoFreshnessStore.hydrateInPhases(
      loadScrape  = () => hydrateInto(c, Filters.eq("kind", scrapeLabel), 15.seconds, "scrape"),
      scrapeReady = scrapeReady,
      loadRest    = () => hydrateInto(c, Filters.ne("kind", scrapeLabel), 60.seconds, "enrichment")
    )
  }

  private def hydrateInto(c: MongoCollection[Document], filter: Bson, timeout: FiniteDuration, label: String): Unit = Try {
    val documents = Await.result(c.find(filter).toFuture(), timeout)
    var count = 0
    documents.foreach { document =>
      for {
        key  <- Option(document.getString("_id"))
        date <- Option(document.getDate("lastFetchedAt"))
      } { mirror.put(key, Instant.ofEpochMilli(date.getTime)); count += 1 }
    }
    if (count > 0) logger.info(s"Hydrated $count $label freshness stamp(s) from Mongo.")
  }.recover { case ex => logger.warn(s"Freshness $label hydrate failed: ${ex.getMessage}") }
}

object MongoFreshnessStore {
  /** Boot hydrate in two phases: load the scrape stamps (`loadScrape`), then
   *  signal `scrapeReady` so the [[services.tasks.ScrapeReaper]] can start, then
   *  load the rest of the corpus (`loadRest`) off the critical path. The signal
   *  and the rest phase run even if the scrape phase throws, so a hydrate failure
   *  degrades to the old re-scrape-all behaviour rather than wedging the reaper
   *  forever. Pulled out as a pure orchestration so the ordering is testable
   *  without a live Mongo. */
  def hydrateInPhases(loadScrape: () => Unit, scrapeReady: Promise[Unit], loadRest: () => Unit): Unit =
    try loadScrape()
    finally { scrapeReady.trySuccess(()); loadRest() }
}
