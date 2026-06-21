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

  /** Drop `key`'s stamp so the work behind it reads as STALE again and a
   *  re-enqueue is no longer deduped away by the freshness gate. Used when a row
   *  MERGE changed an enrichment's input fields (see
   *  [[services.movies.MergeRetrigger]]): the cached rating/id was computed for
   *  the pre-merge inputs, so it must be re-fetched even though the tmdbId-keyed
   *  stamp still looks fresh. Best-effort — never throws; a no-op for an unknown
   *  key. */
  def invalidate(key: String): Unit

  /** Completes once the mirror holds every persisted stamp of `kind` from a
   *  SUCCESSFUL load, so `isFresh(_, kind)` can't read a not-yet-loaded key as
   *  stale. A transient read failure/timeout is retried first — it does NOT signal
   *  ready against an empty mirror. A scheduler awaits this before its first tick,
   *  so a slow boot hydrate doesn't make every unit of work look stale and enqueue
   *  it all at once — the boot storm (see [[services.tasks.ScrapeReaper]]). Stores
   *  with nothing to load (in-memory, Mongo-less dev) are ready immediately. */
  def whenReady(kind: FreshnessKind): Future[Unit] = Future.unit

  /** Non-blocking form of [[whenReady]]: are `kind`'s stamps loaded into the
   *  mirror yet? A periodic reaper gates each scheduled tick on this so it never
   *  reads a not-yet-hydrated mirror as "everything stale" and re-enqueues the
   *  whole corpus on every boot. (ScrapeReaper instead *blocks* its first tick on
   *  `whenReady` — equivalent intent, but the non-scrape reapers tick-and-skip so
   *  the gate stays unit-testable without a held thread.) Derived, so impls only
   *  define the Future. */
  final def isReady(kind: FreshnessKind): Boolean = whenReady(kind).isCompleted

  def close(): Unit = ()
}

/** In-memory `FreshnessStore` for tests and Mongo-less local dev. */
class InMemoryFreshnessStore extends FreshnessStore {
  private val stamps = new ConcurrentHashMap[String, Instant]()
  override def lastFetchedAt(key: String): Option[Instant] = Option(stamps.get(key))
  override def markFresh(key: String, kind: FreshnessKind, at: Instant): Unit = { stamps.put(key, at); () }
  override def invalidate(key: String): Unit = { stamps.remove(key); () }
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
  // The rest-phase signal (everything that ISN'T a scrape stamp — detail + rating
  // stamps), completed once `loadRest` has run. The DetailReaper / EnrichmentReaper
  // gate on this: those stamps hydrate AFTER the scrape phase, so without the gate
  // their first post-deploy tick reads an empty mirror as "every detail/rating
  // stale" and re-enqueues the whole corpus (each cascading downstream) on EVERY
  // boot — the recurring per-deploy `EnrichDetails`/rating spike.
  private val restReady = Promise[Unit]()

  coll match {
    case Some(c) =>
      val thread = new Thread(() => hydrate(c), "freshness-init")
      thread.setDaemon(true)
      thread.start()
    case None =>
      scrapeReady.trySuccess(())
      restReady.trySuccess(())
  }

  override def whenReady(kind: FreshnessKind): Future[Unit] = kind match {
    case FreshnessKind.CinemaScrape => scrapeReady.future
    case _                          => restReady.future
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
          (exception: Throwable) => logger.debug(s"Freshness write failed for $key: ${exception.getMessage}")
        )
      }.recover { case exception => logger.debug(s"Freshness write failed for $key: ${exception.getMessage}") }
    }
  }

  override def invalidate(key: String): Unit = {
    mirror.remove(key)
    coll.foreach { c =>
      // Fire-and-forget like markFresh: the mirror is already cleared, so even if
      // the Mongo delete is lost the next restart just re-hydrates a stale stamp
      // that the merge will invalidate again. Never break the caller's loop.
      Try {
        c.deleteOne(Filters.eq("_id", key)).subscribe(
          (_: org.mongodb.scala.result.DeleteResult) => (),
          (exception: Throwable) => logger.debug(s"Freshness invalidate failed for $key: ${exception.getMessage}")
        )
      }.recover { case exception => logger.debug(s"Freshness invalidate failed for $key: ${exception.getMessage}") }
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
      loadRest    = () => { hydrateInto(c, Filters.ne("kind", scrapeLabel), 60.seconds, "enrichment"); () },
      restReady   = restReady
    )
  }

  /** Load the stamps matching `filter` into the mirror. Returns true only if the
   *  read COMPLETED (cursor exhausted) — matching nothing still counts, that's a
   *  genuine empty result. Returns false if it timed out or failed, which the
   *  caller retries rather than treating the empty mirror as authoritative. */
  private def hydrateInto(c: MongoCollection[Document], filter: Bson, timeout: FiniteDuration, label: String): Boolean =
    Try {
      val documents = Await.result(c.find(filter).toFuture(), timeout)
      var count = 0
      documents.foreach { document =>
        for {
          key  <- Option(document.getString("_id"))
          date <- Option(document.getDate("lastFetchedAt"))
        } { mirror.put(key, Instant.ofEpochMilli(date.getTime)); count += 1 }
      }
      if (count > 0) logger.info(s"Hydrated $count $label freshness stamp(s) from Mongo.")
    } match {
      case scala.util.Success(_) => true
      case scala.util.Failure(exception) =>
        logger.warn(s"Freshness $label hydrate failed: ${exception.getMessage}"); false
    }
}

object MongoFreshnessStore {
  /** Boot hydrate in two phases. Load the scrape stamps (`loadScrape`, returning
   *  true once the read SUCCEEDS), then signal `scrapeReady` so the
   *  [[services.tasks.ScrapeReaper]] can start, then load the rest of the corpus
   *  (`loadRest`) off the critical path.
   *
   *  A failed/timed-out scrape read is RETRIED up to `maxScrapeAttempts` (sleeping
   *  `retryDelay` between) before readiness is signalled. This is the boot-storm
   *  fix: previously a single attempt signalled ready even when it timed out,
   *  handing the reaper an EMPTY mirror — it then saw every cinema as stale and
   *  re-scraped all ~300, which throttled the shared CPU and Mongo so the NEXT
   *  restart's hydrate was slower still, storming again (the stacked-restart
   *  amplification). Retrying past a transient Mongo slowdown keeps readiness
   *  withheld so the reaper holds its ticks instead of storming.
   *
   *  Readiness still completes once the budget is spent even if every attempt
   *  failed — a bounded last-resort fail-open, so a genuinely-down Mongo can't
   *  wedge scraping forever (the reaper's per-tick cap bounds any residual burst) —
   *  and `loadRest` always runs. `sleep` is injectable so tests don't block. Pure
   *  orchestration so the retry/ordering is testable without a live Mongo. */
  def hydrateInPhases(
    loadScrape:        () => Boolean,
    scrapeReady:       Promise[Unit],
    loadRest:          () => Unit,
    // Completed once `loadRest` has run, gating the detail/rating reapers the way
    // `scrapeReady` gates the ScrapeReaper. Defaulted so the existing scrape-phase
    // tests that don't care about it stay terse; production always wires it.
    restReady:         Promise[Unit] = Promise[Unit](),
    maxScrapeAttempts: Int = 5,
    retryDelay:        FiniteDuration = 5.seconds,
    sleep:             FiniteDuration => Unit = d => Thread.sleep(d.toMillis)
  ): Unit =
    try {
      var attempt = 1
      var loaded  = Try(loadScrape()).getOrElse(false)
      while (!loaded && attempt < maxScrapeAttempts) {
        sleep(retryDelay)
        attempt += 1
        loaded = Try(loadScrape()).getOrElse(false)
      }
    } finally { scrapeReady.trySuccess(()); try loadRest() finally restReady.trySuccess(()) }
}
