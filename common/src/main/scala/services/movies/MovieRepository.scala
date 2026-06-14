package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.{ReplaceOptions, UpdateOptions}
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import models.MovieRecord
import org.mongodb.scala.bson.{BsonDateTime, BsonNull}
import org.mongodb.scala.model.{Filters, IndexOptions, Indexes, Sorts, Updates}
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription}
import org.bson.conversions.Bson
import play.api.Logging
import tools.Env

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** One persisted (title, year) → MovieRecord row. Used as the return type
 *  of `MovieRepository.findAll` and `MovieCache.snapshot` so callers iterate
 *  named fields instead of destructuring an anonymous 3-tuple. */
case class StoredMovieRecord(title: String, year: Option[Int], record: MovieRecord)

object StoredMovieRecord {
  /** The Mongo `_id` for a `(title, year)` row: `sanitize(title)|year`. The one
   *  formula the repository keys rows by — exposed so the change stream and the
   *  /debug live view can key DOM rows on the same id the store does. Matches
   *  the in-memory `CacheKey` normalization (case/diacritic-folded). */
  def idFor(title: String, year: Option[Int]): String =
    s"${TitleNormalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"

  /** The `_id` of a stored row. */
  def idOf(row: StoredMovieRecord): String = idFor(row.title, row.year)

  /** Rebuild a stored row from its persisted `_id` and `MovieRecord`, deriving
   *  the display `title` and `year` rather than reading pinned columns — used by
   *  the Mongo codec (`MovieCodecs.toDomain`), whose BSON drops the `title`/
   *  `year` fields. The `_id` is `sanitize(title)|year`: `sanitize` never emits
   *  `|`, so the suffix is the year and the prefix is the cache key's sanitized
   *  form. Every spelling in a row sanitizes to that prefix (the `CacheKey`
   *  identity), so `displayTitle(prefix)` sanitizes back to it — the rebuilt key
   *  recomputes to the same `_id`, no re-keying churn. (The in-memory repository keeps
   *  the full record in memory and returns its title verbatim, so it needs no
   *  recovery step; for realistic rows the two agree.) */
  def fromStorage(id: String, record: MovieRecord): StoredMovieRecord = {
    val sep      = id.lastIndexOf('|')
    val idPrefix = if (sep >= 0) id.substring(0, sep) else id
    val year     = if (sep >= 0) id.substring(sep + 1).toIntOption else None
    StoredMovieRecord(record.displayTitle(idPrefix), year, record)
  }
}

/**
 * Persistent store for `(title, year) → MovieRecord` records.
 *
 * The trait is what consumers (`MovieCache`, scripts, integration tests) see
 * — `MongoMovieRepository` (production) and `InMemoryMovieRepository` (tests) are the two
 * implementations. Per CLAUDE.md's DIP guidance: every collaborator is wired
 * via the trait; the concrete type only appears at the composition root
 * (`AppLoader`) and in test setup.
 */
trait MovieRepository {
  /** Whether the persistence layer is wired up. When false, callers can still
   *  use the in-memory cache but writes are no-ops. */
  def enabled: Boolean

  /** Snapshot of every persisted record. Returns empty when disabled. */
  def findAll(): Seq[StoredMovieRecord]

  /** Remove every record matching the given (title, year). Best-effort —
   *  failures are logged, never thrown. */
  def delete(title: String, year: Option[Int]): Unit

  /** Write-through upsert. Best-effort — failures are logged, never thrown. */
  def upsert(title: String, year: Option[Int], e: MovieRecord): Unit

  /** Update the row at `(title, year)` only if it currently exists. Returns
   *  true on update, false when no row matched (concurrent delete, or the
   *  row never existed). Used by the cache's `putIfPresent` so a rating
   *  write that races against a concurrent `cache.invalidate` can't
   *  resurrect the row by upserting it back into existence.
   *
   *  Writes only the fields where `before` and `after` differ — via
   *  `$set`/`$unset` per `MovieRecordPatch`. An out-of-band Mongo edit
   *  to a field this updater didn't touch (e.g. `FilmwebUrlAudit`
   *  clearing `filmwebUrl` while a stale-cache rating tick concurrently
   *  bumps `filmwebRating`) is therefore preserved instead of being
   *  clobbered by a full-doc replace. */
  def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord): Boolean

  /** Stream out-of-band changes to persisted rows as they happen, so the cache
   *  can apply each change incrementally instead of periodically reloading the
   *  whole collection. `onUpsert` fires once per inserted / updated / replaced
   *  row, already decoded. Best-effort: out-of-band *deletes* and any gap while
   *  the stream reconnects are left to the periodic backstop rehydrate, so a
   *  store that can't stream (disabled, or a standalone Mongo with no change
   *  streams) may return `None` and the caller simply relies on that backstop.
   *  The returned handle stops watching. Default: not supported. */
  def watchUpserts(onUpsert: StoredMovieRecord => Unit): Option[AutoCloseable] =
    watchChanges(onUpsert, _ => ())

  /** Like [[watchUpserts]] but also surfaces out-of-band DELETEs (by `_id`), so
   *  a consumer that must reflect row *removal* sees it — the /debug live view,
   *  where a merge deletes the losing row and the row must disappear. `onDelete`
   *  gets the raw `_id` (`sanitize(title)|year`, the [[StoredMovieRecord.idFor]]
   *  form). Default: not supported (returns None), same as [[watchUpserts]]. */
  def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: String => Unit
  ): Option[AutoCloseable] = None

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit
}

/**
 * MongoDB-backed `MovieRepository`. Persists records to the `movies` collection.
 *
 * When `MONGODB_URI` is unset the repository silently no-ops — local dev / tests
 * without Mongo connectivity keep working off the in-memory cache only.
 *
 * The driver uses Reactive Streams, but the enrichment pipeline is a single
 * daemon worker so we use the blocking `.toFuture()` form throughout.
 *
 * Round-tripping happens through mongo-scala-driver's case-class codec
 * macros — see `MovieCodecs.registry` for the wiring and `StoredMovieDto`
 * for the storage-shape DTO. The collection is typed `[StoredMovieDto]`,
 * so reads and writes carry the case class directly; the small
 * `fromDomain`/`toDomain` helpers bridge the `Map[Source, SourceData]`
 * domain shape and the `Map[String, SourceData]` storage shape.
 *
 * Lifecycle: caller (`AppLoader`) registers a shutdown hook that calls
 * `close()` — the class doesn't self-register.
 */
class MongoMovieRepository(
  sharedDb: Option[MongoDatabase] = None,
  // Scripts pass `sharedDb = None` and expect us to connect from
  // `MONGODB_URI` ourselves (default true). Wiring sets it to false:
  // when `MongoConnection` is already attempted, an explicit `None`
  // means it failed and re-running our own init would just hit the
  // same DNS / TLS timeout twice. Saves ~15s of boot time on the
  // offline / unreachable-cluster path.
  fallbackToOwnInit: Boolean = true
) extends MovieRepository with Logging {

  // Lazy so subclasses that override every wire method (e.g.
  // `InMemoryMovieRepository` in tests) never trigger a Mongo connection
  // attempt — `new InMemoryMovieRepository()` was waiting 10 seconds per test
  // for the parent's init() to time out against an unreachable cluster.
  //
  // `sharedDb` injection (the production path): Wiring's `MongoConnection`
  // owns a single MongoClient and passes its `.database` here. We apply
  // our own codec registry to that database (a view, not a clone — the
  // underlying client is shared) and grab our collection from it. This
  // class doesn't own the client and its `close()` is a no-op.
  //
  // `sharedDb = None` (legacy path used by ad-hoc scripts under
  // test/scala/scripts/): we build our own MongoClient from `MONGODB_URI`
  // and own its close().
  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[StoredMovieDto]]) =
    sharedDb match {
      case Some(db) =>
        val withRegistry = db.withCodecRegistry(MovieCodecs.registry)
        // Relaxed write concern (w:1, j:false): `movies` is re-scraped continuously,
        // so a write lost to a crash is recovered by the next scrape pass. Skipping
        // the journal sync cuts per-write cost on the shared-CPU Mongo — the worker's
        // write rate is what throttles it. Same trade `MongoTaskQueue` already makes.
        val coll = withRegistry.getCollection[StoredMovieDto]("movies")
          .withWriteConcern(WriteConcern.W1.withJournal(false))
        ensureIndexes(coll)
        (None, Some(coll))
      case None if fallbackToOwnInit => init()
      case None                      => (None, None)
    }
  private def clientOpt: Option[MongoClient]                     = initResult._1
  private def coll:      Option[MongoCollection[StoredMovieDto]] = initResult._2

  def enabled: Boolean = coll.isDefined

  /** Test seam: the write concern configured on the `movies` collection. */
  def collectionWriteConcern: Option[WriteConcern] = coll.map(_.writeConcern)

  /** Boot-time + periodic full reload: a single `find()` over the whole
   *  collection. The earlier discover-then-fan-out (project the `_id`s, split
   *  them into contiguous half-open `_id` ranges, fire four parallel ranged
   *  finds, join) existed to dodge Atlas serialising single cursors at
   *  ~50 ms/doc. We have since moved to a self-hosted Mongo, where that
   *  pathology is gone: on the prod app→Mongo LAN one cursor ties the fan-out
   *  — measured (N=20, warm) p50 ~108 ms for ~600 docs vs ~128 ms, inside the
   *  run-to-run noise. Dropping it also removes the range-gap correctness
   *  hazard — a single cursor reads a consistent set by construction.
   *
   *  Deliberate trade-off (measured, not assumed): the fan-out's one remaining
   *  benefit is parallelising read latency across 4 connections over a HIGH-
   *  latency link. Over a local→prod `flyctl` tunnel the fan-out stayed ~2.7 s
   *  while a single cursor ran ~6 s p50 (up to ~8.5 s, high variance); a large
   *  `batchSize` was measured NOT to help (the limiter is single-connection
   *  serial throughput, not `getMore` count). We accept that *local-dev-only*
   *  cost for the far simpler single-cursor shape, since prod — the path that
   *  matters — is a tie. Note: on a bad tunnel night the slow hydrate can
   *  approach the timeout below; local dev that needs fast/reliable boots
   *  should point at a local Mongo rather than tunnelling to prod.
   *
   *  The 60s timeout (not the 10s used by point writes) covers a cold
   *  WiredTiger first read after a process boot, which can take 10–20 s even
   *  when steady-state finds are <100 ms; a short timeout there silently
   *  strands the cache empty (the surrounding `Try.getOrElse(Seq.empty)`
   *  swallows the `TimeoutException` with no log line — what a
   *  stale-page-on-startup bug looks like from the outside). */
  def findAll(): Seq[StoredMovieRecord] = coll match {
    case Some(c) =>
      Try {
        // Sort by the immutable, unique `_id` index. An UNSORTED scan over a
        // collection being written concurrently (the worker resolves TMDB,
        // clears `detailPending`, re-keys years on `movies` continuously) can
        // return the same document more than once — and skip others — when an
        // intervening write relocates it mid-scan. That surfaced on `/debug` as
        // phantom duplicate rows (the same `_id` rendered twice, one a stale
        // pre-write image) that never cleared, plus silently-dropped rows. An
        // `_id`-ordered IXSCAN returns each doc exactly once (the key is unique
        // and never changes), so it can neither duplicate nor skip.
        Await.result(c.find().sort(Sorts.ascending("_id")).toFuture(), 60.seconds).map(StoredMovieDto.toDomain)
      }.recover {
        // Catch every throwable, not just MongoException — TimeoutException
        // (Java `j.u.c.TimeoutException` from `Await.result`) and BSON decode
        // errors must be logged, not swallowed into `.getOrElse(Seq.empty)`,
        // which would strand the cache empty on the boot path with no log line.
        case ex: Throwable =>
          logger.warn(s"MovieRepository.findAll failed: ${ex.getClass.getSimpleName}: ${ex.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
    case None => Seq.empty
  }

  /** Deletes by `_id` (the current `docId` formula) OR by the legacy `title` +
   *  `year` fields. Current docs no longer persist `title`/`year` (the `_id`
   *  encodes both — see `StoredMovieDto`), so they're caught by the `_id`
   *  branch. The legacy field branch still catches OLD-format docs whose `_id`
   *  used a prior `docId` formula but which carry the `title`/`year` columns —
   *  `_id`-only would silently miss those orphans and they'd survive every
   *  startup's merge. */
  def delete(title: String, year: Option[Int]): Unit = coll.foreach { c =>
    val yearFilter = year match {
      case Some(y) => Filters.eq("year", y)
      // year=None in the in-memory model lands as either BsonNull() or a
      // missing field in legacy docs; cover both.
      case None    => Filters.or(Filters.eq("year", BsonNull()), Filters.exists("year", false))
    }
    val filter = Filters.or(
      Filters.eq("_id", docId(title, year)),
      Filters.and(Filters.eq("title", title), yearFilter)
    )
    Try {
      val result = Await.result(c.deleteMany(filter).toFuture(), 10.seconds)
      if (result.getDeletedCount > 1)
        logger.info(s"MovieRepository.delete($title, $year) removed ${result.getDeletedCount} doc(s).")
      ()
    }.recover {
      case ex: Throwable => logger.warn(s"MovieRepository.delete($title, $year) failed: ${ex.getMessage}")
    }
  }

  def upsert(title: String, year: Option[Int], e: MovieRecord): Unit = coll.foreach { c =>
    val id   = docId(title, year)
    val dto  = StoredMovieDto.fromDomain(id, e, Instant.now())
    val opts = new ReplaceOptions().upsert(true)
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", id), dto, opts).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable if isClusterClosed(ex) =>
        // Shutdown race — the lifecycle closed the MongoClient while a worker
        // was still mid-write. Harmless: the in-memory cache already has the
        // value and the next refresh will persist it.
        logger.debug(s"MovieRepository.upsert($title, $year) skipped — Mongo client closing.")
      case ex: Throwable =>
        logger.warn(s"MovieRepository.upsert($title, $year) failed: ${ex.getMessage}")
    }
  }

  def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord): Boolean = coll match {
    case None => false
    case Some(c) =>
      val id    = docId(title, year)
      val patch = MovieRecordPatch.diff(before, after)
      Try {
        // MongoDB update-operator paths treat '.' as a nesting separator, so a
        // per-source `$set`/`$unset` on `sourceData.<displayName>` is rejected
        // (code 56, "empty field name") when a source's displayName contains a
        // dot — e.g. "Helios Ostrów Wlkp.". A full-document write DOES allow
        // dotted field names, so for those rows fall back to a conditional
        // replaceOne (still "update only if present"); the common dot-free case
        // keeps the precise field-level diff that preserves out-of-band edits.
        val matched =
          if (patch.data.keysIterator.exists(_.displayName.contains('.'))) {
            val dto = StoredMovieDto.fromDomain(id, after, Instant.now())
            Await.result(c.replaceOne(Filters.eq("_id", id), dto, new ReplaceOptions().upsert(false)).toFuture(), 10.seconds)
              .getMatchedCount
          } else {
            Await.result(c.updateOne(Filters.eq("_id", id), patchToUpdate(patch), new UpdateOptions().upsert(false)).toFuture(), 10.seconds)
              .getMatchedCount
          }
        matched > 0
      }.recover {
        case ex: Throwable if isClusterClosed(ex) => false
        case ex: Throwable =>
          logger.warn(s"MovieRepository.updateIfPresent($title, $year) failed: ${ex.getMessage}")
          false
      }.getOrElse(false)
  }

  // Translate a `MovieRecordPatch` into a `$set`/`$unset` Mongo update. Each
  // scalar field gets its own atom; the `data` map gets per-source
  // `sourceData.<sourceName>` paths so a Tmdb-only refresh doesn't touch a
  // cinema's slot and vice versa. `updatedAt` always bumps so write tracking
  // works even when the patch is otherwise empty (the row was touched).
  private def patchToUpdate(p: MovieRecordPatch): Bson = {
    val atoms = scala.collection.mutable.ListBuffer.empty[Bson]
    def scalar[A](field: String, u: FieldUpdate[A], toBson: A => org.bson.BsonValue): Unit = u match {
      case FieldUpdate.NoChange => ()
      case FieldUpdate.Unset    => atoms += Updates.unset(field)
      case FieldUpdate.SetTo(v) => atoms += Updates.set(field, toBson(v))
    }
    scalar("imdbId",            p.imdbId,            (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("imdbRating",        p.imdbRating,        (d: Double) => new org.mongodb.scala.bson.BsonDouble(d))
    scalar("metascore",         p.metascore,         (i: Int)    => new org.mongodb.scala.bson.BsonInt32(i))
    scalar("filmwebUrl",        p.filmwebUrl,        (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("filmwebRating",     p.filmwebRating,     (d: Double) => new org.mongodb.scala.bson.BsonDouble(d))
    scalar("rottenTomatoes",    p.rottenTomatoes,    (i: Int)    => new org.mongodb.scala.bson.BsonInt32(i))
    scalar("tmdbId",            p.tmdbId,            (i: Int)    => new org.mongodb.scala.bson.BsonInt32(i))
    scalar("metacriticUrl",     p.metacriticUrl,     (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("rottenTomatoesUrl", p.rottenTomatoesUrl, (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("tmdbNoMatch",       p.tmdbNoMatch,       (b: Boolean) => new org.mongodb.scala.bson.BsonBoolean(b))
    scalar("detailPending",     p.detailPending,     (b: Boolean) => new org.mongodb.scala.bson.BsonBoolean(b))
    p.data.foreach {
      case (source, FieldUpdate.SetTo(sd)) => atoms += Updates.set(s"sourceData.${source.displayName}", sd)
      case (source, FieldUpdate.Unset)     => atoms += Updates.unset(s"sourceData.${source.displayName}")
      case (_, FieldUpdate.NoChange)       => ()
    }
    atoms += Updates.set("updatedAt", BsonDateTime(Instant.now().toEpochMilli))
    Updates.combine(atoms.toSeq*)
  }

  /** Open a MongoDB change stream and route each change to `onUpsert` /
   *  `onDelete`. `UPDATE_LOOKUP` makes insert/update/replace events carry the
   *  full post-image (not just the delta), so we always hand a complete row to
   *  `onUpsert`. A DELETE has no `fullDocument`, so we surface its `documentKey._id`
   *  to `onDelete` instead (what the cache's periodic backstop used to be the
   *  only path for, and what the /debug live view needs so a merged-away row
   *  disappears). The driver auto-resumes across transient blips; a terminal
   *  error just logs. Requires a replica set (a single-node RS counts); on a
   *  standalone Mongo the stream errors out and the caller falls back to its
   *  backstop. */
  override def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: String => Unit
  ): Option[AutoCloseable] = coll.map { c =>
    val subRef = new AtomicReference[Subscription]()
    c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
      .subscribe(new Observer[ChangeStreamDocument[StoredMovieDto]] {
        override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[StoredMovieDto]): Unit =
          Option(change.getFullDocument) match {
            case Some(dto) =>
              try onUpsert(StoredMovieDto.toDomain(dto))
              catch { case ex: Throwable => logger.warn(s"MovieRepository change-stream apply failed: ${ex.getMessage}") }
            case None =>
              // No post-image ⇒ a delete (the only op UPDATE_LOOKUP can't
              // back-fill). Surface its _id so the consumer can drop the row.
              Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
                .map(v => if (v.isString) v.asString.getValue else v.toString)
                .foreach { id =>
                  try onDelete(id)
                  catch { case ex: Throwable => logger.warn(s"MovieRepository change-stream delete apply failed: ${ex.getMessage}") }
                }
          }
        override def onError(e: Throwable): Unit =
          logger.warn(s"MovieRepository change stream ended (${e.getMessage}) — relying on the periodic backstop rehydrate.")
        override def onComplete(): Unit = ()
      })
    logger.info("MongoMovieRepository: watching change stream for incremental cache updates.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }

  def close(): Unit = clientOpt.foreach(_.close())

  /** Index `(title, year)` so [[delete]]'s `$or(_id, title+year)` filter resolves
   *  by index union instead of a full collection scan. The stored docs no longer
   *  carry `title`/`year` columns (the 2026-06-11 derived-title migration dropped
   *  them), so for current rows the second `$or` branch matches nothing — but
   *  without this index Mongo still COLLSCANs the whole collection to prove that
   *  on every delete (~400ms / ~1100 docs examined per delete; the single largest
   *  source of `movies` read-lock time in prod). With the index the branch is a
   *  1-key IXSCAN. The index stays cheap (currently all-null entries, ~24KB) and
   *  still catches any legacy stale-`_id` doc that DOES carry the columns — the
   *  delete-by-(title,year) safety net the change-stream regression depends on.
   *  Idempotent + best-effort: a re-create is a no-op, a failure only logs. */
  private def ensureIndexes(coll: MongoCollection[StoredMovieDto]): Unit =
    Try {
      Await.result(
        coll.createIndex(Indexes.ascending("title", "year"), new IndexOptions().background(true)).toFuture(),
        10.seconds)
      ()
    }.recover {
      case ex: Throwable => logger.warn(s"movies (title, year) index creation failed: ${ex.getMessage}")
    }

  private def init(): (Option[MongoClient], Option[MongoCollection[StoredMovieDto]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoMovieRepository disabled (in-memory cache only).")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName  = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client  = MongoClient(uri)
          val db      = client.getDatabase(dbName).withCodecRegistry(MovieCodecs.registry)
          // Relaxed write concern — see the sharedDb path above.
          val coll    = db.getCollection[StoredMovieDto]("movies")
            .withWriteConcern(WriteConcern.W1.withJournal(false))
          // Touch the collection to surface connectivity errors at startup,
          // not on the first read after the app is "up".
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          ensureIndexes(coll)
          logger.info(s"MongoMovieRepository connected to $dbName.movies")
          (client, coll)
        }.recover {
          case ex: Throwable =>
            logger.error(s"MongoMovieRepository init failed (${ex.getMessage}) — falling back to in-memory cache.")
            null
        }.toOption.filter(_ != null) match {
          case Some((c, coll)) => (Some(c), Some(coll))
          case None            => (None, None)
        }
    }

  // The driver throws IllegalStateException("state should be: open") from
  // BaseCluster / DefaultConnectionPool once MongoClient.close() has fired.
  private def isClusterClosed(ex: Throwable): Boolean =
    Option(ex.getMessage).exists(_.contains("state should be: open"))

  // Match the in-memory CacheKey's normalization rules so case-only and
  // diacritic variants of the same title share a single Mongo record. Without
  // this, "Tom i Jerry: Przygoda w muzeum" and "Tom i jerry: przygoda w
  // muzeum" — both reported by different cinemas for the same film — each get
  // their own row, and only one can be updated per hourly refresh tick (the
  // tick walks the deduplicated Caffeine cache).
  private def docId(title: String, year: Option[Int]): String =
    StoredMovieRecord.idFor(title, year)
}
