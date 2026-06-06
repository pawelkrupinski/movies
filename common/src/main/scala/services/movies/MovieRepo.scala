package services.movies

import com.mongodb.client.model.{ReplaceOptions, UpdateOptions}
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import models.MovieRecord
import org.mongodb.scala.bson.{BsonDateTime, BsonNull}
import org.mongodb.scala.model.{Filters, Updates}
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription}
import org.bson.conversions.Bson
import play.api.Logging
import tools.{DaemonExecutors, Env}

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService}
import scala.concurrent.duration._
import scala.util.Try

/** One persisted (title, year) → MovieRecord row. Used as the return type
 *  of `MovieRepo.findAll` and `MovieCache.snapshot` so callers iterate
 *  named fields instead of destructuring an anonymous 3-tuple. */
case class StoredMovieRecord(title: String, year: Option[Int], record: MovieRecord)

/**
 * Persistent store for `(title, year) → MovieRecord` records.
 *
 * The trait is what consumers (`MovieCache`, scripts, integration tests) see
 * — `MongoMovieRepo` (production) and `InMemoryMovieRepo` (tests) are the two
 * implementations. Per CLAUDE.md's DIP guidance: every collaborator is wired
 * via the trait; the concrete type only appears at the composition root
 * (`AppLoader`) and in test setup.
 */
trait MovieRepo {
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
  def watchUpserts(onUpsert: StoredMovieRecord => Unit): Option[AutoCloseable] = None

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit
}

object MongoMovieRepo {
  /** Split the sorted `_id` list into `parallelism` contiguous, *half-open*
   *  ranges that together tile the entire id space: the first range has no
   *  lower bound, the last no upper bound, and adjacent ranges meet exactly
   *  at a chunk head — range `i` is `[headᵢ, headᵢ₊₁)`. This leaves no gap
   *  between ranges, so a document committed between the `_id` discovery
   *  query and the ranged finds still lands in exactly one range.
   *
   *  The older approach bounded each chunk by its own observed min/max id
   *  (`gte(headᵢ) … lte(lastᵢ)`), which left a gap `(lastᵢ, headᵢ₊₁)`
   *  matching no range — a concurrent insert whose `_id` sorted into that
   *  gap was silently dropped from the reload.
   *
   *  Precondition: `ids` is sorted in the same order the ranged `gte`/`lt`
   *  finds compare against — i.e. Mongo's `_id` order. `findAll` guarantees
   *  this by sorting server-side rather than in Scala.
   *
   *  Returns one `(lower, upper)` pair per chunk; `None` means unbounded.
   *  Empty input → empty output. */
  private[movies] def idRanges(ids: Seq[String], parallelism: Int): Seq[(Option[String], Option[String])] =
    if (ids.isEmpty) Seq.empty
    else {
      val chunkSize = math.ceil(ids.size.toDouble / parallelism).toInt.max(1)
      val heads     = ids.grouped(chunkSize).map(_.head).toVector
      heads.indices.map { i =>
        val lower = if (i == 0) None else Some(heads(i))
        val upper = if (i == heads.size - 1) None else Some(heads(i + 1))
        (lower, upper)
      }
    }
}

/**
 * MongoDB-backed `MovieRepo`. Persists records to the `movies` collection.
 *
 * When `MONGODB_URI` is unset the repo silently no-ops — local dev / tests
 * without Atlas connectivity keep working off the in-memory cache only.
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
class MongoMovieRepo(
  sharedDb: Option[MongoDatabase] = None,
  // Scripts pass `sharedDb = None` and expect us to connect from
  // `MONGODB_URI` ourselves (default true). Wiring sets it to false:
  // when `MongoConnection` is already attempted, an explicit `None`
  // means it failed and re-running our own init would just hit the
  // same DNS / TLS timeout twice. Saves ~15s of boot time on the
  // offline / unreachable-cluster path.
  fallbackToOwnInit: Boolean = true
) extends MovieRepo with Logging {

  // Lazy so subclasses that override every wire method (e.g.
  // `InMemoryMovieRepo` in tests) never trigger a Mongo connection
  // attempt — `new InMemoryMovieRepo()` was waiting 10 seconds per test
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
  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[StoredMovieDto]], Option[MongoCollection[org.mongodb.scala.Document]]) =
    sharedDb match {
      case Some(db) =>
        val withRegistry = db.withCodecRegistry(MovieCodecs.registry)
        val coll    = withRegistry.getCollection[StoredMovieDto]("movies")
        val rawColl = withRegistry.getCollection("movies")
        (None, Some(coll), Some(rawColl))
      case None if fallbackToOwnInit => init()
      case None                      => (None, None, None)
    }
  private def clientOpt: Option[MongoClient]                                 = initResult._1
  private def coll:      Option[MongoCollection[StoredMovieDto]]             = initResult._2
  private def rawColl:   Option[MongoCollection[org.mongodb.scala.Document]] = initResult._3

  def enabled: Boolean = coll.isDefined

  // The ranged `findAll` fan-out runs its `Future.sequence` join on a
  // virtual-thread EC rather than the global ForkJoinPool, matching every
  // other background path in the app (see `DaemonExecutors`). Virtual threads
  // park cheaply while the Mongo driver's reactive callbacks complete, so the
  // join never pins a scarce carrier thread. Owned here and shut down by
  // `close()`. `private[movies]` only so the repo spec can assert the EC is
  // virtual-threaded.
  private[movies] val findAllEc: ExecutionContextExecutorService =
    DaemonExecutors.virtualThreadEC("movie-repo-find")

  /** Boot-time + periodic full reload. Implemented as a discover-then-fan-out
   *  pair of queries rather than one big `find()` because Atlas serialises
   *  individual cursor responses at ~50 ms/doc — a single `find()` of 231
   *  docs takes ~12 s, but four parallel ranged finds of ~58 docs each
   *  finish in ~4 s. Measured against the production cluster; see
   *  `MeasureStartup`.
   *
   *  Algorithm:
   *    1. Project just `_id` (small payload, ~200 ms).
   *    2. Sort, split into `Parallelism` contiguous *half-open* `_id` ranges
   *       (see `MongoMovieRepo.idRanges`) that tile the whole id space with
   *       no gaps — so a doc committed between this discovery query and the
   *       ranged finds can't fall between two chunk boundaries and vanish.
   *    3. Fire one ranged `find()` per range, joined.
   *    4. Decode + return.
   *
   *  Falls back to the single-query path on the discovery query's empty /
   *  failure result so a transient hiccup degrades to the previous
   *  behaviour, never worse. */
  def findAll(): Seq[StoredMovieRecord] = (coll, rawColl) match {
    case (Some(c), Some(rc)) =>
      Try {
        implicit val ec: ExecutionContext = findAllEc
        val Parallelism = 4

        // Sort in Mongo, not Scala: the chunk boundaries must be ordered by
        // the same comparison the ranged `gte`/`lt` finds use (the `_id`
        // index's binary order), or a boundary could straddle the range
        // semantics and reopen a gap. A Scala `.sorted` (UTF-16 code-unit
        // order) agrees for all BMP ids but diverges on supplementary-plane
        // characters; sorting server-side makes the partition consistent by
        // construction and rides the existing `_id` index for free.
        // Discovery timeout is 60s, not 15s, because the first call after a
        // process boot lands on a cold WiredTiger cache that can take 10–20 s
        // to assemble the response even when steady-state finds are <100 ms.
        // A short timeout here silently strands the cache empty (the
        // surrounding `Try.getOrElse(Seq.empty)` swallows TimeoutException
        // without a log line) — which is what a stale-page-on-startup bug
        // looks like from the outside.
        val ids = Await.result(
          rc.find()
            .projection(org.mongodb.scala.model.Projections.include("_id"))
            .sort(org.mongodb.scala.model.Sorts.ascending("_id"))
            .toFuture(),
          60.seconds
        ).iterator.flatMap(d =>
          d.get[org.mongodb.scala.bson.BsonString]("_id").map(_.getValue)
        ).toList

        if (ids.isEmpty) Seq.empty
        else {
          val futures = MongoMovieRepo.idRanges(ids, Parallelism).map { case (lower, upper) =>
            val bounds = Seq(
              lower.map(Filters.gte("_id", _)),
              upper.map(Filters.lt("_id", _))
            ).flatten
            val filter = bounds match {
              case Seq()    => Filters.empty()
              case Seq(one) => one
              case many     => Filters.and(many*)
            }
            c.find(filter).toFuture()
          }
          val all = Await.result(scala.concurrent.Future.sequence(futures), 60.seconds).flatten
          all.map(StoredMovieDto.toDomain)
        }
      }.recover {
        // Catch every throwable here, not just MongoException — the original
        // narrow `case MongoException` let TimeoutException (Java
        // `j.u.c.TimeoutException` from `Await.result`) and BSON decode errors
        // fall through to `.getOrElse(Seq.empty)`, which returned an empty
        // result with no log line and stranded the cache on the boot path.
        case ex: Throwable =>
          logger.warn(s"MovieRepo.findAll failed: ${ex.getClass.getSimpleName}: ${ex.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
    case _ => Seq.empty
  }

  /** Filters by `title` + `year` fields rather than by `_id`, so legacy docs
   *  whose `_id` was computed with a prior `docId` formula still get caught
   *  — `deleteOne` by `_id` would silently match nothing and the orphan
   *  would survive every startup's merge. */
  def delete(title: String, year: Option[Int]): Unit = coll.foreach { c =>
    val yearFilter = year match {
      case Some(y) => Filters.eq("year", y)
      // year=None in the in-memory model lands as either BsonNull() or a
      // missing field in legacy docs; cover both.
      case None    => Filters.or(Filters.eq("year", BsonNull()), Filters.exists("year", false))
    }
    val filter = Filters.and(Filters.eq("title", title), yearFilter)
    Try {
      val result = Await.result(c.deleteMany(filter).toFuture(), 10.seconds)
      if (result.getDeletedCount > 1)
        logger.info(s"MovieRepo.delete($title, $year) removed ${result.getDeletedCount} doc(s).")
      ()
    }.recover {
      case ex: Throwable => logger.warn(s"MovieRepo.delete($title, $year) failed: ${ex.getMessage}")
    }
  }

  def upsert(title: String, year: Option[Int], e: MovieRecord): Unit = coll.foreach { c =>
    val id   = docId(title, year)
    val dto  = StoredMovieDto.fromDomain(id, title, year, e, Instant.now())
    val opts = new ReplaceOptions().upsert(true)
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", id), dto, opts).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable if isClusterClosed(ex) =>
        // Shutdown race — the lifecycle closed the MongoClient while a worker
        // was still mid-write. Harmless: the in-memory cache already has the
        // value and the next refresh will persist it.
        logger.debug(s"MovieRepo.upsert($title, $year) skipped — Mongo client closing.")
      case ex: Throwable =>
        logger.warn(s"MovieRepo.upsert($title, $year) failed: ${ex.getMessage}")
    }
  }

  def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord): Boolean = coll match {
    case None => false
    case Some(c) =>
      val id    = docId(title, year)
      val patch = MovieRecordPatch.diff(before, after)
      val update = patchToUpdate(patch)
      val opts   = new UpdateOptions().upsert(false)
      Try {
        val result = Await.result(c.updateOne(Filters.eq("_id", id), update, opts).toFuture(), 10.seconds)
        result.getMatchedCount > 0
      }.recover {
        case ex: Throwable if isClusterClosed(ex) => false
        case ex: Throwable =>
          logger.warn(s"MovieRepo.updateIfPresent($title, $year) failed: ${ex.getMessage}")
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
    p.data.foreach {
      case (source, FieldUpdate.SetTo(sd)) => atoms += Updates.set(s"sourceData.${source.displayName}", sd)
      case (source, FieldUpdate.Unset)     => atoms += Updates.unset(s"sourceData.${source.displayName}")
      case (_, FieldUpdate.NoChange)       => ()
    }
    atoms += Updates.set("updatedAt", BsonDateTime(Instant.now().toEpochMilli))
    Updates.combine(atoms.toSeq*)
  }

  /** Open a MongoDB change stream and route each inserted / updated / replaced
   *  doc to `onUpsert`. `UPDATE_LOOKUP` makes update events carry the full
   *  post-image (not just the delta), so we always hand the cache a complete
   *  row. Delete events have no `fullDocument` and are skipped — the periodic
   *  backstop rehydrate reconciles those. The mongo driver auto-resumes the
   *  stream across transient blips; a terminal error just logs and leaves the
   *  backstop in charge. Requires a replica set (a single-node RS counts); on a
   *  standalone Mongo the stream errors out and we fall back to the backstop. */
  override def watchUpserts(onUpsert: StoredMovieRecord => Unit): Option[AutoCloseable] = coll.map { c =>
    val subRef = new AtomicReference[Subscription]()
    c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
      .subscribe(new Observer[ChangeStreamDocument[StoredMovieDto]] {
        override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[StoredMovieDto]): Unit =
          Option(change.getFullDocument).foreach { dto =>
            try onUpsert(StoredMovieDto.toDomain(dto))
            catch { case ex: Throwable => logger.warn(s"MovieRepo change-stream apply failed: ${ex.getMessage}") }
          }
        override def onError(e: Throwable): Unit =
          logger.warn(s"MovieRepo change stream ended (${e.getMessage}) — relying on the periodic backstop rehydrate.")
        override def onComplete(): Unit = ()
      })
    logger.info("MongoMovieRepo: watching change stream for incremental cache updates.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }

  def close(): Unit = {
    clientOpt.foreach(_.close())
    findAllEc.shutdown()
  }

  private def init(): (Option[MongoClient], Option[MongoCollection[StoredMovieDto]], Option[MongoCollection[org.mongodb.scala.Document]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoMovieRepo disabled (in-memory cache only).")
        (None, None, None)
      case Some(uri) =>
        Try {
          val dbName  = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client  = MongoClient(uri)
          val db      = client.getDatabase(dbName).withCodecRegistry(MovieCodecs.registry)
          val coll    = db.getCollection[StoredMovieDto]("movies")
          val rawColl = db.getCollection("movies")
          // Touch the collection to surface connectivity errors at startup,
          // not on the first read after the app is "up".
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          logger.info(s"MongoMovieRepo connected to $dbName.movies")
          (client, coll, rawColl)
        }.recover {
          case ex: Throwable =>
            logger.error(s"MongoMovieRepo init failed (${ex.getMessage}) — falling back to in-memory cache.")
            null
        }.toOption.filter(_ != null) match {
          case Some((c, coll, rawColl)) => (Some(c), Some(coll), Some(rawColl))
          case None                     => (None, None, None)
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
    s"${TitleNormalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"
}
