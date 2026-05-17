package services.movies

import com.mongodb.MongoException
import com.mongodb.client.model.ReplaceOptions
import models.MovieRecord
import org.mongodb.scala.bson.BsonNull
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, MongoCollection, ObservableFuture, SingleObservableFuture}
import play.api.Logging
import tools.Env

import java.time.Instant
import scala.concurrent.Await
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
   *  resurrect the row by upserting it back into existence. */
  def updateIfPresent(title: String, year: Option[Int], e: MovieRecord): Boolean

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit
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
class MongoMovieRepo extends MovieRepo with Logging {

  // Lazy so subclasses that override every wire method (e.g.
  // `InMemoryMovieRepo` in tests) never trigger a Mongo connection
  // attempt — `new InMemoryMovieRepo()` was waiting 10 seconds per test
  // for the parent's init() to time out against an unreachable cluster.
  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[StoredMovieDto]]) = init()
  private def clientOpt: Option[MongoClient]                       = initResult._1
  private def coll:      Option[MongoCollection[StoredMovieDto]]   = initResult._2

  def enabled: Boolean = coll.isDefined

  def findAll(): Seq[StoredMovieRecord] = coll match {
    case None => Seq.empty
    case Some(c) =>
      Try {
        Await.result(c.find().toFuture(), 30.seconds).map(StoredMovieDto.toDomain)
      }.recover {
        case ex: MongoException =>
          logger.warn(s"MovieRepo.findAll failed: ${ex.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
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

  def updateIfPresent(title: String, year: Option[Int], e: MovieRecord): Boolean = coll match {
    case None => false
    case Some(c) =>
      val id   = docId(title, year)
      val dto  = StoredMovieDto.fromDomain(id, title, year, e, Instant.now())
      val opts = new ReplaceOptions().upsert(false)
      Try {
        val result = Await.result(c.replaceOne(Filters.eq("_id", id), dto, opts).toFuture(), 10.seconds)
        result.getMatchedCount > 0
      }.recover {
        case ex: Throwable if isClusterClosed(ex) => false
        case ex: Throwable =>
          logger.warn(s"MovieRepo.updateIfPresent($title, $year) failed: ${ex.getMessage}")
          false
      }.getOrElse(false)
  }

  def close(): Unit = clientOpt.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoCollection[StoredMovieDto]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoMovieRepo disabled (in-memory cache only).")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client = MongoClient(uri)
          val db     = client.getDatabase(dbName).withCodecRegistry(MovieCodecs.registry)
          val coll   = db.getCollection[StoredMovieDto]("movies")
          // Touch the collection to surface connectivity errors at startup,
          // not on the first read after the app is "up".
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          logger.info(s"MongoMovieRepo connected to $dbName.movies")
          (client, coll)
        }.recover {
          case ex: Throwable =>
            logger.error(s"MongoMovieRepo init failed (${ex.getMessage}) — falling back to in-memory cache.")
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
    s"${MovieService.normalize(title)}|${year.map(_.toString).getOrElse("")}"
}
