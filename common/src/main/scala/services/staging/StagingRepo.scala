package services.staging

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import models.{MovieRecord, Source}
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, Observer, SingleObservableFuture, Subscription}
import play.api.Logging
import services.movies.{MovieCodecs, StoredMovieDto, TitleNormalizer}

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** One per-cinema staging row: a single cinema's report of a film that has not
 *  yet been TMDB-concluded. Unlike `movies` (one merged row per film across all
 *  cinemas), staging is one row per `cinema|title|year` — the raw, un-merged
 *  facts a newcomer arrives with. The `record` carries that one cinema's slot
 *  (plus a `Tmdb` slot once resolution runs); folding into `movies` unions the
 *  cinemas back together using the existing merge rules. */
case class StagingRecord(cinema: Source, title: String, year: Option[Int], record: MovieRecord)

object StagingRecord {
  /** The Mongo `_id` for a staging row: `cinemaDisplayName|sanitize(title)|year`.
   *  A `Cinema.displayName` never contains `|` and `sanitize` never emits one, so
   *  the first `|` ends the cinema and the last `|` precedes the year — the middle
   *  is the sanitized title (the same prefix `movies` keys its `_id` on). */
  def idFor(cinema: Source, title: String, year: Option[Int]): String =
    s"${cinema.displayName}|${TitleNormalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"

  /** Rebuild a staging row from its persisted `_id` + `MovieRecord`. The display
   *  title is derived from the record's slots (same as `StoredMovieRecord`), the
   *  year + cinema from the `_id`. Returns None for a row whose cinema segment is
   *  unknown (a dropped/renamed cinema), matching the codec's drop-unknown-source
   *  behaviour. */
  def fromStorage(id: String, record: MovieRecord): Option[StagingRecord] = {
    val firstSep = id.indexOf('|')
    val lastSep  = id.lastIndexOf('|')
    if (firstSep < 0 || lastSep <= firstSep) None
    else {
      val cinemaName = id.substring(0, firstSep)
      val prefix     = id.substring(firstSep + 1, lastSep)
      val year       = id.substring(lastSep + 1).toIntOption
      Source.byDisplayName.get(cinemaName).map(src => StagingRecord(src, record.displayTitle(prefix), year, record))
    }
  }
}

/**
 * Persistent store for per-cinema newcomer rows awaiting TMDB conclusion — the
 * `pending_movies` collection. A genuinely-new `(title, year)` incubates here
 * (one row per cinema) until it resolves, then a transactional fold moves it
 * into `movies` and deletes the staging rows. The trait is what consumers see;
 * `MongoStagingRepo` (prod) and `InMemoryStagingRepo` (tests) are the impls,
 * wired by the trait per CLAUDE.md's DIP guidance.
 */
trait StagingRepo {
  /** Whether the persistence layer is wired up; writes no-op when false. */
  def enabled: Boolean

  /** Every staging row, ordered by `_id`. Returns empty when disabled. */
  def findAll(): Seq[StagingRecord]

  /** Write-through upsert of one cinema's row. Best-effort — never throws. */
  def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit

  /** Remove one cinema's row. Best-effort — never throws. */
  def delete(cinema: Source, title: String, year: Option[Int]): Unit

  /** Stream inserted/updated rows so the promoter can enrich them as they land.
   *  Best-effort; `None` when unsupported (disabled, or standalone Mongo). */
  def watchUpserts(onUpsert: StagingRecord => Unit): Option[AutoCloseable] = None

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit = ()
}

object StagingRepo {
  /** A disabled, empty no-op `StagingRepo` — the default for callers that don't
   *  wire staging (e.g. the web `/debug` controller in tests, or any non-staging
   *  build). `findAll` is empty and writes are dropped. */
  val empty: StagingRepo = new StagingRepo {
    def enabled: Boolean = false
    def findAll(): Seq[StagingRecord] = Seq.empty
    def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit = ()
    def delete(cinema: Source, title: String, year: Option[Int]): Unit = ()
  }
}

/**
 * MongoDB-backed `StagingRepo` over the `pending_movies` collection. Reuses the
 * `movies` storage shape (`StoredMovieDto` + `MovieCodecs.registry`) — a staging
 * row is just a `MovieRecord` with a single-cinema `data` map — differing only in
 * the collection name and the cinema-scoped `_id`. Mirrors `MongoMovieRepo`'s
 * relaxed write concern: `pending_movies` is re-scraped continuously and its rows
 * are transient, so a write lost to a crash is recovered by the next scrape.
 */
class MongoStagingRepo(sharedDb: Option[MongoDatabase] = None) extends StagingRepo with Logging {

  private lazy val coll: Option[MongoCollection[StoredMovieDto]] =
    sharedDb.map { db =>
      db.withCodecRegistry(MovieCodecs.registry)
        .getCollection[StoredMovieDto]("pending_movies")
        .withWriteConcern(WriteConcern.W1.withJournal(false))
    }

  def enabled: Boolean = coll.isDefined

  def findAll(): Seq[StagingRecord] = coll match {
    case None => Seq.empty
    case Some(c) =>
      Try {
        Await.result(c.find().sort(Sorts.ascending("_id")).toFuture(), 60.seconds)
          .flatMap(dto => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto).record))
      }.recover {
        case ex: Throwable =>
          logger.warn(s"StagingRepo.findAll failed: ${ex.getClass.getSimpleName}: ${ex.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
  }

  def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit = coll.foreach { c =>
    val id  = StagingRecord.idFor(cinema, title, year)
    val dto = StoredMovieDto.fromDomain(id, record, Instant.now())
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", id), dto, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable => logger.warn(s"StagingRepo.upsert($id) failed: ${ex.getMessage}")
    }
  }

  def delete(cinema: Source, title: String, year: Option[Int]): Unit = coll.foreach { c =>
    val id = StagingRecord.idFor(cinema, title, year)
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable => logger.warn(s"StagingRepo.delete($id) failed: ${ex.getMessage}")
    }
  }

  override def watchUpserts(onUpsert: StagingRecord => Unit): Option[AutoCloseable] = coll.map { c =>
    val subRef = new AtomicReference[Subscription]()
    c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
      .subscribe(new Observer[ChangeStreamDocument[StoredMovieDto]] {
        override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[StoredMovieDto]): Unit =
          Option(change.getFullDocument).foreach { dto =>
            StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto).record).foreach { row =>
              try onUpsert(row)
              catch { case ex: Throwable => logger.warn(s"StagingRepo change-stream apply failed: ${ex.getMessage}") }
            }
          }
        override def onError(e: Throwable): Unit =
          logger.warn(s"StagingRepo change stream ended (${e.getMessage}) — relying on the periodic backstop.")
        override def onComplete(): Unit = ()
      })
    logger.info("MongoStagingRepo: watching pending_movies change stream.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }
}
