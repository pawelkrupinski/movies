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
/** `id` is the row's PERSISTED `_id`. It's normally `idFor(cinema, title, year)`,
 *  but the two can DRIFT: a row is keyed once at creation, while `title` is
 *  re-derived (`fromStorage` → `displayTitle`) on every read and re-cased by
 *  `recase`. Because `sanitize` is not perfectly casing-invariant (e.g. the
 *  case-sensitive "Gwiezdne Wojny: " strip), a re-cased title can sanitize to a
 *  DIFFERENT key than the one baked into `id`. So mutations/deletes of an EXISTING
 *  row MUST go through `id` (see `upsertRow`/`deleteRow`) — recomputing the id from
 *  the drifted title would spawn a duplicate under the new key and strand the
 *  original, which the staging reaper then re-resolves forever. */
case class StagingRecord(cinema: Source, title: String, year: Option[Int], record: MovieRecord, id: String)

object StagingRecord {
  /** Build a row whose `id` is the canonical `idFor` of its fields — the right
   *  default for a FRESH row (a scrape divert, a test seed). Rows read back from
   *  storage carry their persisted `_id` instead (see `fromStorage`). */
  def apply(cinema: Source, title: String, year: Option[Int], record: MovieRecord): StagingRecord =
    StagingRecord(cinema, title, year, record, idFor(cinema, title, year))

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
      Source.byDisplayName.get(cinemaName).map(src => StagingRecord(src, record.displayTitle(prefix), year, record, id))
    }
  }
}

/**
 * Persistent store for per-cinema newcomer rows awaiting TMDB conclusion — the
 * `pending_movies` collection. A genuinely-new `(title, year)` incubates here
 * (one row per cinema) until it resolves, then a transactional fold moves it
 * into `movies` and deletes the staging rows. The trait is what consumers see;
 * `MongoStagingRepository` (prod) and `InMemoryStagingRepository` (tests) are the impls,
 * wired by the trait per CLAUDE.md's DIP guidance.
 */
trait StagingRepository {
  /** Whether the persistence layer is wired up; writes no-op when false. */
  def enabled: Boolean

  /** Every staging row, ordered by `_id`. Returns empty when disabled. */
  def findAll(): Seq[StagingRecord]

  /** Write-through upsert of one cinema's row, keyed by `idFor(cinema, title,
   *  year)`. Use for FRESH rows (a scrape divert). Best-effort — never throws. */
  def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit

  /** Write `row.record` back under the row's PERSISTED `id`. Use to re-stamp an
   *  EXISTING row (detail/resolve/imdb) so a title whose casing drifted updates
   *  the SAME row instead of spawning a duplicate. The real impls key by `row.id`;
   *  this default delegation suffices for lightweight stubs and for rows that never
   *  drift (`id == idFor(cinema, title, year)`). Best-effort — never throws. */
  def upsertRow(row: StagingRecord): Unit = upsert(row.cinema, row.title, row.year, row.record)

  /** Remove one cinema's row by `idFor(cinema, title, year)`. Best-effort. */
  def delete(cinema: Source, title: String, year: Option[Int]): Unit

  /** Remove an EXISTING row by its persisted `id` (drift-proof in the real impls;
   *  the default delegation is correct for non-drifting rows). Best-effort. */
  def deleteRow(row: StagingRecord): Unit = delete(row.cinema, row.title, row.year)

  /** Stream inserts/updates (`onUpsert`) and deletes (`onDelete`, given the row's
   *  `_id`) so consumers can react as newcomers land and graduate. Best-effort;
   *  `None` when unsupported (disabled, or standalone Mongo). */
  def watchChanges(onUpsert: StagingRecord => Unit, onDelete: String => Unit): Option[AutoCloseable] = None

  /** Stream inserted/updated rows so the promoter can enrich them as they land.
   *  Derived from `watchChanges` (deletes ignored). */
  def watchUpserts(onUpsert: StagingRecord => Unit): Option[AutoCloseable] = watchChanges(onUpsert, _ => ())

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit = ()
}

object StagingRepository {
  /** A disabled, empty no-op `StagingRepository` — the default for callers that don't
   *  wire staging (e.g. the web `/debug` controller in tests, or any non-staging
   *  build). `findAll` is empty and writes are dropped. */
  val empty: StagingRepository = new StagingRepository {
    def enabled: Boolean = false
    def findAll(): Seq[StagingRecord] = Seq.empty
    def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit = ()
    def delete(cinema: Source, title: String, year: Option[Int]): Unit = ()
  }
}

/**
 * MongoDB-backed `StagingRepository` over the `pending_movies` collection. Reuses the
 * `movies` storage shape (`StoredMovieDto` + `MovieCodecs.registry`) — a staging
 * row is just a `MovieRecord` with a single-cinema `data` map — differing only in
 * the collection name and the cinema-scoped `_id`. Mirrors `MongoMovieRepository`'s
 * relaxed write concern: `pending_movies` is re-scraped continuously and its rows
 * are transient, so a write lost to a crash is recovered by the next scrape.
 */
class MongoStagingRepository(sharedDb: Option[MongoDatabase] = None) extends StagingRepository with Logging {

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
        case exception: Throwable =>
          logger.warn(s"StagingRepository.findAll failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
  }

  def upsert(cinema: Source, title: String, year: Option[Int], record: MovieRecord): Unit =
    upsertId(StagingRecord.idFor(cinema, title, year), record)

  override def upsertRow(row: StagingRecord): Unit = upsertId(row.id, row.record)

  private def upsertId(id: String, record: MovieRecord): Unit = coll.foreach { c =>
    val dto = StoredMovieDto.fromDomain(id, record, Instant.now())
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", id), dto, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"StagingRepository.upsert($id) failed: ${exception.getMessage}")
    }
  }

  def delete(cinema: Source, title: String, year: Option[Int]): Unit =
    deleteId(StagingRecord.idFor(cinema, title, year))

  override def deleteRow(row: StagingRecord): Unit = deleteId(row.id)

  private def deleteId(id: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds)
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"StagingRepository.delete($id) failed: ${exception.getMessage}")
    }
  }

  override def watchChanges(onUpsert: StagingRecord => Unit, onDelete: String => Unit): Option[AutoCloseable] = coll.map { c =>
    val subRef = new AtomicReference[Subscription]()
    c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
      .subscribe(new Observer[ChangeStreamDocument[StoredMovieDto]] {
        override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[StoredMovieDto]): Unit =
          try Option(change.getFullDocument) match {
            case Some(dto) => StagingRecord.fromStorage(dto._id, StoredMovieDto.toDomain(dto).record).foreach(onUpsert)
            // A delete (or drop/invalidate) carries no full document — the change's
            // document key holds the `_id` of the row that graduated/left.
            case None => Option(change.getDocumentKey).map(_.getString("_id").getValue).foreach(onDelete)
          }
          catch { case exception: Throwable => logger.warn(s"StagingRepository change-stream apply failed: ${exception.getMessage}") }
        override def onError(e: Throwable): Unit =
          logger.warn(s"StagingRepository change stream ended (${e.getMessage}) — relying on the periodic backstop.")
        override def onComplete(): Unit = ()
      })
    logger.info("MongoStagingRepository: watching pending_movies change stream.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }
}
