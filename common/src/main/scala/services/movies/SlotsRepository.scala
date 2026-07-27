package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import com.mongodb.client.model.changestream.ChangeStreamDocument
import models.{Source, SourceData}
import org.mongodb.scala.model.{BulkWriteOptions, DeleteManyModel, Filters, Indexes, ReplaceOneModel, Sorts}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, Observer, SingleObservableFuture, Subscription}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Per-cinema `SourceData` slots, split out of the embedded `movies.sourceData` map
 * into their own `movie_slots` collection — the same move [[ScreeningsRepository]]
 * made for showtimes, for the same reason and one level further.
 *
 * WHY (measured, 2026-07-27 UK heap dump): the `movies` change stream runs with
 * `FullDocument.UPDATE_LOOKUP`, so every event carries the WHOLE film document, and
 * those documents are captured by lambdas queued on an unbounded single-thread
 * executor. One popular film (~471 UK venues) had 23 events queued during a scrape
 * burst, so ONE poster URL was resident 10,848 times — 471 slots x 23 in-flight
 * copies. Splitting the slots out makes each event carry one slot instead of the
 * whole fat document, which removes the per-event multiplier at its source. It also
 * stops one cinema's re-scrape rewriting every OTHER cinema's slot on the same film.
 *
 * One row per cinema slot, keyed by `(filmId, slotKey)` where `slotKey` is the slot's
 * wire form — `Source.displayName` (`"<cinema>␟<titleKey>"`) — i.e. exactly the key
 * `movies.sourceData` and `screenings` already use, so all three stay addressable by
 * the same string.
 *
 * Slots are stored WITHOUT showtimes: those remain authoritative in `screenings`, and
 * the index-only cache depends on that separation ([[ShowtimesDigest]]). A slot here
 * is the metadata half — titles, synopsis, cast, poster/film urls, year.
 *
 * Two implementations share this contract: [[MongoSlotsRepository]] (durable,
 * multi-instance-safe) and [[InMemorySlotsRepository]] (tests / Mongo-less dev).
 * There is no business logic in either — both just store — so neither can drift from
 * the other's understanding of the rules.
 */
trait SlotsRepository {

  /** Every slot of one film: `slotKey -> slot`. Empty when the film has none. */
  def findForFilm(filmId: String): Map[String, SourceData]

  /** Every film's slots: `filmId -> (slotKey -> slot)`. For the boot hydrate /
   *  `findAll` read-stitch. */
  def findAll(): Map[String, Map[String, SourceData]]

  /** Set a film's slots to EXACTLY `slots` — upsert those present, delete any no
   *  longer present. The whole-record write path. */
  def replaceFilm(filmId: String, slots: Map[String, SourceData]): Unit

  /** Upsert one slot — the per-slot patch write path. */
  def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit

  /** Drop one slot (it left the film's listings). */
  def deleteSlot(filmId: String, slotKey: String): Unit

  /** Drop all of a film's slots (the film was deleted / re-keyed). */
  def deleteFilm(filmId: String): Unit

  /** Push: ring `onChange(filmId)` whenever a film's slots actually change. A no-op
   *  write does NOT ring, mirroring the `movies` and `screenings` guards. Returns a
   *  handle to stop watching, or None when this impl can't push. */
  def watch(onChange: String => Unit): Option[AutoCloseable] = None

  def close(): Unit = ()
}

/**
 * In-memory `SlotsRepository` for tests and Mongo-less dev. Mirrors
 * [[MongoSlotsRepository]]'s semantics: idempotent per-slot writes, per-film
 * grouping, and a ring that fires only on a real change.
 */
class InMemorySlotsRepository extends SlotsRepository {

  private val byFilm    = scala.collection.mutable.Map.empty[String, Map[String, SourceData]]
  private val lock      = new Object
  private val listeners = new CopyOnWriteArrayList[String => Unit]()

  def findForFilm(filmId: String): Map[String, SourceData] =
    lock.synchronized(byFilm.getOrElse(filmId, Map.empty))

  def findAll(): Map[String, Map[String, SourceData]] = lock.synchronized(byFilm.toMap)

  def replaceFilm(filmId: String, slots: Map[String, SourceData]): Unit = {
    val changed = lock.synchronized {
      if (byFilm.getOrElse(filmId, Map.empty) == slots) false
      else { if (slots.isEmpty) byFilm.remove(filmId) else byFilm.update(filmId, slots); true }
    }
    if (changed) ring(filmId)
  }

  def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit = {
    val changed = lock.synchronized {
      val cur = byFilm.getOrElse(filmId, Map.empty)
      if (cur.get(slotKey).contains(slot)) false
      else { byFilm.update(filmId, cur + (slotKey -> slot)); true }
    }
    if (changed) ring(filmId)
  }

  def deleteSlot(filmId: String, slotKey: String): Unit = {
    val changed = lock.synchronized {
      val cur = byFilm.getOrElse(filmId, Map.empty)
      if (!cur.contains(slotKey)) false
      else { val next = cur - slotKey; if (next.isEmpty) byFilm.remove(filmId) else byFilm.update(filmId, next); true }
    }
    if (changed) ring(filmId)
  }

  def deleteFilm(filmId: String): Unit = {
    val changed = lock.synchronized(byFilm.remove(filmId).isDefined)
    if (changed) ring(filmId)
  }

  override def watch(onChange: String => Unit): Option[AutoCloseable] = {
    listeners.add(onChange)
    Some(new AutoCloseable { override def close(): Unit = { listeners.remove(onChange); () } })
  }

  private def ring(filmId: String): Unit = listeners.asScala.foreach(_(filmId))
}

object SlotsRepository {

  /** A record's slots in wire form, ready to store. Showtimes are dropped — they are
   *  authoritative in `screenings`, and storing them twice would let the two disagree. */
  def slotsOf(data: Map[Source, SourceData]): Map[String, SourceData] =
    data.iterator.map { case (s, sd) =>
      s.displayName -> (if (sd.showtimes.isEmpty) sd else sd.copy(showtimes = Seq.empty))
    }.toMap

  /** Rebuild a record's `data` map from stored wire keys. Mirrors the `movies` codec's
   *  decode exactly: a key that no longer names a known `Source` is dropped (a retired
   *  cinema), and `dropSupersededCinemaSlots` re-applies afterwards so a bare-cinema slot
   *  superseded by per-title slots doesn't resurrect. */
  def stitch(slots: Map[String, SourceData]): Map[Source, SourceData] =
    Source.dropSupersededCinemaSlots(
      slots.iterator.flatMap { case (k, sd) => Source.byWireKey(k).map(_ -> sd) }.toMap)

  /** The per-slot writes needed to turn `before` into `after`: `slotKey -> Some(slot)`
   *  to upsert, `slotKey -> None` to delete. Only genuinely-changed slots appear, so an
   *  unrelated-field change writes nothing here. Pure + unit-tested. */
  def slotOps(before: Map[Source, SourceData], after: Map[Source, SourceData]): Map[String, Option[SourceData]] = {
    val b = slotsOf(before)
    val a = slotsOf(after)
    (b.keySet ++ a.keySet).iterator.flatMap { k =>
      (b.get(k), a.get(k)) match {
        case (x, y) if x == y => None
        case (_, Some(y))     => Some(k -> Some(y))
        case (_, None)        => Some(k -> None)
      }
    }.toMap
  }
}

/** Storage DTO for one cinema slot's metadata — the macro codec target for the
 *  `movie_slots` collection. `_id = "<filmId><slotKey>"`; `filmId` is indexed
 *  for per-film reads/deletes. */
case class StoredSlotDto(
  _id:       String,
  filmId:    String,
  slotKey:   String,
  slot:      SourceData,
  updatedAt: Instant
)

/**
 * Mongo-backed `SlotsRepository`, collection `movie_slots`. Relaxed write concern
 * like `movies` / `screenings` (re-scraped continuously; a lost write self-heals on
 * the next scrape). Every method is defensively `Try`-guarded so a slots failure can
 * never break the caller's `movies` write.
 */
class MongoSlotsRepository(
  sharedDb: Option[MongoDatabase],
  // Keyset page size for the full-collection scan — same StackOverflow defence as
  // `screenings` (Sentry KINOWO-19). These docs are fatter than a screenings row, so
  // the page is smaller. Injectable so tests can force multiple pages.
  findAllBatchSize:     Int            = 250,
  findAllBatchAttempts: Int            = 4,
  findAllBatchBackoff:  FiniteDuration = 500.millis,
  // Persist this stream's resume token so a restart replays slot changes that landed
  // while down. ON only in the worker; OFF for web /debug + scripts.
  persistResumeToken:   Boolean        = false
) extends SlotsRepository with Logging {
  import SlotKeyed.idOf

  private lazy val coll: Option[MongoCollection[StoredSlotDto]] = sharedDb.map { db =>
    val c = db.withCodecRegistry(MovieCodecs.registry).getCollection[StoredSlotDto]("movie_slots")
      .withWriteConcern(WriteConcern.W1.withJournal(false))
    Try(Await.result(c.createIndex(Indexes.ascending("filmId")).toFuture(), 10.seconds))
    c
  }

  private val resumeToken = new ChangeStreamResumeToken("movie_slots", sharedDb, persistResumeToken)

  def findForFilm(filmId: String): Map[String, SourceData] = coll.fold(Map.empty[String, SourceData]) { c =>
    Try(Await.result(c.find(Filters.eq("filmId", filmId)).toFuture(), 30.seconds))
      .getOrElse(Seq.empty).map(d => d.slotKey -> d.slot).toMap
  }

  /** Every film's slots, keyset-paged by `_id` — see [[MongoScreeningsRepository.findAll]]
   *  for why a single unbounded cursor is not safe here. An INCOMPLETE scan returns an
   *  empty map so a caller can treat it as "unknown" rather than "the film has no slots"
   *  and prune on it. */
  def findAll(): Map[String, Map[String, SourceData]] = coll match {
    case Some(c) =>
      val buf = Vector.newBuilder[StoredSlotDto]
      val complete = KeysetScan.scan[StoredSlotDto](
        label          = "SlotsRepository keyset batch",
        batchSize      = findAllBatchSize,
        maxAttempts    = findAllBatchAttempts,
        initialBackoff = findAllBatchBackoff,
        keyOf          = _._id,
        fetchPage      = (afterId, limit) => {
          val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
          Await.result(c.find(filter).sort(Sorts.ascending("_id")).limit(limit).toFuture(), 60.seconds)
        },
        onIncomplete   = exception =>
          logger.warn(s"SlotsRepository.findAll keyset scan failed after retries: " +
            s"${exception.getClass.getSimpleName}: ${exception.getMessage} — returning empty")
      )(batch => buf ++= batch)
      if (complete) buf.result().groupBy(_.filmId).view.mapValues(_.map(d => d.slotKey -> d.slot).toMap).toMap
      else Map.empty
    case None => Map.empty
  }

  /** ONE ordered bulk round-trip: every slot's upsert plus a single `deleteMany` of
   *  whatever `slots` no longer names — the same shape as
   *  [[MongoScreeningsRepository.replaceFilm]], including the `$nin: []` edge case where
   *  an EMPTY `slots` clears every slot of the film. */
  def replaceFilm(filmId: String, slots: Map[String, SourceData]): Unit = coll.foreach { c =>
    Try {
      val now     = Instant.now()
      val upserts = slots.toSeq.map { case (k, sd) =>
        val dto = StoredSlotDto(idOf(filmId, k), filmId, k, sd, now)
        ReplaceOneModel(Filters.eq("_id", dto._id), dto, new ReplaceOptions().upsert(true))
      }
      val dropStale = DeleteManyModel[StoredSlotDto](SlotKeyed.staleSlotsFilter(filmId, slots.keySet))
      val result    = Await.result(c.bulkWrite(upserts :+ dropStale, new BulkWriteOptions().ordered(true)).toFuture(), 30.seconds)
      if (result.getDeletedCount > 0)
        RemovalAudit.screeningsCleared("movie_slots.replaceFilm", filmId, result.getDeletedCount.toInt,
          whole = slots.isEmpty, reason = "stale-slot-prune")
    }.recover { case e => logger.warn(s"SlotsRepository.replaceFilm($filmId) failed: ${e.getMessage}") }
  }

  def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit = coll.foreach { c =>
    Try {
      val dto = StoredSlotDto(idOf(filmId, slotKey), filmId, slotKey, slot, Instant.now())
      Await.result(c.replaceOne(Filters.eq("_id", dto._id), dto, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds); ()
    }.recover { case e => logger.warn(s"SlotsRepository.upsertSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  def deleteSlot(filmId: String, slotKey: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", idOf(filmId, slotKey))).toFuture(), 10.seconds)
      RemovalAudit.slotRemoved("movie_slots.deleteSlot", filmId, slotKey, "slot-deleted")
    }.recover { case e => logger.warn(s"SlotsRepository.deleteSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  def deleteFilm(filmId: String): Unit = coll.foreach { c =>
    Try {
      val deleted = Await.result(c.deleteMany(Filters.eq("filmId", filmId)).toFuture(), 10.seconds).getDeletedCount
      if (deleted > 0)
        RemovalAudit.screeningsCleared("movie_slots.deleteFilm", filmId, deleted.toInt, whole = true, reason = "film-deleted")
    }.recover { case e => logger.warn(s"SlotsRepository.deleteFilm($filmId) failed: ${e.getMessage}") }
  }

  /** Watch `movie_slots`; ring `onChange(filmId)` for every change. Insert/update/replace
   *  carry the doc's `filmId`; a delete carries only the composite `_id`, whose prefix is
   *  the filmId. The caller re-reads + stitches. Requires a replica set. */
  override def watch(onChange: String => Unit): Option[AutoCloseable] = coll.map { c =>
    val subRef     = new AtomicReference[Subscription]()
    val resumeFrom = resumeToken.load()
    val base       = c.watch()
    resumeFrom.fold(base)(t => base.resumeAfter(Document(t)))
      .subscribe(new Observer[ChangeStreamDocument[StoredSlotDto]] {
        override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
        override def onNext(change: ChangeStreamDocument[StoredSlotDto]): Unit = {
          resumeToken.advance(change.getResumeToken)
          val filmId = Option(change.getFullDocument).map(_.filmId).orElse(
            Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
              .map(v => if (v.isString) v.asString.getValue else v.toString)
              .map(SlotKeyed.filmIdOf))
          filmId.foreach(fid => try onChange(fid)
            catch { case e: Throwable => logger.warn(s"movie_slots watch onChange($fid) failed: ${e.getMessage}") })
          resumeToken.save(force = false)
        }
        override def onError(e: Throwable): Unit = {
          if (ChangeStreamResumeToken.isInvalid(e)) {
            logger.warn(s"movie_slots change stream: resume token invalid (${e.getMessage}) — clearing it; " +
              "the next open starts fresh and the backstop resyncs the gap.")
            resumeToken.clear()
          } else
            logger.warn(s"movie_slots change stream ended (${e.getMessage}) — a reopen resumes from the " +
              "persisted token; the backstop covers the meantime.")
          subRef.set(null)
        }
        override def onComplete(): Unit = subRef.set(null)
      })
    logger.info(s"MongoSlotsRepository: watching movie_slots change stream" +
      s"${if (resumeFrom.isDefined) ", resumed from persisted token" else ""}.")
    new AutoCloseable { override def close(): Unit = {
      resumeToken.save(force = true)
      Option(subRef.get()).foreach(_.unsubscribe())
    } }
  }

  override def close(): Unit = resumeToken.save(force = true)
}
