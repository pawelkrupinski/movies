package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import com.mongodb.client.model.changestream.ChangeStreamDocument
import models.{Showtime, Source, SourceData}
import org.mongodb.scala.model.{Filters, Indexes, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, Observer, SingleObservableFuture, Subscription}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Per-cinema showtimes, split out of the embedded `movies.sourceData` map into
 * their own `screenings` collection so a showtime change no longer rewrites the
 * (1–2 MB, 150–250-slot) film document that the change stream re-decodes on every
 * write. One row per cinema slot, keyed by `(filmId, slotKey)` where `slotKey` is
 * the slot's wire form — `Source.displayName` (`"<cinema>␟<titleKey>"`) — i.e. the
 * same key `movies.sourceData` uses.
 *
 * The domain keeps `SourceData.showtimes` in memory; this repository is purely the
 * storage split. `MongoMovieRepository` writes/reads/streams through it and stitches
 * showtimes back into each slot on read, so every downstream consumer (projection,
 * ingest merge, serving, metrics) is unchanged.
 *
 * Two implementations share this contract: [[MongoScreeningsRepository]] (durable,
 * multi-instance-safe — the authority for showtimes) and [[InMemoryScreeningsRepository]]
 * (tests / Mongo-less dev). There is no business logic here — both just store — so
 * neither needs to re-implement any rule.
 */
trait ScreeningsRepository {

  /** Every slot's showtimes for one film: `slotKey -> showtimes`. Empty when the
   *  film has no recorded screenings. */
  def findForFilm(filmId: String): Map[String, Seq[Showtime]]

  /** Every film's screenings: `filmId -> (slotKey -> showtimes)`. For the boot
   *  hydrate / `findAll` read-stitch. */
  def findAll(): Map[String, Map[String, Seq[Showtime]]]

  /** Set a film's screenings to EXACTLY `slots` — upsert those present, delete any
   *  no longer present. The whole-record write path (`MovieRepository.upsert`). */
  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]]): Unit

  /** Upsert one slot's showtimes — the per-slot patch write path
   *  (`MovieRepository.updateIfPresent`). */
  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit

  /** Drop one slot's screenings (the slot left the film's listings). */
  def deleteSlot(filmId: String, slotKey: String): Unit

  /** Drop all of a film's screenings (the film was deleted / re-keyed). */
  def deleteFilm(filmId: String): Unit

  /** Push: ring `onChange(filmId)` whenever a film's screenings actually change, so
   *  the change-stream fanout can re-stitch + re-dispatch that film. A no-op write
   *  (unchanged showtimes) does NOT ring — mirroring the `movies` no-op guard.
   *  Returns a handle to stop watching, or None when this impl can't push. */
  def watch(onChange: String => Unit): Option[AutoCloseable] = None

  def close(): Unit = ()
}

/**
 * In-memory `ScreeningsRepository` for tests and Mongo-less dev. Mirrors
 * [[MongoScreeningsRepository]]'s semantics: idempotent per-slot writes, per-film
 * grouping, and a change ring that fires only on a real change. One monitor guards
 * all mutations — fine at test/dev scale.
 */
class InMemoryScreeningsRepository extends ScreeningsRepository {

  // filmId -> (slotKey -> showtimes)
  private val byFilm    = scala.collection.mutable.Map.empty[String, Map[String, Seq[Showtime]]]
  private val lock      = new Object
  private val listeners = new CopyOnWriteArrayList[String => Unit]()

  def findForFilm(filmId: String): Map[String, Seq[Showtime]] =
    lock.synchronized(byFilm.getOrElse(filmId, Map.empty))

  def findAll(): Map[String, Map[String, Seq[Showtime]]] =
    lock.synchronized(byFilm.toMap)

  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]]): Unit = {
    val changed = lock.synchronized {
      if (byFilm.getOrElse(filmId, Map.empty) == slots) false
      else { if (slots.isEmpty) byFilm.remove(filmId) else byFilm.update(filmId, slots); true }
    }
    if (changed) ring(filmId)
  }

  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit = {
    val changed = lock.synchronized {
      val cur = byFilm.getOrElse(filmId, Map.empty)
      if (cur.get(slotKey).contains(showtimes)) false
      else { byFilm.update(filmId, cur + (slotKey -> showtimes)); true }
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

  // Ring outside the lock, only for genuine changes (see the per-method guards).
  private def ring(filmId: String): Unit = listeners.asScala.foreach(_(filmId))
}

object ScreeningsRepository {

  /** The cinema slots that carry showtimes, keyed by slot wire-key
   *  (`Source.displayName`). Tmdb/Imdb slots never have showtimes, so they're
   *  excluded. Pure — the write paths derive their screenings docs from this. */
  def showtimesOf(data: Map[Source, SourceData]): Map[String, Seq[Showtime]] =
    data.iterator.collect { case (s, sd) if sd.showtimes.nonEmpty => s.displayName -> sd.showtimes }.toMap

  /** The per-slot screening writes needed to turn `before`'s showtimes into
   *  `after`'s: `slotKey -> Some(showtimes)` to upsert, `slotKey -> None` to
   *  delete. Only slots whose showtimes actually changed appear — so a
   *  metadata-only change yields an empty map (no screening write) and a
   *  showtimes-only change writes ONLY here. Pure + unit-tested. */
  def slotOps(before: Map[Source, SourceData], after: Map[Source, SourceData]): Map[String, Option[Seq[Showtime]]] =
    (before.keySet ++ after.keySet).iterator.flatMap { s =>
      val b = before.get(s).map(_.showtimes).getOrElse(Seq.empty)
      val a = after.get(s).map(_.showtimes).getOrElse(Seq.empty)
      if (a == b) None
      else Some(s.displayName -> (if (a.nonEmpty) Some(a) else None))
    }.toMap

  /** Movies-side view of a record's data with every slot's showtimes emptied — they
   *  live in `screenings` now. Used when WRITING `movies` under the read-split, so a
   *  showtime change doesn't rewrite the (fat) film document. */
  def stripShowtimes(data: Map[Source, SourceData]): Map[Source, SourceData] =
    data.view.mapValues(sd => if (sd.showtimes.isEmpty) sd else sd.copy(showtimes = Seq.empty)).toMap

  /** Re-inject each slot's showtimes from `screenings` (keyed by slot wire-key) when
   *  READING. `screenings` is AUTHORITATIVE: a slot with a screenings doc takes its
   *  showtimes; a slot without one has none. (`movies` no longer stores showtimes —
   *  the one-time embedded→screenings migration is complete.) Pure + unit-tested. */
  def stitch(data: Map[Source, SourceData], screenings: Map[String, Seq[Showtime]]): Map[Source, SourceData] =
    data.map { case (s, sd) =>
      s -> screenings.get(s.displayName).fold(if (sd.showtimes.isEmpty) sd else sd.copy(showtimes = Seq.empty))(st => sd.copy(showtimes = st))
    }

  // Non-printable separator so the composite `_id` never collides with a slot key.
  private[movies] val IdSep: Char = '\u001f'
}

/** Storage DTO for one cinema slot's screenings — the macro codec target for the
 *  `screenings` collection. `_id = "<filmId><slotKey>"`; `filmId` is indexed
 *  for per-film reads/deletes. */
case class StoredScreeningsDto(
  _id:       String,
  filmId:    String,
  slotKey:   String,
  showtimes: Seq[Showtime],
  updatedAt: Instant
)

/**
 * Mongo-backed `ScreeningsRepository`, collection `screenings`. Relaxed write
 * concern like `movies` (re-scraped continuously; a lost write self-heals next
 * scrape). Every method is defensively `Try`-guarded so a screenings failure can
 * never break the caller's `movies` write.
 */
class MongoScreeningsRepository(
  sharedDb: Option[MongoDatabase],
  // Cursor page size for the keyset-paged full-collection scan in `findAll`. Caps how
  // many rows any ONE async cursor delivers before the next `_id`-keyset page, so a
  // full `screenings` read can't recurse the async Mongo driver's read-completion chain
  // into a StackOverflowError the way one unbounded `find().toFuture()` did once the
  // collection grew (Sentry KINOWO-19 first hit `movies.findAll`, then `screenings`).
  // These are small, single-slot docs, so 500/page keeps round-trips low while staying
  // far under the recursion depth. Injectable so tests can force multiple pages with a
  // handful of rows. See [[KeysetScan]].
  findAllBatchSize:     Int            = 500,
  findAllBatchAttempts: Int            = 4,
  findAllBatchBackoff:  FiniteDuration = 500.millis
) extends ScreeningsRepository with Logging {
  import ScreeningsRepository.IdSep

  private lazy val coll: Option[MongoCollection[StoredScreeningsDto]] = sharedDb.map { db =>
    val c = db.withCodecRegistry(MovieCodecs.registry).getCollection[StoredScreeningsDto]("screenings")
      .withWriteConcern(WriteConcern.W1.withJournal(false))
    Try(Await.result(c.createIndex(Indexes.ascending("filmId")).toFuture(), 10.seconds))
    c
  }

  private def idOf(filmId: String, slotKey: String): String = s"$filmId$IdSep$slotKey"

  def findForFilm(filmId: String): Map[String, Seq[Showtime]] = coll.fold(Map.empty[String, Seq[Showtime]]) { c =>
    Try(Await.result(c.find(Filters.eq("filmId", filmId)).toFuture(), 30.seconds))
      .getOrElse(Seq.empty).map(d => d.slotKey -> d.showtimes).toMap
  }

  /** Every film's screenings, keyset-paged by `_id` (via [[KeysetScan]]) rather than pulled
   *  through ONE unbounded `find().toFuture()`. That single cursor over the whole
   *  `screenings` collection recursed the async Mongo driver into a `StackOverflowError`
   *  on a driver I/O thread once the collection grew (Sentry KINOWO-19) — and because it
   *  runs FIRST inside `MovieRepository.scanStitched`, that crash killed the worker's
   *  cold-cache rehydrate, so `findAll()` reported empty and the pages served no films.
   *  Paging caps how many rows any one cursor delivers synchronously. On an INCOMPLETE
   *  scan (a page still failing after retries) returns an empty map — `scanStitched`
   *  treats that as "incomplete" and won't let a reconcile prune on stripped rows. */
  def findAll(): Map[String, Map[String, Seq[Showtime]]] = coll match {
    case Some(c) =>
      val buf = Vector.newBuilder[StoredScreeningsDto]
      val complete = KeysetScan.scan[StoredScreeningsDto](
        label          = "ScreeningsRepository keyset batch",
        batchSize      = findAllBatchSize,
        maxAttempts    = findAllBatchAttempts,
        initialBackoff = findAllBatchBackoff,
        keyOf          = _._id,
        fetchPage      = (afterId, limit) => {
          val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
          Await.result(c.find(filter).sort(Sorts.ascending("_id")).limit(limit).toFuture(), 60.seconds)
        },
        onIncomplete   = exception =>
          logger.warn(s"ScreeningsRepository.findAll keyset scan failed after retries: " +
            s"${exception.getClass.getSimpleName}: ${exception.getMessage} — returning empty")
      )(batch => buf ++= batch)
      if (complete) buf.result().groupBy(_.filmId).view.mapValues(_.map(d => d.slotKey -> d.showtimes).toMap).toMap
      else Map.empty
    case None => Map.empty
  }

  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]]): Unit = coll.foreach { c =>
    Try {
      slots.foreach { case (k, st) => upsertOne(c, filmId, k, st) }
      (findForFilm(filmId).keySet -- slots.keySet).foreach(k => deleteOne(c, filmId, k))
    }.recover { case e => logger.warn(s"ScreeningsRepository.replaceFilm($filmId) failed: ${e.getMessage}") }
  }

  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit = coll.foreach { c =>
    Try(upsertOne(c, filmId, slotKey, showtimes))
      .recover { case e => logger.warn(s"ScreeningsRepository.upsertSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  def deleteSlot(filmId: String, slotKey: String): Unit = coll.foreach { c =>
    Try(deleteOne(c, filmId, slotKey))
      .recover { case e => logger.warn(s"ScreeningsRepository.deleteSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  def deleteFilm(filmId: String): Unit = coll.foreach { c =>
    Try(Await.result(c.deleteMany(Filters.eq("filmId", filmId)).toFuture(), 10.seconds))
      .recover { case e => logger.warn(s"ScreeningsRepository.deleteFilm($filmId) failed: ${e.getMessage}") }
  }

  private def upsertOne(c: MongoCollection[StoredScreeningsDto], filmId: String, slotKey: String, st: Seq[Showtime]): Unit = {
    val dto = StoredScreeningsDto(idOf(filmId, slotKey), filmId, slotKey, st, Instant.now())
    Await.result(c.replaceOne(Filters.eq("_id", dto._id), dto, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds); ()
  }

  private def deleteOne(c: MongoCollection[StoredScreeningsDto], filmId: String, slotKey: String): Unit = {
    Await.result(c.deleteOne(Filters.eq("_id", idOf(filmId, slotKey))).toFuture(), 10.seconds); ()
  }

  /** Watch the `screenings` collection; ring `onChange(filmId)` for every change
   *  (insert/update/replace carry the doc's `filmId`; a delete carries only the
   *  composite `_id`, from which the `filmId` prefix is parsed). The caller re-reads
   *  + stitches the film. Requires a replica set (like the movies stream). */
  override def watch(onChange: String => Unit): Option[AutoCloseable] = coll.map { c =>
    val subRef = new AtomicReference[Subscription]()
    c.watch().subscribe(new Observer[ChangeStreamDocument[StoredScreeningsDto]] {
      override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
      override def onNext(change: ChangeStreamDocument[StoredScreeningsDto]): Unit = {
        val filmId = Option(change.getFullDocument).map(_.filmId).orElse(
          Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
            .map(v => if (v.isString) v.asString.getValue else v.toString)
            .map(_.takeWhile(_ != IdSep))) // delete carries no post-image — split the _id
        filmId.foreach(fid => try onChange(fid)
          catch { case e: Throwable => logger.warn(s"screenings watch onChange($fid) failed: ${e.getMessage}") })
      }
      override def onError(e: Throwable): Unit =
        logger.warn(s"screenings change stream ended (${e.getMessage}).")
      override def onComplete(): Unit = ()
    })
    logger.info("MongoScreeningsRepository: watching screenings change stream.")
    new AutoCloseable { override def close(): Unit = Option(subRef.get()).foreach(_.unsubscribe()) }
  }
}
