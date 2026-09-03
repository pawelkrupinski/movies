package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import com.mongodb.client.model.changestream.ChangeStreamDocument
import models.{Showtime, Source, SourceData}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.{BulkWriteOptions, DeleteManyModel, Filters, Indexes, ReplaceOneModel, Sorts}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, Observer, SingleObservableFuture, Subscription}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

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
   *  film has no recorded screenings — OR when the read failed, which callers that
   *  act destructively on the emptiness must not conflate. Use
   *  [[findForFilmChecked]] there. */
  def findForFilm(filmId: String): Map[String, Seq[Showtime]] = findForFilmChecked(filmId)._1

  /** [[findForFilm]] plus whether the read actually SAW the film's screenings.
   *  `(Map.empty, true)` is "this film has none"; `(Map.empty, false)` is "we could
   *  not tell". `reStitch` feeds the whole-record write path, whose `replaceFilm`
   *  DELETES every slot the record does not name — so a failed read that reads as
   *  "no showtimes" deletes them all. That is how the German corpus lost ~80% of its
   *  upcoming showtimes on 2026-07-27 while its film count and every film↔cinema slot
   *  row stayed intact. */
  def findForFilmChecked(filmId: String): (Map[String, Seq[Showtime]], Boolean)

  /** The rows of SEVERAL films in ONE round-trip, plus whether the read succeeded.
   *
   *  The corpus scan used to preload this entire collection before it began paging
   *  `movies`, making the scan's peak heap the size of the collection (7.5 MB each for
   *  `screenings` and `movie_slots` on prod PL, more on UK) however small the page. Reading
   *  a page's films at a time keeps the scan bounded by the page — which is what
   *  `foreachRecord` promises its callers and had stopped being true when the side
   *  collections were split out. Default: one call per id, so a store with no batch read
   *  still satisfies the contract. */
  def findForFilmsChecked(filmIds: Set[String]): (Map[String, Map[String, Seq[Showtime]]], Boolean) = {
    val results = filmIds.iterator.map(id => id -> findForFilmChecked(id)).toSeq
    (results.collect { case (id, (rows, _)) if rows.nonEmpty => id -> rows }.toMap,
     results.forall { case (_, (_, ok)) => ok })
  }

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

  def findForFilmChecked(filmId: String): (Map[String, Seq[Showtime]], Boolean) =
    (lock.synchronized(byFilm.getOrElse(filmId, Map.empty)), true)

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
  /** The showtimes collection — see [[services.DebugMirror]] for why the name is
   *  a constant rather than an inline literal. */
  val Collection = "screenings"

  /** The cinema slots that carry showtimes, keyed by slot wire-key
   *  (`Source.displayName`). Tmdb/Imdb slots never have showtimes, so they're
   *  excluded. Pure — the write paths derive their screenings docs from this. */
  def showtimesOf(data: Map[Source, SourceData]): Map[String, Seq[Showtime]] =
    data.iterator.collect { case (s, sd) if sd.showtimes.nonEmpty => s.displayName -> sd.showtimes }.toMap

  /** Re-inject any slots STRIPPED for the cache (empty showtimes + a digest) from a
   *  film's current screenings — the full-replace delete-vector defense. A whole-record
   *  write from the (stripped) cache would otherwise `showtimesOf`-drop those slots and
   *  `replaceFilm`-delete their screenings; re-stitching keeps them. Slots carrying real
   *  showtimes pass through unchanged. Pure + unit-tested. */
  def reStitch(screenings: ScreeningsRepository, id: String, data: Map[Source, SourceData]): Map[Source, SourceData] =
    reStitchChecked(screenings, id, data).data

  /**
   * A re-stitched record, beside the read that re-stitched it.
   *
   * `stored` is the film's screenings AS STORED — the read `data` was refilled from. It
   * is carried out rather than dropped because the caller's next act is to write those
   * same rows back, and comparing against what is already there is the difference between
   * rewriting every slot of the film and writing nothing. Free: the read has happened
   * either way. Meaningless when `complete` is false — a read that did not see the film
   * cannot say what it holds.
   */
  case class ReStitched(data: Map[Source, SourceData], stored: Map[String, Seq[Showtime]], complete: Boolean)

  /** [[reStitch]] plus whether the screenings read that fed it SAW the film. A
   *  `complete = false` means the stripped slots could not be refilled, so the result
   *  under-reports the film's showtimes and MUST NOT be handed to a full `replaceFilm` —
   *  its delete vector would erase every slot the read failed to return. */
  def reStitchChecked(screenings: ScreeningsRepository, id: String,
                      data: Map[Source, SourceData]): ReStitched = {
    val (scr, complete) = screenings.findForFilmChecked(id)
    val stitched = data.map {
      case (src, sd) if sd.showtimes.isEmpty && sd.showtimesDigest.isDefined =>
        src -> sd.copy(showtimes = scr.getOrElse(src.displayName, Seq.empty))
      case other => other
    }
    ReStitched(stitched, scr, complete)
  }

  /** The per-slot screening writes needed to turn `before`'s showtimes into
   *  `after`'s: `slotKey -> Some(showtimes)` to upsert, `slotKey -> None` to
   *  delete. Only slots whose showtimes actually changed appear — so a
   *  metadata-only change yields an empty map (no screening write) and a
   *  showtimes-only change writes ONLY here. Pure + unit-tested. */
  def slotOps(before: Map[Source, SourceData], after: Map[Source, SourceData]): Map[String, Option[Seq[Showtime]]] =
    (before.keySet ++ after.keySet).iterator.flatMap { s =>
      val bDigest = before.get(s).map(ShowtimesDigest.slotDigest).getOrElse(ShowtimesDigest.EmptyDigest)
      val aDigest = after.get(s).map(ShowtimesDigest.slotDigest).getOrElse(ShowtimesDigest.EmptyDigest)
      if (aDigest == bDigest) None
      else {
        val a = after.get(s).map(_.showtimes).getOrElse(Seq.empty)
        if (a.nonEmpty) Some(s.displayName -> Some(a))
        // An empty LIST is not evidence of an empty slot. A record resident in the cache has
        // been through `stripForCache`, which drops every list and keeps only the digest — so
        // "stripped" and "screening nothing" look identical here, and `putIfPresent` (every
        // rating refresh, every per-slot scrape update) hands us exactly those stripped
        // records. Deleting on that basis wipes the film's screenings for a cinema from a
        // record that never carried them.
        //
        // The digest is what tells the two apart. Delete only when it says the slot really is
        // empty — which also covers the slot vanishing from `after` entirely, a genuine
        // removal that must still delete. Otherwise we know the showtimes CHANGED but not
        // what to, so there is nothing this record can write: skip, and let the whole-record
        // path (`upsert`, which re-stitches from `screenings` first) carry the change.
        else if (aDigest == ShowtimesDigest.EmptyDigest) Some(s.displayName -> None)
        else None
      }
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

  /** The stored slots of `filmId` that `keep` no longer names — the DELETE half of
   *  `replaceFilm`, as ONE server-side predicate instead of a `findForFilm` read plus a
   *  `deleteOne` per stale slot.
   *
   *  The same document set as the old loop: its candidates were exactly
   *  `find(filmId == filmId)`, and for each it deleted `_id = filmId + IdSep + slotKey`,
   *  which by the write invariant IS that document. Keying on the `filmId` + `slotKey`
   *  FIELDS reproduces that set without re-deriving the composite `_id`, so it stays
   *  unambiguous even for a `filmId` that itself contains [[IdSep]].
   *
   *  `keep` EMPTY yields `$nin: []` — nothing is a member of the empty set, so it matches
   *  EVERY slot of the film. An empty `slots` map therefore still clears the film exactly
   *  as the old "delete every key the read returned" did. This predicate is the only thing
   *  standing between a whole-record write and a film's screenings, so it is unit-tested
   *  directly. */
  private[movies] def staleSlotsFilter(filmId: String, keep: Set[String]): Bson =
    SlotKeyed.staleSlotsFilter(filmId, keep)

  // Non-printable separator so the composite `_id` never collides with a slot key.
  private[movies] val IdSep: Char = SlotKeyed.IdSep
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
  findAllBatchBackoff:  FiniteDuration = 500.millis,
  // Persist THIS stream's resume token so a restart replays showtime changes that landed
  // while down — the `movies` stream can't, a showtime write never touches `movies`. ON only
  // in the worker (the durable mirror); OFF for web /debug + scripts. See [[ChangeStreamResumeToken]].
  persistResumeToken:   Boolean        = false
) extends ScreeningsRepository with Logging {
  import ScreeningsRepository.IdSep

  private lazy val coll: Option[MongoCollection[StoredScreeningsDto]] = sharedDb.map { db =>
    val c = db.withCodecRegistry(MovieCodecs.registry).getCollection[StoredScreeningsDto](ScreeningsRepository.Collection)
      .withWriteConcern(WriteConcern.W1.withJournal(false))
    Try(Await.result(c.createIndex(Indexes.ascending("filmId")).toFuture(), 10.seconds))
    c
  }

  private val resumeToken = new ChangeStreamResumeToken("screenings", sharedDb, persistResumeToken)

  private def idOf(filmId: String, slotKey: String): String = s"$filmId$IdSep$slotKey"

  // No collection wired at all reports COMPLETE, not failed — there is simply nothing to
  // read, which is not the same as having failed to read it. Matches
  // `MongoSlotsRepository.findForFilmChecked`, its sibling under `SlotKeyed`; the two
  // answered opposite things for the same state, and a caller that treats "unreadable" as
  // "defer" would have deferred forever against a Mongo-less stack.
  def findForFilmChecked(filmId: String): (Map[String, Seq[Showtime]], Boolean) =
    coll.fold((Map.empty[String, Seq[Showtime]], true)) { c =>
      Try(Await.result(c.find(Filters.eq("filmId", filmId)).toFuture(), 30.seconds)) match {
        case Success(docs) => (docs.map(d => d.slotKey -> d.showtimes).toMap, true)
        case Failure(exception) =>
          logger.warn(s"ScreeningsRepository.findForFilm($filmId) failed: ${exception.getMessage}")
          (Map.empty, false)
      }
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
  /** ONE `filmId $in [...]` query, served by the `filmId` index. */
  override def findForFilmsChecked(filmIds: Set[String]): (Map[String, Map[String, Seq[Showtime]]], Boolean) =
    if (filmIds.isEmpty) (Map.empty, true)
    else coll.fold((Map.empty[String, Map[String, Seq[Showtime]]], true)) { c =>
      Try(Await.result(c.find(Filters.in("filmId", filmIds.toSeq*)).toFuture(), 60.seconds)) match {
        case scala.util.Success(rows) =>
          (rows.groupBy(_.filmId).view.mapValues(_.map(d => d.slotKey -> d.showtimes).toMap).toMap, true)
        case scala.util.Failure(e) =>
          logger.warn(s"ScreeningsRepository.findForFilms(${filmIds.size} film(s)) failed: " +
            s"${e.getClass.getSimpleName}: ${e.getMessage} — reporting the read as incomplete.")
          (Map.empty, false)
      }
    }

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

  /** ONE bulk round-trip: every slot's upsert plus a single `deleteMany` of whatever
   *  `slots` no longer names. This used to be a blocking `replaceOne` per slot, then a
   *  `findForFilm` read, then a blocking `deleteOne` per stale slot — 12 sequential
   *  round-trips for a film showing in 10 cinemas, paid on EVERY `MovieRepository.upsert`.
   *
   *  Semantics are unchanged, edge cases included:
   *   - empty `slots` → no upserts, and [[ScreeningsRepository.staleSlotsFilter]]'s
   *     `$nin: []` still deletes every one of the film's slots;
   *   - a slot mapped to EMPTY showtimes is still STORED (callers filter those out via
   *     `showtimesOf`; `replaceFilm` itself never did), not treated as a delete;
   *   - an unchanged slot is still rewritten — idempotent, as before.
   *
   *  ORDERED, so the upserts land before the delete exactly as they did and the delete
   *  can never race ahead of a slot this same call is re-writing. The request list is
   *  never empty (the delete is always present), so the driver's empty-`bulkWrite`
   *  rejection is unreachable. */
  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]]): Unit = coll.foreach { c =>
    Try {
      val now     = Instant.now()
      val upserts = slots.toSeq.map { case (k, st) =>
        val dto = StoredScreeningsDto(idOf(filmId, k), filmId, k, st, now)
        ReplaceOneModel(Filters.eq("_id", dto._id), dto, new ReplaceOptions().upsert(true))
      }
      val dropStale = DeleteManyModel[StoredScreeningsDto](ScreeningsRepository.staleSlotsFilter(filmId, slots.keySet))
      val result    = Await.result(c.bulkWrite(upserts :+ dropStale, new BulkWriteOptions().ordered(true)).toFuture(), 30.seconds)
      if (result.getDeletedCount > 0)
        RemovalAudit.screeningsCleared("screenings.replaceFilm", filmId, result.getDeletedCount.toInt,
          whole = slots.isEmpty, reason = "stale-slot-prune")
    }.recover { case e => logger.warn(s"ScreeningsRepository.replaceFilm($filmId) failed: ${e.getMessage}") }
  }

  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit = coll.foreach { c =>
    Try(upsertOne(c, filmId, slotKey, showtimes))
      .recover { case e => logger.warn(s"ScreeningsRepository.upsertSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  def deleteSlot(filmId: String, slotKey: String): Unit = coll.foreach { c =>
    Try { deleteOne(c, filmId, slotKey); RemovalAudit.slotRemoved("screenings.deleteSlot", filmId, slotKey, "slot-deleted") }
      .recover { case e => logger.warn(s"ScreeningsRepository.deleteSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  def deleteFilm(filmId: String): Unit = coll.foreach { c =>
    Try {
      val deleted = Await.result(c.deleteMany(Filters.eq("filmId", filmId)).toFuture(), 10.seconds).getDeletedCount
      if (deleted > 0)
        RemovalAudit.screeningsCleared("screenings.deleteFilm", filmId, deleted.toInt, whole = true, reason = "film-deleted")
    }.recover { case e => logger.warn(s"ScreeningsRepository.deleteFilm($filmId) failed: ${e.getMessage}") }
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
    // A terminal error is the END of a cursor — the driver never brings it back, and unlike
    // the movies stream there is not even a later registration to re-open this one. Without
    // this driver a single blip left every showtime change unseen until the process restarted.
    def open(): Unit = {
      // Resume from the last persisted token (a restart / prior terminal error) so showtime
      // changes that landed while down are replayed; else open at "now".
      val resumeFrom = resumeToken.load()
      val base       = c.watch()
      resumeFrom.fold(base)(t => base.resumeAfter(Document(t)))
        .subscribe(new Observer[ChangeStreamDocument[StoredScreeningsDto]] {
          override def onSubscribe(s: Subscription): Unit = { subRef.set(s); s.request(Long.MaxValue) }
          override def onNext(change: ChangeStreamDocument[StoredScreeningsDto]): Unit = {
            reopen.opened() // a delivered event is what proves the cursor healthy — reset the backoff
            // Advance the resume position BEFORE ringing onChange, so a re-stitch can never
            // observe the change before the token moves past it.
            resumeToken.advance(change.getResumeToken)
            val filmId = Option(change.getFullDocument).map(_.filmId).orElse(
              Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
                .map(v => if (v.isString) v.asString.getValue else v.toString)
                .map(_.takeWhile(_ != IdSep))) // delete carries no post-image — split the _id
            filmId.foreach(fid => try onChange(fid)
              catch { case e: Throwable => logger.warn(s"screenings watch onChange($fid) failed: ${e.getMessage}") })
            resumeToken.save(force = false) // time-throttled, fire-and-forget
          }
          override def onError(e: Throwable): Unit = {
            if (ChangeStreamResumeToken.isInvalid(e)) {
              logger.warn(s"screenings change stream: resume token invalid (${e.getMessage}) — clearing it; " +
                "the next open starts fresh and the backstop resyncs the gap.")
              resumeToken.clear()
            } else
              logger.warn(s"screenings change stream ended (${e.getMessage}) — a reopen resumes from the " +
                "persisted token; the backstop covers the meantime.")
            subRef.set(null)
            reopen.failed()
          }
          override def onComplete(): Unit = { subRef.set(null); reopen.failed() }
        })
      logger.info(s"MongoScreeningsRepository: watching screenings change stream" +
        s"${if (resumeFrom.isDefined) ", resumed from persisted token" else ""}.")
    }
    lazy val reopen: ChangeStreamReopen = ChangeStreamReopen.onDaemonScheduler("screenings", () => open())
    open()
    new AutoCloseable { override def close(): Unit = {
      reopen.close()
      resumeToken.save(force = true) // final position synchronously so the next process resumes here
      Option(subRef.get()).foreach(_.unsubscribe())
    } }
  }

  override def close(): Unit = resumeToken.save(force = true)
}
