package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import models.{Source, SourceData}
import org.mongodb.scala.model.{BulkWriteOptions, DeleteManyModel, Filters, Indexes, ReplaceOneModel, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
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
trait SlotsRepository extends SlotKeyedRows {

  /** Every slot of one film: `slotKey -> slot`. Empty when the film has none.
   *
   *  Conflates "no rows" with "the read failed" — use [[findForFilmChecked]] anywhere
   *  the difference decides what gets SERVED. */
  def findForFilm(filmId: String): Map[String, SourceData] = findForFilmChecked(filmId)._1

  /** One film's slots, PLUS whether the read actually succeeded — the per-film twin of
   *  [[findAllChecked]], and load-bearing for the same reason one level down.
   *
   *  The corpus scan learned this lesson first; the per-film path needed it just as
   *  much. Once a film's embedded copy is retired, `movies` carries no cinemas at all,
   *  so a failed slot read decodes to a film with NO cinemas — and the change-stream
   *  fan-out hands that straight to the read-model projector, whose `diffScreenings`
   *  deletes every `web_screening` the film has. A transient Mongo blip would empty a
   *  live film's showtimes off the site. Only the repository can tell "genuinely
   *  slot-less" from "could not read", so it says so instead of leaving the caller to
   *  infer it from the emptiness. A store that cannot fail always reports true. */
  def findForFilmChecked(filmId: String): (Map[String, SourceData], Boolean)

  /** Every film's slots: `filmId -> (slotKey -> slot)`. For the boot hydrate /
   *  `findAll` read-stitch. */
  def findAll(): Map[String, Map[String, SourceData]] = findAllChecked()._1

  /** The rows of SEVERAL films in ONE round-trip, plus whether the read succeeded.
   *
   *  The corpus scan used to preload this entire collection before it began paging
   *  `movies`, making the scan's peak heap the size of the collection (7.5 MB each for
   *  `screenings` and `movie_slots` on prod PL, more on UK) however small the page. Reading
   *  a page's films at a time keeps the scan bounded by the page — which is what
   *  `foreachRecord` promises its callers and had stopped being true when the side
   *  collections were split out. Default: one call per id, so a store with no batch read
   *  still satisfies the contract. */
  def findForFilmsChecked(filmIds: Set[String]): (Map[String, Map[String, SourceData]], Boolean) = {
    val results = filmIds.iterator.map(id => id -> findForFilmChecked(id)).toSeq
    (results.collect { case (id, (rows, _)) if rows.nonEmpty => id -> rows }.toMap,
     results.forall { case (_, (_, ok)) => ok })
  }

  /** Every film's slots, PLUS whether the scan actually completed.
   *
   *  The distinction is load-bearing once the embedded copy is retired. An empty map
   *  can mean "the collection is genuinely empty" (early in the lazy migration —
   *  harmless) or "the read failed" (every migrated film looks like it has no cinemas —
   *  catastrophic if a pruning caller believes it). Only the repository knows which, so
   *  it says so rather than making callers guess from the emptiness. */
  def findAllChecked(): (Map[String, Map[String, SourceData]], Boolean)

  /** Set a film's slots to EXACTLY `slots` — upsert those present, delete any no
   *  longer present. The whole-record write path.
   *
   *  Returns whether the write actually landed. The caller uses that to decide
   *  whether it may drop the embedded copy from the `movies` document: dropping it
   *  after a FAILED slot write would leave the film with no cinemas anywhere. A
   *  store that cannot fail (in-memory) always reports true. */
  def replaceFilm(filmId: String, slots: Map[String, SourceData],
                  stored: Option[Map[String, SourceData]] = None): Boolean

  /** Upsert one slot — the per-slot patch write path. */
  def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit

  /** Drop one slot (it left the film's listings). */
  def deleteSlot(filmId: String, slotKey: String): Unit

  /** Drop all of a film's slots (the film was deleted, or merged away). */
  def deleteFilm(filmId: String): Unit

  /** Push: ring `onChange(filmId)` whenever a film's slots actually change, so the
   *  change-stream fanout can re-read + re-dispatch that film. A no-op write (an
   *  identical slot) does NOT ring — the same contract as `ScreeningsRepository.watch`.
   *  Returns a handle to stop watching, or None when this impl can't push.
   *
   *  This collection deliberately had no cursor once, on the reasoning that every slot
   *  write rides a `movies` write and a second stream would fan one logical change out
   *  twice. Prod disproved the premise (2026-09-07): `MovieRepository.upsert` skips the
   *  `movies` write when the document is unchanged — and under the split the document
   *  carries neither slots nor showtimes, so it usually IS unchanged — while `movie_slots`
   *  still takes the row. A slot that lands after the film's last projection therefore had
   *  no event at all: 63 (film, venue) pairs in the UK and 33 in PL whose venue never
   *  reached the site. The double fan-out the old comment feared is what
   *  `MovieChangeStream` coalesces: events on one film share one queued re-read.
   *
   *  `demand` bounds how far the cursor may run ahead of the caller's apply — the caller
   *  owns it, because the caller is what decides when an event is APPLIED. Impls that ring
   *  listeners synchronously have no backlog and can ignore it. */
  def watch(onChange: String => Unit,
            demand:   ChangeStreamDemand = ChangeStreamDemand.unbounded): Option[AutoCloseable] = None

  def close(): Unit = ()
}

/**
 * In-memory `SlotsRepository` for tests and Mongo-less dev. Mirrors
 * [[MongoSlotsRepository]]'s semantics: idempotent per-slot writes, per-film grouping,
 * and a change ring that fires only on a real change — all of it [[InMemorySlotRows]],
 * the store its `screenings` twin is built on too. Neither store holds business logic —
 * both just store — so neither can drift from the other's understanding of the rules.
 */
class InMemorySlotsRepository extends SlotsRepository {

  private val rows = new InMemorySlotRows[SourceData]

  // An in-memory read cannot fail, so the checked form always reports complete.
  def findForFilmChecked(filmId: String): (Map[String, SourceData], Boolean) = (rows.forFilm(filmId), true)

  def findAllChecked(): (Map[String, Map[String, SourceData]], Boolean) = (rows.all(), true)   // an in-memory scan cannot fail

  // `stored` is ignored: this store's rows are already in memory, so re-reading them is
  // free and the parameter exists only to honour the trait (see the screenings twin).
  def replaceFilm(filmId: String, slots: Map[String, SourceData],
                  stored: Option[Map[String, SourceData]] = None): Boolean = {
    rows.replaceFilm(filmId, slots)
    true   // an in-memory store cannot fail to write
  }

  def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit = rows.upsert(filmId, slotKey, slot)

  def deleteSlot(filmId: String, slotKey: String): Unit = rows.delete(filmId, slotKey)

  def deleteFilm(filmId: String): Unit = rows.deleteFilm(filmId)

  def filmIdsChecked(): (Set[String], Boolean) = (rows.all().keySet, true)

  def rowIdsChecked(): (Set[String], Boolean) =
    (rows.all().iterator.flatMap { case (filmId, byKey) => byKey.keysIterator.map(SlotKeyed.idOf(filmId, _)) }.toSet, true)

  def deleteRows(ids: Set[String]): Long = {
    val present = ids.filter(id => rows.forFilm(SlotKeyed.filmIdOf(id)).contains(id.drop(SlotKeyed.filmIdOf(id).length + 1)))
    present.foreach(id => rows.delete(SlotKeyed.filmIdOf(id), id.drop(SlotKeyed.filmIdOf(id).length + 1)))
    present.size.toLong
  }

  def deleteFilms(filmIds: Set[String]): Long = {
    val removed = filmIds.toSeq.map(id => rows.forFilm(id).size.toLong).sum
    filmIds.foreach(rows.deleteFilm)
    removed
  }

  // Rings listeners synchronously, so there is no queue and nothing for `demand` to
  // bound — it is accepted only to honour the trait's contract.
  override def watch(onChange: String => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] =
    Some(rows.watch(onChange))
}

object SlotsRepository {
  /** The per-cinema slot collection — see [[services.DebugMirror]] for why the name
   *  is a constant rather than an inline literal. */
  val Collection = "movie_slots"

  /**
   * Write one film's slots the way `MovieRepository.upsert` must, and report whether they are now
   * known to be on disk. THE RULE LIVES HERE for [[ScreeningsSplit.applyFilm]]'s reason: both
   * the Mongo repository and the in-memory one call this rather than restating it, because the
   * in-memory one had already drifted into the unchecked read this exists to avoid.
   *
   * THE RETURN VALUE IS LOAD-BEARING and is why this is not fire-and-forget: `upsert` drops the
   * film's embedded slot map from `movies` only once the slots have landed, so answering `true`
   * for a write that failed loses the film's cinemas from BOTH places. A slots failure is
   * swallowed so it cannot break the movies write, which is exactly why the write has to report.
   *
   * The read is `findForFilmChecked`, never the plain one: a FAILED read returns an empty map,
   * and an empty map equals an empty payload — so the unchecked form answers "already landed" for
   * a film it could not read, on the one path where that answer deletes data.
   */
  def applyFilm(slots: SlotsRepository, filmId: String, payload: Map[String, SourceData]): Boolean = {
    val (current, readOk) = slots.findForFilmChecked(filmId)
    if (readOk && current == payload) true
    else slots.replaceFilm(filmId, payload, if (readOk) Some(current) else None)
  }

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

  /** A film's slots as a reader should see them mid-migration: the stored rows UNIONED
   *  with whatever the `movies` document still embeds, a stored row winning any key both
   *  carry.
   *
   *  Union, not "stored wins outright", because during the lazy migration NEITHER store
   *  is a complete view on its own and the two genuinely disagree. Measured on prod PL
   *  (2026-07-27): 81 of the 82 films holding both had diverging key sets, and 14 of them
   *  had a cinema in the embedded map that `movie_slots` did not carry — so a
   *  stored-shadows-embedded read served those 14 films with FEWER cinemas than the
   *  corpus actually held. The divergence is produced by writers that touch one store
   *  and not the other: `MongoStagingFolder` writes the embedded map in its transaction
   *  and no slot rows at all, while `updateIfPresent` writes per-slot deltas and
   *  deliberately leaves the embedded map alone.
   *
   *  Losing a cinema is the harmful direction; carrying a stale one for a scrape cycle is
   *  not, because the next whole-record `upsert` writes the full slot set and clears the
   *  embedded map — at which point this union collapses to exactly the stored rows. The
   *  rule therefore erases itself as the migration completes rather than needing a second
   *  cleanup to retire it. */
  def merge(embedded: Map[Source, SourceData], stored: Map[String, SourceData]): Map[Source, SourceData] =
    if (stored.isEmpty) embedded
    else if (embedded.isEmpty) stitch(stored)
    else stitch(embedded.map { case (s, sd) => s.displayName -> sd } ++ stored)

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
  // Persist THIS stream's resume token so a restart replays slot changes that landed while
  // down — like `screenings`, and for the same reason: a slot write need not touch `movies`.
  // ON only in the worker (the durable mirror); OFF for web /debug + scripts.
  persistResumeToken:   Boolean        = false,
  // What this store's cursor delivers, and what the apply coalesced away. The worker passes
  // the Prometheus sink; everything else keeps the no-op. See [[SideCollectionChangeMetrics]].
  metrics:              SideCollectionChangeMetrics = SideCollectionChangeMetrics.noop
) extends SlotsRepository with Logging {
  import SlotKeyed.idOf

  private lazy val coll: Option[MongoCollection[StoredSlotDto]] = sharedDb.map { db =>
    val c = db.withCodecRegistry(MovieCodecs.registry).getCollection[StoredSlotDto](SlotsRepository.Collection)
      .withWriteConcern(WriteConcern.W1.withJournal(false))
    Try(Await.result(c.createIndex(Indexes.ascending("filmId")).toFuture(), 10.seconds))
    c
  }

  private val resumeToken = new ChangeStreamResumeToken(SlotsRepository.Collection, sharedDb, persistResumeToken)

  /** A FAILED read reports `false` rather than passing an empty map off as "this film has
   *  no cinemas" — see the trait doc for what believing that emptiness costs. Logged too:
   *  the old silent `getOrElse(Seq.empty)` meant the one read whose failure can empty a
   *  live film left no trace at all. */
  def findForFilmChecked(filmId: String): (Map[String, SourceData], Boolean) =
    coll.fold((Map.empty[String, SourceData], true)) { c =>
      Try(Await.result(c.find(SlotKeyed.filmFilter(filmId)).toFuture(), 30.seconds)) match {
        case scala.util.Success(rows) => (rows.map(d => d.slotKey -> d.slot).toMap, true)
        case scala.util.Failure(e) =>
          logger.warn(s"SlotsRepository.findForFilm($filmId) failed: ${e.getClass.getSimpleName}: ${e.getMessage} " +
            "— reporting the read as incomplete so no caller serves the film as cinema-less.")
          (Map.empty, false)
      }
    }

  /** ONE `filmId $in [...]` query, served by the `filmId` index. */
  override def findForFilmsChecked(filmIds: Set[String]): (Map[String, Map[String, SourceData]], Boolean) =
    if (filmIds.isEmpty) (Map.empty, true)
    else coll.fold((Map.empty[String, Map[String, SourceData]], true)) { c =>
      Try(Await.result(c.find(Filters.in("filmId", filmIds.toSeq*)).toFuture(), 60.seconds)) match {
        case scala.util.Success(rows) =>
          (rows.groupBy(_.filmId).view.mapValues(_.map(d => d.slotKey -> d.slot).toMap).toMap, true)
        case scala.util.Failure(e) =>
          logger.warn(s"SlotsRepository.findForFilms(${filmIds.size} film(s)) failed: " +
            s"${e.getClass.getSimpleName}: ${e.getMessage} — reporting the read as incomplete.")
          (Map.empty, false)
      }
    }

  /** Every film's slots, keyset-paged by `_id` — see [[MongoScreeningsRepository.findAll]]
   *  for why a single unbounded cursor is not safe here. An INCOMPLETE scan returns an
   *  empty map so a caller can treat it as "unknown" rather than "the film has no slots"
   *  and prune on it. */
  def findAllChecked(): (Map[String, Map[String, SourceData]], Boolean) = coll match {
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
      if (complete) (buf.result().groupBy(_.filmId).view.mapValues(_.map(d => d.slotKey -> d.slot).toMap).toMap, true)
      else (Map.empty, false)
    // No collection wired at all — not a failure, there is simply nothing to read.
    case None => (Map.empty, true)
  }

  /** ONE ordered bulk round-trip: every slot's upsert plus a single `deleteMany` of
   *  whatever `slots` no longer names — the same shape as
   *  [[MongoScreeningsRepository.replaceFilm]], including the `$nin: []` edge case where
   *  an EMPTY `slots` clears every slot of the film. */
  def replaceFilm(filmId: String, slots: Map[String, SourceData],
                  stored: Option[Map[String, SourceData]] = None): Boolean = coll.fold(false) { c =>
    Try {
      val now     = Instant.now()
      // Rewrite only the rows that MOVED — the same guard `MongoScreeningsRepository.replaceFilm`
      // carries, and for the same reason one collection over: this method is film-wide while its
      // caller's change is one venue, so a wide release rewrote every row it had with nothing but
      // a fresh `updatedAt`. Each of those rewrites rings this collection's cursor and buys a
      // re-projection exactly as a screenings rewrite does — and these rows are whole `SourceData`
      // documents (title, synopsis, cast, poster), so it is MORE bytes for the same projection.
      //
      // The DELETE vector is unaffected: it is derived from `slots.keySet` (what the film should
      // end up with), never from the subset being written.
      // The caller's read when it has one — `MovieRepository.upsert` reads these rows to
      // decide whether to write at all, so making this read them again was a duplicated
      // full-film read on the hottest write path in the system.
      val (current, readComplete) = stored.map(_ -> true).getOrElse(findForFilmChecked(filmId))
      val upserts = SlotKeyed.changedRows(current, readComplete, slots).toSeq.map { case (k, sd) =>
        val dto = StoredSlotDto(idOf(filmId, k), filmId, k, sd, now)
        ReplaceOneModel(Filters.eq("_id", dto._id), dto, new ReplaceOptions().upsert(true))
      }
      val dropStale = DeleteManyModel[StoredSlotDto](SlotKeyed.staleSlotsFilter(filmId, slots.keySet))
      val result    = Await.result(c.bulkWrite(upserts :+ dropStale, new BulkWriteOptions().ordered(true)).toFuture(), 30.seconds)
      if (result.getDeletedCount > 0)
        RemovalAudit.screeningsCleared("movie_slots.replaceFilm", filmId, result.getDeletedCount.toInt,
          whole = slots.isEmpty, reason = "stale-slot-prune", what = "slots")
      true
    }.recover { case e =>
      logger.warn(s"SlotsRepository.replaceFilm($filmId) failed: ${e.getMessage}")
      false
    }.getOrElse(false)
  }

  def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit = coll.foreach { c =>
    // Same no-op guard as `replaceFilm`, at this method's granularity: a point read on the
    // composite `_id`. A row that already holds exactly what we would write must not be written.
    // A read that fails, and a row that is absent, both read as "differs" and write.
    Try {
      if (!storedSlot(c, filmId, slotKey).contains(slot)) {
        val dto = StoredSlotDto(idOf(filmId, slotKey), filmId, slotKey, slot, Instant.now())
        Await.result(c.replaceOne(Filters.eq("_id", dto._id), dto, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds); ()
      }
    }.recover { case e => logger.warn(s"SlotsRepository.upsertSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  /** One row's stored slot, or None when it is absent OR unreadable — the two cases `upsertSlot`
   *  treats alike, because both mean "we cannot say this write is redundant". */
  private def storedSlot(c: MongoCollection[StoredSlotDto], filmId: String, slotKey: String): Option[SourceData] =
    Try(Await.result(c.find(Filters.eq("_id", idOf(filmId, slotKey))).first().toFuture(), 10.seconds))
      .toOption.flatMap(Option(_)).map(_.slot)

  def deleteSlot(filmId: String, slotKey: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", idOf(filmId, slotKey))).toFuture(), 10.seconds)
      RemovalAudit.slotRemoved("movie_slots.deleteSlot", filmId, slotKey, "slot-deleted")
    }.recover { case e => logger.warn(s"SlotsRepository.deleteSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  def deleteFilm(filmId: String): Unit = coll.foreach { c =>
    Try {
      val deleted = Await.result(c.deleteMany(SlotKeyed.filmFilter(filmId)).toFuture(), 10.seconds).getDeletedCount
      if (deleted > 0)
        RemovalAudit.screeningsCleared("movie_slots.deleteFilm", filmId, deleted.toInt, whole = true,
          reason = "film-deleted", what = "slots")
    }.recover { case e => logger.warn(s"SlotsRepository.deleteFilm($filmId) failed: ${e.getMessage}") }
  }

  def filmIdsChecked(): (Set[String], Boolean) =
    coll.fold((Set.empty[String], true))(SlotKeyed.distinctFilmIdsChecked(_, "SlotsRepository", logger.warn(_)))

  def rowIdsChecked(): (Set[String], Boolean) =
    coll.fold((Set.empty[String], true))(SlotKeyed.rowIdsChecked(_, "SlotsRepository", logger.warn(_)))

  def deleteRows(ids: Set[String]): Long =
    coll.fold(0L)(SlotKeyed.deleteRows(_, ids, "SlotsRepository", logger.warn(_)))

  def deleteFilms(filmIds: Set[String]): Long =
    coll.fold(0L)(SlotKeyed.deleteFilms(_, filmIds, "SlotsRepository", logger.warn(_)))

  /** Watch `movie_slots`; ring `onChange(filmId)` for every change. The cursor itself is
   *  [[SideCollectionWatch]], shared with `screenings`, under this collection's own
   *  persisted resume token. */
  private lazy val changes: Option[SideCollectionWatch[StoredSlotDto]] =
    coll.map(c => new SideCollectionWatch(SlotsRepository.Collection, c, _.filmId, resumeToken, metrics))

  override def watch(onChange: String => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] =
    changes.map(_.watch(onChange, demand))

  override def close(): Unit = resumeToken.save(force = true)
}
