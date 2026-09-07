package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import models.Showtime
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.{BulkWriteOptions, DeleteManyModel, Filters, Indexes, ReplaceOneModel, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
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
trait ScreeningsRepository extends SlotKeyedRows {

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
   *  no longer present. The whole-record write path (`MovieRepository.upsert`).
   *
   *  `stored` is the film's rows AS THEY ARE NOW, when the caller has already read them.
   *  The write only rewrites the rows that actually moved, so it needs that map — and
   *  `MovieRepository.upsert` has it in hand from the re-stitch, on the hottest write path
   *  in the system. `None` means "read it yourself"; passing a map from a read that FAILED
   *  would have every row look new and be trusted, so a failed read must pass `None`. */
  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]],
                  stored: Option[Map[String, Seq[Showtime]]] = None): Unit

  /** Upsert one slot's showtimes — the per-slot patch write path
   *  (`MovieRepository.updateIfPresent`). */
  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit

  /** Drop one slot's screenings (the slot left the film's listings). */
  def deleteSlot(filmId: String, slotKey: String): Unit

  /** Drop all of a film's screenings (the film was deleted, or merged away). */
  def deleteFilm(filmId: String): Unit

  /** Push: ring `onChange(filmId)` whenever a film's screenings actually change, so
   *  the change-stream fanout can re-stitch + re-dispatch that film. A no-op write
   *  (unchanged showtimes) does NOT ring — mirroring the `movies` no-op guard.
   *  Returns a handle to stop watching, or None when this impl can't push.
   *
   *  `demand` bounds how far the cursor may run ahead of the caller's apply — the
   *  caller owns it, because the caller is what decides when an event is APPLIED (it
   *  hands the work to a queue rather than doing it in `onChange`). Impls that ring
   *  listeners synchronously have no backlog and can ignore it. */
  def watch(onChange: String => Unit,
            demand:   ChangeStreamDemand = ChangeStreamDemand.unbounded): Option[AutoCloseable] = None

  def close(): Unit = ()
}

/**
 * In-memory `ScreeningsRepository` for tests and Mongo-less dev. Mirrors
 * [[MongoScreeningsRepository]]'s semantics: idempotent per-slot writes, per-film
 * grouping, and a change ring that fires only on a real change — all of it
 * [[InMemorySlotRows]], the store its `movie_slots` twin is built on too.
 */
class InMemoryScreeningsRepository extends ScreeningsRepository {

  private val rows = new InMemorySlotRows[Seq[Showtime]]

  def findForFilmChecked(filmId: String): (Map[String, Seq[Showtime]], Boolean) = (rows.forFilm(filmId), true)

  def findAll(): Map[String, Map[String, Seq[Showtime]]] = rows.all()

  // `stored` is ignored: this store's rows are already in memory, so re-reading them is free
  // and the parameter exists only to honour the trait.
  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]],
                  stored: Option[Map[String, Seq[Showtime]]] = None): Unit = rows.replaceFilm(filmId, slots)

  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit = rows.upsert(filmId, slotKey, showtimes)

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

object ScreeningsRepository {
  /** The showtimes collection — see [[services.DebugMirror]] for why the name is
   *  a constant rather than an inline literal. */
  val Collection = "screenings"

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
  persistResumeToken:   Boolean        = false,
  // What this store streams. The worker passes the Prometheus sink; everything else keeps
  // the no-op. This cursor is the read-model projection's larger trigger and had no metric
  // at all, which is what made a 40x projection climb unattributable — see [[ScreeningsMetrics]].
  metrics:              ScreeningsMetrics = ScreeningsMetrics.noop
) extends ScreeningsRepository with Logging {
  import ScreeningsRepository.IdSep

  private lazy val coll: Option[MongoCollection[StoredScreeningsDto]] = sharedDb.map { db =>
    val c = db.withCodecRegistry(MovieCodecs.registry).getCollection[StoredScreeningsDto](ScreeningsRepository.Collection)
      .withWriteConcern(WriteConcern.W1.withJournal(false))
    Try(Await.result(c.createIndex(Indexes.ascending("filmId")).toFuture(), 10.seconds))
    c
  }

  private val resumeToken = new ChangeStreamResumeToken(ScreeningsRepository.Collection, sharedDb, persistResumeToken)

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
   *   - an unchanged slot is NO LONGER rewritten. That is the one semantic that changed, and
   *     it changed because the rewrite was never idempotent where it counted: the row lands
   *     in the oplog with a fresh `updatedAt`, rings the change stream, and buys a projection.
   *
   *  ORDERED, so the upserts land before the delete exactly as they did and the delete
   *  can never race ahead of a slot this same call is re-writing. The request list is
   *  never empty (the delete is always present), so the driver's empty-`bulkWrite`
   *  rejection is unreachable. */
  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]],
                  stored: Option[Map[String, Seq[Showtime]]] = None): Unit = coll.foreach { c =>
    Try {
      val now     = Instant.now()
      // Rewrite only the rows that MOVED. One indexed read on `filmId` — the same read the
      // delete half used to make before it became a server-side predicate — buys the whole
      // saving, because the redundant rows are not merely wasted writes: each one rings this
      // collection's change stream and costs a projection of the film it belongs to. See
      // `ScreeningsSplit.changedSlots` for the measurement.
      //
      // The DELETE vector is unaffected: it is derived from `slots.keySet` (what the film
      // should end up with), never from the subset being written, so a row that is correct and
      // therefore skipped is still a row this call keeps.
      val (current, readComplete) = stored.map(_ -> true).getOrElse(findForFilmChecked(filmId))
      val changed = ScreeningsSplit.changedSlots(current, readComplete, slots)
      // The SKIP is counted here and the WRITE is counted after the bulkWrite returns, which is
      // not fussiness: this whole `Try` swallows its failure into a `logger.warn`, so counting
      // `written` up front meant a 30-second bulkWrite timeout or a stepdown incremented it for
      // rows that never landed. The documented canary for a broken guard is "`written` climbing
      // under a flat scrape rate" -- exactly the shape a Mongo incident would have forged.
      metrics.recordWrite(ScreeningsMetrics.Outcome.Unchanged, slots.size - changed.size)
      val upserts = changed.toSeq.map { case (k, st) =>
        val dto = StoredScreeningsDto(idOf(filmId, k), filmId, k, st, now)
        ReplaceOneModel(Filters.eq("_id", dto._id), dto, new ReplaceOptions().upsert(true))
      }
      val dropStale = DeleteManyModel[StoredScreeningsDto](ScreeningsRepository.staleSlotsFilter(filmId, slots.keySet))
      val result    = Await.result(c.bulkWrite(upserts :+ dropStale, new BulkWriteOptions().ordered(true)).toFuture(), 30.seconds)
      metrics.recordWrite(ScreeningsMetrics.Outcome.Written, changed.size)
      if (result.getDeletedCount > 0)
        RemovalAudit.screeningsCleared("screenings.replaceFilm", filmId, result.getDeletedCount.toInt,
          whole = slots.isEmpty, reason = "stale-slot-prune")
    }.recover { case e => logger.warn(s"ScreeningsRepository.replaceFilm($filmId) failed: ${e.getMessage}") }
  }

  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit = coll.foreach { c =>
    // Same no-op guard as `replaceFilm`, at the granularity this method works in: a point read
    // on the composite `_id`. A row that is already what we would write must not be written,
    // because the write is what rings the change stream — the trait's contract has always said
    // so and `InMemoryScreeningsRepository` has always honoured it; only this side did not.
    // A read that fails, and a row that is absent, both read as "differs" and write.
    Try {
      if (storedShowtimes(c, filmId, slotKey).contains(showtimes))
        metrics.recordWrite(ScreeningsMetrics.Outcome.Unchanged, 1)
      else {
        upsertOne(c, filmId, slotKey, showtimes)
        metrics.recordWrite(ScreeningsMetrics.Outcome.Written, 1)
      }
    }.recover { case e => logger.warn(s"ScreeningsRepository.upsertSlot($filmId,$slotKey) failed: ${e.getMessage}") }
  }

  /** One row's stored showtimes, or None when it is absent OR unreadable — the two cases
   *  `upsertSlot` treats alike, because both mean "we cannot say this write is redundant". */
  private def storedShowtimes(c: MongoCollection[StoredScreeningsDto],
                              filmId: String, slotKey: String): Option[Seq[Showtime]] =
    Try(Await.result(c.find(Filters.eq("_id", idOf(filmId, slotKey))).first().toFuture(), 10.seconds))
      .toOption.flatMap(Option(_)).map(_.showtimes)

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

  def filmIdsChecked(): (Set[String], Boolean) =
    coll.fold((Set.empty[String], true))(SlotKeyed.distinctFilmIdsChecked(_, "ScreeningsRepository", logger.warn(_)))

  def rowIdsChecked(): (Set[String], Boolean) =
    coll.fold((Set.empty[String], true))(SlotKeyed.rowIdsChecked(_, "ScreeningsRepository", logger.warn(_)))

  def deleteRows(ids: Set[String]): Long =
    coll.fold(0L)(SlotKeyed.deleteRows(_, ids, "ScreeningsRepository", logger.warn(_)))

  def deleteFilms(filmIds: Set[String]): Long =
    coll.fold(0L)(SlotKeyed.deleteFilms(_, filmIds, "ScreeningsRepository", logger.warn(_)))

  private def upsertOne(c: MongoCollection[StoredScreeningsDto], filmId: String, slotKey: String, st: Seq[Showtime]): Unit = {
    val dto = StoredScreeningsDto(idOf(filmId, slotKey), filmId, slotKey, st, Instant.now())
    Await.result(c.replaceOne(Filters.eq("_id", dto._id), dto, new ReplaceOptions().upsert(true)).toFuture(), 10.seconds); ()
  }

  private def deleteOne(c: MongoCollection[StoredScreeningsDto], filmId: String, slotKey: String): Unit = {
    Await.result(c.deleteOne(Filters.eq("_id", idOf(filmId, slotKey))).toFuture(), 10.seconds); ()
  }

  /** Watch the `screenings` collection; ring `onChange(filmId)` for every change. The
   *  caller re-reads + stitches the film. The cursor itself — resume token, demand,
   *  reopen, metrics, the delete's `_id` parse — is [[SideCollectionWatch]], shared with
   *  `movie_slots`. */
  private lazy val changes: Option[SideCollectionWatch[StoredScreeningsDto]] =
    coll.map(c => new SideCollectionWatch(ScreeningsRepository.Collection, c, _.filmId, resumeToken, metrics))

  override def watch(onChange: String => Unit, demand: ChangeStreamDemand): Option[AutoCloseable] =
    changes.map(_.watch(onChange, demand))

  override def close(): Unit = resumeToken.save(force = true)
}
