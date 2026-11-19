package services.movies

import models.{CacheKey, FilmId, StoredMovieRecord}

import services.titles.TitleNormalizer

import com.mongodb.WriteConcern
import models.MovieRecord
import org.mongodb.scala.bson.{BsonDateTime, BsonDocument}
import org.mongodb.scala.model.{Aggregates, Filters, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.bson.conversions.Bson
import play.api.Logging

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

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
  // The database, handed in by the composition root (a `MongoConnection`'s, whose
  // client this class borrows and never closes). `None` (no Mongo configured, or the
  // connection failed) leaves the store disabled. It never opens a client of its own.
  sharedDb: Option[MongoDatabase],
  // The composition root's clock. Every `updatedAt` is a stamp from one strictly increasing
  // sequence over it ([[stamps]]), never a bare reading.
  private[movies] val clock: java.time.Clock,
  // The composition root's monotonic clock: the corpus scan's timings (and, mixed in, the change stream's
  // resume-token throttle and decoder log — [[MongoMovieChangeFeed]]).
  private[movies] val stopwatch: tools.Stopwatch,
  // Cursor page size for the keyset-paged corpus scan shared by `findAll` and
  // `foreachRecord` — the cap on how many rows any ONE cursor delivers before the
  // next `_id`-keyset page. Bounds the async driver's synchronous read-completion
  // depth so a full-corpus read can't StackOverflow (a single unbounded cursor did —
  // see `findAll`), and for `foreachRecord` also caps heap: 200 rows × ~13 KB avg ≈
  // a few hundred KB per batch (vs ~13 MB for the whole corpus). Injectable so tests
  // can force multiple pages with a handful of rows.
  findAllBatchSize: Int = 200,
  // Per-batch retry budget for the keyset corpus scan (`findAll` + `foreachRecord`).
  // A batch read that fails transiently (a server-selection / socket timeout while the
  // worker is CPU-throttled) is retried with 0.5s → 1s → 2s backoff before the whole
  // scan is declared incomplete. Injectable so a test can force the exhausted path fast.
  foreachRecordBatchAttempts: Int            = 4,
  foreachRecordBatchBackoff:  FiniteDuration = 500.millis,
  // Showtimes live in the separate `screenings` collection, not embedded in the
  // `movies` document — so a showtime change no longer rewrites the (formerly
  // 1-2MB) film doc the change stream re-decodes on every write. Wiring `screenings`
  // turns the split ON: `movies` is written WITHOUT showtimes, reads stitch them
  // back from `screenings`, and a `screenings` change is fanned out as a stitched
  // record. The stitch is authoritative — a slot with no `screenings` doc has no
  // showtimes (the one-time embedded→screenings migration is complete, so `movies`
  // carries no showtimes). Maintenance scripts/tests that pass `None` keep the plain
  // embedded shape (they don't serve).
  screenings: Option[ScreeningsRepository] = None,
  // The per-cinema SourceData slots, split out of `movies.sourceData` into
  // `movie_slots` — the same move as `screenings`, one level further, so a change
  // event carries ONE slot instead of the whole film document (see [[SlotsRepository]]
  // for the measurement that motivated it).
  //
  // Wiring this turns on DUAL WRITE only: `movies` still carries the embedded
  // `sourceData` map and reads still come from it, so this is behaviour-preserving
  // and reversible — unwire it and nothing notices. Flipping reads to `movie_slots`
  // is a later phase, and must not happen before a backfill has populated the
  // collection for films whose slots haven't been rewritten since the split landed.
  // Scripts/tests that pass `None` write nothing extra.
  slots: Option[SlotsRepository] = None,
  // The country whose rules derive a row's `_id` (`sanitize(title)|year`). The
  // document id IS the row's identity, so a repository writing under another
  // country's rules would split or collide rows. REQUIRED, not defaulted: this is
  // a production persistence path, and a silent fallback here is the 2026 incident.
  override val normalizer: TitleNormalizer,
  // Where a write that THREW is counted — see [[WriteOutcome]]. Noop for scripts/web/tests.
  writeMetrics: RepositoryWriteMetrics = RepositoryWriteMetrics.noop,
  // Where a `movies` document the codec could not decode is counted — the same counter as the
  // read model's skipped documents. Noop for scripts/web/tests.
  private[movies] val decodeFailures: services.movies.DecodeFailureMetrics = services.movies.DecodeFailureMetrics.noop
) extends MovieRepository with KeyAddressedMovieWrites with Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)
  /** Every `updatedAt` this store writes: strictly increasing within the process, so the stamp
   *  is a version the dotted-replace guard can trust (two writes in one millisecond no longer
   *  share one) and a cursor the catch-up can trust (its floor is a stamp from this same
   *  sequence — see [[ChangeStreamLiveness.now]]). */
  private[movies] val stamps = new tools.MonotonicStampSequence(clock)


  override def hasScreenings: Boolean = screenings.isDefined
  override def hasSlots:       Boolean = slots.isDefined

  // How a stored row is made whole again from `movie_slots` and `screenings`.
  private val stitch = new MovieSideStitch(screenings, slots, normalizer, logger)
  // How every corpus scan pages the `movies` collection by `_id`.
  private val pages = new MoviesKeysetPages(findAllBatchSize, foreachRecordBatchAttempts, foreachRecordBatchBackoff, decodeFailures)
  // How every corpus scan reads `movies` a page at a time and stitches each page whole.
  private val scan = new StitchedCorpusScan(coll, pages, stitch, normalizer, stopwatch, logger)
  // How a whole record is written over `movies` and its side collections; none without a store.
  private lazy val upserts = coll.map(new WholeRecordUpsert(_, screenings, slots, slotsForStorage, stamps, writeMetrics, logger))
  // How a stored row is updated field by field, its dotted-name fallback reading through this class's seam; none without a store.
  private lazy val updates = coll.map(c => new InPlaceMovieUpdate(c, screenings, slots, slotsForStorage, normalizer, stamps, writeMetrics,
    logger, readWhole = dottedReplaceRead(c, _)))

  /** The film's slots at `cinemas`, read from those venues alone — see [[MovieSideStitch.readVenues]]. */
  private[movies] def readVenues(filmId: String, cinemas: Set[models.Cinema]): Option[VenueSlots] = stitch.readVenues(filmId, cinemas)

  // Lazy so constructing the store touches no server: the index builds run on
  // first use. We apply our own codec registry to the handed-in database (a
  // view, not a clone — the underlying client is the `MongoConnection`'s) and
  // grab our collection from it.
  private lazy val coll: Option[MongoCollection[StoredMovieDto]] =
    sharedDb match {
      case Some(db) =>
        val withRegistry = db.withCodecRegistry(MovieCodecs.registry)
        // Relaxed write concern (w:1, j:false): `movies` is re-scraped continuously,
        // so a write lost to a crash is recovered by the next scrape pass. Skipping
        // the journal sync cuts per-write cost on the shared-CPU Mongo — the worker's
        // write rate is what throttles it. Same trade `MongoTaskQueue` already makes.
        val coll = withRegistry.getCollection[StoredMovieDto](MovieRepository.Collection)
          .withWriteConcern(WriteConcern.W1.withJournal(false))
        MovieIndexes.ensure(withRegistry, coll)
        Some(coll)
      case None => None
    }

  def enabled: Boolean = coll.isDefined

  /** Test seam: the write concern configured on the `movies` collection. */
  def collectionWriteConcern: Option[WriteConcern] = coll.map(_.writeConcern)

  /** Boot-time + periodic full reload of every persisted row. Pages the cursor by
   *  `_id` (keyset, via [[StitchedCorpusScan.scanByKeyset]]) and collects the batches, rather than pulling
   *  the whole corpus through ONE unbounded `find().toFuture()`.
   *
   *  Why paged, not one cursor: a single unbounded find over the whole (~13 MB,
   *  ~1400-row) corpus recursed the async Mongo driver's per-message read-completion
   *  chain (`AsyncSupplier.finish` → `AsyncCompletionHandler` → `SingleResultCallback`)
   *  deep enough to throw `StackOverflowError` on a driver I/O thread once the corpus
   *  grew past a threshold (Sentry KINOWO-19, 2026-07-02). Because the crash lands on
   *  an uncaught I/O thread — NOT on the `Await` here — it isn't caught by any
   *  `Try.recover`; it killed the worker's cold-cache rehydrate and left it in a boot
   *  crash-loop that never warmed the cache. Keyset paging caps how many rows any ONE
   *  cursor delivers synchronously, so the completion chain stays shallow. The result
   *  is still the full corpus on the heap (findAll's contract), just read in
   *  `findAllBatchSize`-row bites.
   *
   *  `StitchedCorpusScan.scanByKeyset` sorts each page by the unique, immutable `_id` index, so — exactly
   *  as the old single sorted cursor did — the scan returns each document once (no
   *  duplicate at a page boundary, no skipped row) even under concurrent writes.
   *
   *  On an INCOMPLETE scan (a batch still failing after its retries) returns
   *  `Seq.empty` — findAll's historical failure contract: `MovieCache.rehydrate` treats
   *  an empty result as "transient Mongo failure, keep the current cache" rather than
   *  acting on a partial corpus. The 60s per-batch timeout (vs the 10s on point writes)
   *  still covers a cold WiredTiger first read after a process boot (10–20 s even when
   *  steady-state finds are <100 ms). */
  def findAll(): Seq[StoredMovieRecord] = findAllChecked().answered.getOrElse(Seq.empty)

  override def findAllChecked(): tools.ReadOutcome[Seq[StoredMovieRecord]] = coll match {
    case Some(_) =>
      val buf = Vector.newBuilder[StoredMovieRecord]
      scan.scanStitched(batch => buf ++= batch).collected(buf.result())
    case None => tools.ReadOutcome.Answered(Seq.empty)
  }

  /** The stitched scan's own pages, as each is read — see the trait. */
  override def foreachPage(onPage: Seq[StoredMovieRecord] => Unit): tools.ScanOutcome = coll match {
    case Some(_) => scan.scanStitched(onPage)
    case None    => onPage(Seq.empty); tools.ScanOutcome.complete
  }

  /** Indexed single-document lookup by `_id` — the `/debug` lazy-details endpoint
   *  fetches one row's per-source breakdown when its table row is expanded.
   *  Mirrors [[findAll]]'s decode; an absent `_id` yields `None`. Best-effort:
   *  failures are logged, not thrown, and reported as `false` so a caller can tell an
   *  absent row from an unreadable one — see the trait doc for what conflating them
   *  costs. A row whose SLOT read failed counts as unreadable too: `MovieSideStitch.decode`
   *  declines to build it, and that `None` means "could not look", not "no such film". */
  override def findByIdChecked(id: FilmId): tools.ReadOutcome[StoredMovieRecord] =
    findOneChecked(Filters.eq("_id", id.value), s"findById($id)")

  /** The indexed `_id` lookup without the `screenings` read — see the trait. */
  override def findByIdWithSlotsChecked(id: FilmId): tools.ReadOutcome[StoredMovieRecord] =
    findOneChecked(Filters.eq("_id", id.value), s"findByIdWithSlots($id)", withShowtimes = false)

  /** Indexed lookup by the `key` field — see the trait. */
  override def findByKeyChecked(key: CacheKey): tools.ReadOutcome[StoredMovieRecord] =
    findOneChecked(Filters.eq("key", StoredMovieRecord.keyFor(key)), s"findByKey(${StoredMovieRecord.keyFor(key)})")

  private def findOneChecked(filter: Bson, what: String, withShowtimes: Boolean = true): tools.ReadOutcome[StoredMovieRecord] = coll match {
    case Some(c) =>
      tools.MongoRead.one(what, 10.seconds)(c.find(filter).headOption()) match {
        case tools.ReadOutcome.Answered(dto) =>
          // A row whose slots or screenings could not be read is not readable either.
          stitch.decode(dto, withShowtimes).fold[tools.ReadOutcome[StoredMovieRecord]](
            tools.ReadOutcome.Failed(tools.ReadFailure.Thrown(new IllegalStateException(s"$what: a side-collection read failed"))))(tools.ReadOutcome.Answered(_))
        case absent: tools.ReadOutcome.Absent => absent
        case failed @ tools.ReadOutcome.Failed(cause) =>
          logger.warn(s"MovieRepository.$what failed: ${cause.explain}")
          pages.countIfUndecodable(cause.exception)
          failed
      }
    case None => tools.ReadOutcome.none(what)
  }

  /** Strips each source's `showtimes` SERVER-SIDE so they never cross the wire:
   *  rewrites `sourceData` (a dynamic-cinema-keyed subdocument, so a plain
   *  field-exclusion projection can't target it) by mapping every value through
   *  `$objectToArray` → `$filter` out the `showtimes` key → `$arrayToObject`.
   *  Measured ~58% of the corpus bytes, so this roughly halves the `/debug`
   *  corpus scan. `$sort` on `_id` stays the FIRST stage (index-backed) for the
   *  same exactly-once guarantee as [[findAll]]. A missing `showtimes` decodes to
   *  `Seq.empty` (`MovieCodecs.BackwardCompatibleSourceDataCodec`), so the result
   *  round-trips through the normal `StoredMovieDto` codec. */
  override def findAllForListing(): Seq[StoredMovieRecord] = coll match {
    case Some(c) =>
      // A failed read THROWS, and /debug shows the error: an empty table read as an empty corpus.
      val stripShowtimes = org.bson.Document.parse(
        """{ "$set": { "sourceData": { "$arrayToObject": { "$map": {
          |  "input": { "$objectToArray": { "$ifNull": ["$sourceData", {}] } },
          |  "as": "kv",
          |  "in": { "k": "$$kv.k", "v": { "$arrayToObject": { "$filter": {
          |    "input": { "$objectToArray": "$$kv.v" },
          |    "as": "f",
          |    "cond": { "$ne": ["$$f.k", "showtimes"] } } } } } } } } } }""".stripMargin)
      val pipeline = Seq[Bson](Aggregates.sort(Sorts.ascending("_id")), stripShowtimes)
      val rows = Await.result(c.aggregate[StoredMovieDto](pipeline).batchSize(tools.MongoReplies.Films).toFuture(), 60.seconds)
      // Stitch slots like every other reader. This one reads `movies.sourceData`
      // straight out of an aggregation, so a migrated film — whose slots have moved to
      // `movie_slots` — would otherwise list with NO cinemas at all. Showtimes stay
      // stripped: slots are stored without them, which is exactly what this path wants.
      // A failed slots load can't be recovered from here (the listing has no "partial"
      // shape to return), but it MUST NOT pass silently: every migrated film would
      // render cinema-less and the page would read as a corpus-wide outage. This one is
      // the dev /debug table, so it degrades loudly instead of refusing to render.
      val read     = stitch.allSlotRows()
      val allSlots = read.getOrElse(Map.empty)
      if (read.isEmpty)
        logger.warn("MovieRepository.findAllForListing: movie_slots load failed — every migrated film will " +
          "list with no cinemas. The listing is stale, not the corpus.")
      rows.map(dto => stitch.slotsInto(StoredMovieDto.toDomain(dto, normalizer), allSlots.getOrElse(dto._id, Map.empty)))
    case None => Seq.empty
  }

  /** Stream every persisted record through `f`, one keyset page at a time (via
   *  [[StitchedCorpusScan.scanByKeyset]]), so the caller (the read-model reconcile) never holds more than
   *  one batch — `findAllBatchSize` rows — of `movies` at once. The side collections
   *  `StitchedCorpusScan.scanStitched` preloads are NOT bounded that way; see the trait doc. See
   *  [[MoviesKeysetPages]] for the exactly-once + bounded guarantees and the per-batch retry
   *  (the 2026-06-29 served-films flap, where a batch blew its 60s budget under worker
   *  CPU throttle). Returns `true` only when the scan ran to the last page; `false` when
   *  a batch still fails after its retries — so a PRUNING caller
   *  (`ReadModelSweeper.reconcile`) doesn't treat the rows-so-far as the full corpus
   *  and delete the live rows it never reached.
   *
   *  Stitches split-read showtimes back in via the SAME [[StitchedCorpusScan.scanStitched]] path as
   *  [[findAll]] — its callers need them: `ReadModelSweeper.reconcile` PROJECTS
   *  screenings (un-stitched empty showtimes would make it prune every film's
   *  `web_screenings`), and `WorkerShowtimesMetrics` counts them. Prune-safety +
   *  bounded-heap guarantees live in `StitchedCorpusScan.scanStitched`. */
  override def foreachRecord(f: StoredMovieRecord => Unit): tools.ScanOutcome =
    scan.scanStitched(_.foreach(f))

  /** Count-only scan: pages the movies cursor WITHOUT the `screenings` load [[foreachRecord]]
   *  does, so each row's showtimes are empty. Cheap enough to run on a 5-min metrics timer
   *  without a repeated full-collection screenings read. See the trait doc for the invariant. */
  override def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit): tools.ScanOutcome =
    scan.scanByKeyset(Filters.empty())(_.foreach(dto => f(StoredMovieDto.toDomain(dto, normalizer))))

  override def foreachRecordWithSlots(f: StoredMovieRecord => Unit): tools.ScanOutcome =
    scan.scanStitched(_.foreach(f), withShowtimes = false)

  /** The `updatedAt` range read, through the same stitched scan as [[foreachRecord]] — the
   *  catch-up re-projects what it reads, so a row from here must be as whole as one the
   *  change stream would have delivered. */
  override def foreachRecordUpdatedSince(since: Instant)(f: StoredMovieRecord => Unit): tools.ScanOutcome =
    scan.scanStitched(_.foreach(f), filter = Filters.gt("updatedAt", BsonDateTime(since.toEpochMilli)))

  /** Remove the film `id` with its side-collection rows. */
  def delete(id: FilmId): WriteOutcome = coll.fold[WriteOutcome](WriteOutcome.Declined("no-store")) { c =>
    write("delete", s"MovieRepository.delete($id)") {
      val deleted = Await.result(c.deleteOne(Filters.eq("_id", id.value)).toFuture(), 10.seconds).getDeletedCount
      if (deleted > 0) RemovalAudit.filmRemoved("movies.delete", id.value, reason = "by-id")
      WriteOutcome.all(screenings.map(_.deleteFilm(id.value)) ++ slots.map(_.deleteFilm(id.value)))
    }
  }

  /** Every write's exception handling — see [[RepositoryWrite]]. */
  private def write(op: String, what: => String)(body: => WriteOutcome): WriteOutcome =
    RepositoryWrite.attempt(MovieRepository.Collection, op, what, writeMetrics, logger)(body)

  /** The sweep is [[StrandedSideRows.sweep]]'s; this store only supplies the live `_id`
   *  set, read through the same keyset paging as every other corpus scan but projected
   *  to `_id` alone — a few thousand short strings, not the documents. `None` when a page
   *  still failed after its retries, which the sweep treats as "delete nothing". */
  override def deleteStrandedSideRows(): StrandedSideRows =
    StrandedSideRows.sweep(screenings, slots, liveIds = () => liveIdsChecked())

  private def liveIdsChecked(): Option[Set[String]] = coll.flatMap(pages.ids)

  /** The whole-record write every scrape merge takes — see [[WholeRecordUpsert]]. */
  def upsert(film: FilmId, cacheKey: CacheKey, e: MovieRecord): WriteOutcome =
    upserts.fold[WriteOutcome](WriteOutcome.Declined("no-store"))(_.upsert(film, cacheKey, e))

  /** The field-level update of a stored row — see [[InPlaceMovieUpdate]]. */
  def updateIfPresent(film: FilmId, cacheKey: CacheKey, before: MovieRecord, after: MovieRecord): Boolean =
    updates.exists(_.updateIfPresent(film, cacheKey, before, after))

  /** The stored document the dotted-name fallback of [[InPlaceMovieUpdate]] patches — a seam a spec uses to land
   *  another write between this read and the replace. */
  protected def dottedReplaceRead(c: MongoCollection[StoredMovieDto], id: String): Option[InPlaceMovieUpdate.ReadDocument] =
    Option(Await.result(c.withDocumentClass[org.bson.BsonDocument]().find(Filters.eq("_id", id)).first().toFuture(), 10.seconds))
      .map(whole => InPlaceMovieUpdate.ReadDocument(
        c.codecRegistry.get(classOf[StoredMovieDto]).decode(new org.bson.BsonDocumentReader(whole), org.bson.codecs.DecoderContext.builder().build()),
        whole))

  /** The `movies` collection, None on a disabled store — what [[MongoMovieChangeFeed]] opens its cursor on. */
  private[movies] def collection: Option[MongoCollection[StoredMovieDto]] = coll
  /** The side collections a change stream mixed in ([[MongoMovieChangeFeed]]) watches beside `movies`. */
  private[movies] def screeningsStore: Option[SlotKeyedRows] = screenings
  private[movies] def slotsStore: Option[SlotKeyedRows] = slots

  /** Nothing of its own to release: the client is the composition root's `MongoConnection`'s. */
  def close(): Unit = ()
}
