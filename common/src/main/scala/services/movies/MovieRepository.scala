package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.{ReplaceOptions, UpdateOptions}
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import models.{MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.bson.{BsonDateTime, BsonNull}
import org.mongodb.scala.model.{Aggregates, Filters, IndexOptions, Indexes, Sorts, Updates}
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription}
import org.bson.conversions.Bson
import play.api.Logging
import tools.{Env, RetryWithBackoff}

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** One persisted (title, year) → MovieRecord row. Used as the return type
 *  of `MovieRepository.findAll` and `MovieCache.snapshot` so callers iterate
 *  named fields instead of destructuring an anonymous 3-tuple.
 *
 *  `persistedId` carries the row's actual Mongo `_id` when it came from storage
 *  (set by [[StoredMovieRecord.fromStorage]]); `None` for rows synthesized
 *  in-memory (the cache snapshot, tests), where the canonical `idFor` form is
 *  the id. [[idOf]] prefers it so two distinct documents can never share a DOM
 *  id — see [[idOf]] for why re-deriving the id is not safe. */
case class StoredMovieRecord(title: String, year: Option[Int], record: MovieRecord, persistedId: Option[String] = None)

object StoredMovieRecord {
  /** The Mongo `_id` for a `(title, year)` row: `sanitize(title)|year`. The one
   *  formula the repository keys rows by — exposed so the change stream and the
   *  /debug live view can key DOM rows on the same id the store does. Matches
   *  the in-memory `CacheKey` normalization (case/diacritic-folded). */
  def idFor(title: String, year: Option[Int]): String =
    s"${TitleNormalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"

  /** The `_id` of a stored row. Prefers the actual `persistedId` over re-deriving
   *  `idFor(title, year)`: the display `title` is derived from `sourceData`, so a
   *  clean doc whose cinema reports the title WITH the year baked in (e.g.
   *  "Zabriskie Point (1970)") re-sanitizes to a DIFFERENT prefix than its `_id`
   *  — colliding with whatever doc that prefix actually belongs to. Two distinct
   *  Mongo documents then render the same `data-id` and the /debug live view's
   *  first-match DOM lookup opens whichever row is first. The persisted `_id` is
   *  unique by construction, so keying on it keeps the rows independent. */
  def idOf(row: StoredMovieRecord): String = row.persistedId.getOrElse(idFor(row.title, row.year))

  /** Rebuild a stored row from its persisted `_id` and `MovieRecord`, deriving
   *  the display `title` and `year` rather than reading pinned columns — used by
   *  the Mongo codec (`MovieCodecs.toDomain`), whose BSON drops the `title`/
   *  `year` fields. The `_id` is `sanitize(title)|year`: `sanitize` never emits
   *  `|`, so the suffix is the year and the prefix is the cache key's sanitized
   *  form. Every spelling in a row sanitizes to that prefix (the `CacheKey`
   *  identity), so `displayTitle(prefix)` sanitizes back to it — the rebuilt key
   *  recomputes to the same `_id`, no re-keying churn. (The in-memory repository keeps
   *  the full record in memory and returns its title verbatim, so it needs no
   *  recovery step; for realistic rows the two agree.) */
  def fromStorage(id: String, record: MovieRecord): StoredMovieRecord = {
    val sep      = id.lastIndexOf('|')
    val idPrefix = if (sep >= 0) id.substring(0, sep) else id
    val year     = if (sep >= 0) id.substring(sep + 1).toIntOption else None
    StoredMovieRecord(record.displayTitle(idPrefix), year, record, persistedId = Some(id))
  }
}

/**
 * Persistent store for `(title, year) → MovieRecord` records.
 *
 * The trait is what consumers (`MovieCache`, scripts, integration tests) see
 * — `MongoMovieRepository` (production) and `InMemoryMovieRepository` (tests) are the two
 * implementations. Per CLAUDE.md's DIP guidance: every collaborator is wired
 * via the trait; the concrete type only appears at the composition root
 * (`AppLoader`) and in test setup.
 */
trait MovieRepository {
  /** Whether the persistence layer is wired up. When false, callers can still
   *  use the in-memory cache but writes are no-ops. */
  def enabled: Boolean

  /** Snapshot of every persisted record. Returns empty when disabled. */
  def findAll(): Seq[StoredMovieRecord]

  /** The single row stored under this exact `_id` (the [[StoredMovieRecord.idOf]]
   *  form), or `None` when absent. Lets the dev `/debug` page render ONE row's
   *  heavy per-source breakdown lazily, on expand, instead of every row's
   *  up front — rendering the whole corpus's details in one Twirl pass OOM'd the
   *  view. The default scans [[findAll]] (fine for the in-memory store);
   *  `MongoMovieRepository` overrides it with an indexed `_id` lookup. */
  def findById(id: String): Option[StoredMovieRecord] =
    findAll().find(row => StoredMovieRecord.idOf(row) == id)

  /** Like [[findAll]] but with each source's `showtimes` list dropped — the
   *  rows for a LISTING that renders only per-cinema metadata + counts, never
   *  the showtimes themselves (the dev `/debug` corpus table; showtimes there
   *  are fetched per-row on expand). Measured on prod, `showtimes` are ~58% of
   *  the corpus bytes, so omitting them roughly halves what the scan transfers
   *  and holds. Callers that NEED showtimes (cache hydrate, read-model
   *  projection) must use [[findAll]]/[[findById]]. The default strips in-process
   *  via [[MovieRepository.withoutShowtimes]]; `MongoMovieRepository` overrides
   *  it to strip server-side so the bytes never cross the wire. */
  def findAllForListing(): Seq[StoredMovieRecord] =
    findAll().map(MovieRepository.withoutShowtimes)

  /** Stream every persisted record through `f`, one row at a time, without
   *  materialising the whole collection on the heap. The default loads via
   *  [[findAll]] — fine for the in-memory store — while `MongoMovieRepository`
   *  overrides it to page the cursor by `_id`, so the worker's read-model
   *  reconcile never holds the full ~13 MB `movies` corpus (incl. multi-MB raw
   *  `sourceData` rows) in memory at once. Ordering and the concurrent-write
   *  no-duplicate/no-skip guarantee match [[findAll]] (keyset pagination on the
   *  unique, immutable `_id` index).
   *
   *  Returns `true` when the WHOLE corpus was scanned, `false` when a read failed
   *  mid-scan and the iteration stopped early (rows delivered so far still reached
   *  `f`). A caller that PRUNES on a row's absence — the read-model reconcile —
   *  MUST treat `false` as "this set is not the complete corpus" and skip the
   *  destructive step, or a transient Mongo read failure deletes live rows. The
   *  in-memory store never fails, so the default reports `true`. */
  def foreachRecord(f: StoredMovieRecord => Unit): Boolean = { findAll().foreach(f); true }

  /** Like [[foreachRecord]] but WITHOUT re-injecting showtimes from `screenings` —
   *  so under the split each row's showtimes are EMPTY. For count-only callers that
   *  never read showtimes (`WorkerCorpusMetrics`, ad-hoc rating/audit scripts): it
   *  skips the full-collection `screenings` load [[foreachRecord]] pays on every
   *  scan, which on a 5-min metrics timer is a wasteful repeated read. Any caller
   *  that reads `.showtimes` MUST use [[foreachRecord]] instead — the name here is
   *  the guard. Default delegates to the (stitched, safe) [[foreachRecord]]. */
  def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit): Boolean = foreachRecord(f)

  /** Remove every record matching the given (title, year). Best-effort —
   *  failures are logged, never thrown. */
  def delete(title: String, year: Option[Int]): Unit

  /** Remove the record stored under this exact `_id`. Unlike [[delete]] (which
   *  keys off `(title, year)` → `documentId`), this targets a row by its raw,
   *  possibly NON-canonical `_id` — used to reap a mis-keyed orphan whose stored
   *  `_id` no longer matches `idFor(displayTitle, year)` (a row first stored under
   *  a cinema's original-language title whose display form later drifted to the
   *  Polish one, leaving two `movies` docs for one film). Best-effort — failures
   *  are logged, never thrown. */
  def deleteById(id: String): Unit

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
   *  clobbered by a full-document replace. */
  def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord): Boolean

  /** Stream out-of-band changes to persisted rows as they happen, so the cache
   *  can apply each change incrementally instead of periodically reloading the
   *  whole collection. `onUpsert` fires once per inserted / updated / replaced
   *  row, already decoded. Best-effort: out-of-band *deletes* and any gap while
   *  the stream reconnects are left to the periodic backstop rehydrate, so a
   *  store that can't stream (disabled, or a standalone Mongo with no change
   *  streams) may return `None` and the caller simply relies on that backstop.
   *  Multiple consumers may attach — they share ONE underlying stream (see
   *  [[ChangeStreamFanout]]); the returned handle detaches just that consumer.
   *  Default: not supported. */
  def watchUpserts(onUpsert: StoredMovieRecord => Unit): Option[AutoCloseable] =
    watchChanges(onUpsert, _ => ())

  /** Like [[watchUpserts]] but also surfaces out-of-band DELETEs (by `_id`), so
   *  a consumer that must reflect row *removal* sees it — the /debug live view,
   *  where a merge deletes the losing row and the row must disappear. `onDelete`
   *  gets the raw `_id` (`sanitize(title)|year`, the [[StoredMovieRecord.idFor]]
   *  form). Default: not supported (returns None), same as [[watchUpserts]]. */
  def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: String => Unit
  ): Option[AutoCloseable] = None

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit
}

object MovieRepository {
  /** A copy of `row` with every source's `showtimes` dropped — the shared rule
   *  behind [[MovieRepository.findAllForListing]]. `MongoMovieRepository` strips
   *  the same field server-side; this keeps the in-memory store's listing view
   *  byte-for-byte equivalent so both impls honour the same "no showtimes"
   *  contract (the listing renders cinema metadata + counts, never showtimes). */
  def withoutShowtimes(row: StoredMovieRecord): StoredMovieRecord =
    row.copy(record = row.record.copy(
      data = row.record.data.view.mapValues(_.copy(showtimes = Seq.empty)).toMap))
}

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
  sharedDb: Option[MongoDatabase] = None,
  // Scripts pass `sharedDb = None` and expect us to connect from
  // `MONGODB_URI` ourselves (default true). Wiring sets it to false:
  // when `MongoConnection` is already attempted, an explicit `None`
  // means it failed and re-running our own init would just hit the
  // same DNS / TLS timeout twice. Saves ~15s of boot time on the
  // offline / unreachable-cluster path.
  fallbackToOwnInit: Boolean = true,
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
  // Observability sink for the shared change stream — counts events by op and
  // update-field kind. Noop for scripts/web/tests; the worker injects the
  // Prometheus-backed sink so /metrics carries change-stream stats.
  changeStreamMetrics: ChangeStreamMetrics = ChangeStreamMetrics.noop,
  // Showtimes live in the separate `screenings` collection, not embedded in the
  // `movies` document — so a showtime change no longer rewrites the (formerly
  // 1-2MB) film doc the change stream re-decodes on every write. Wiring `screenings`
  // turns the split ON: `movies` is written WITHOUT showtimes, reads stitch them
  // back from `screenings`, and a `screenings` change is fanned out as a stitched
  // record. The stitch is authoritative — a slot with no `screenings` doc has no
  // showtimes (the one-time embedded→screenings migration is complete, so `movies`
  // carries no showtimes). Maintenance scripts/tests that pass `None` keep the plain
  // embedded shape (they don't serve).
  screenings: Option[ScreeningsRepository] = None
) extends MovieRepository with Logging {

  private def stripFor(data: Map[Source, SourceData]): Map[Source, SourceData] =
    if (screenings.isDefined) ScreeningsRepository.stripShowtimes(data) else data

  /** Re-inject a stored row's showtimes from `screenings` (its authority under the
   *  split), given that film's `slotKey -> showtimes` map. No-op without a split. */
  private def stitchRow(r: StoredMovieRecord, scr: Map[String, Seq[Showtime]]): StoredMovieRecord =
    if (screenings.isEmpty) r
    else r.copy(record = r.record.copy(data = ScreeningsRepository.stitch(r.record.data, scr)))

  /** Decode one stored row and re-inject its showtimes from `screenings` — the
   *  per-film read-stitch shared by [[findById]] and the change-stream fan-out. */
  private def decodeStitched(dto: StoredMovieDto): StoredMovieRecord =
    stitchRow(StoredMovieDto.toDomain(dto), screenings.map(_.findForFilm(dto._id)).getOrElse(Map.empty))

  // Lazy so subclasses that override every wire method (e.g.
  // `InMemoryMovieRepository` in tests) never trigger a Mongo connection
  // attempt — `new InMemoryMovieRepository()` was waiting 10 seconds per test
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
  private lazy val initResult: (Option[MongoClient], Option[MongoCollection[StoredMovieDto]]) =
    sharedDb match {
      case Some(db) =>
        val withRegistry = db.withCodecRegistry(MovieCodecs.registry)
        // Relaxed write concern (w:1, j:false): `movies` is re-scraped continuously,
        // so a write lost to a crash is recovered by the next scrape pass. Skipping
        // the journal sync cuts per-write cost on the shared-CPU Mongo — the worker's
        // write rate is what throttles it. Same trade `MongoTaskQueue` already makes.
        val coll = withRegistry.getCollection[StoredMovieDto]("movies")
          .withWriteConcern(WriteConcern.W1.withJournal(false))
        ensureIndexes(coll)
        (None, Some(coll))
      case None if fallbackToOwnInit => init()
      case None                      => (None, None)
    }
  private def clientOpt: Option[MongoClient]                     = initResult._1
  private def coll:      Option[MongoCollection[StoredMovieDto]] = initResult._2

  def enabled: Boolean = coll.isDefined

  /** Test seam: the write concern configured on the `movies` collection. */
  def collectionWriteConcern: Option[WriteConcern] = coll.map(_.writeConcern)

  /** Boot-time + periodic full reload of every persisted row. Pages the cursor by
   *  `_id` (keyset, via [[scanByKeyset]]) and collects the batches, rather than pulling
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
   *  `scanByKeyset` sorts each page by the unique, immutable `_id` index, so — exactly
   *  as the old single sorted cursor did — the scan returns each document once (no
   *  duplicate at a page boundary, no skipped row) even under concurrent writes.
   *
   *  On an INCOMPLETE scan (a batch still failing after its retries) returns
   *  `Seq.empty` — findAll's historical failure contract: `MovieCache.rehydrate` treats
   *  an empty result as "transient Mongo failure, keep the current cache" rather than
   *  acting on a partial corpus. The 60s per-batch timeout (vs the 10s on point writes)
   *  still covers a cold WiredTiger first read after a process boot (10–20 s even when
   *  steady-state finds are <100 ms). */
  def findAll(): Seq[StoredMovieRecord] = coll match {
    case Some(_) =>
      val buf      = Vector.newBuilder[StoredMovieRecord]
      val complete = scanStitched(batch => buf ++= batch)
      if (complete) buf.result() else Seq.empty
    case None => Seq.empty
  }

  /** The ONE stitched corpus scan — keyset-paged movies + showtimes re-injected from
   *  `screenings` — shared by [[findAll]] and [[foreachRecord]] so the two can never
   *  disagree on a film's showtimes (the divergence that dropped 129 films: a reader
   *  that forgot to stitch made the reconcile prune live `web_screenings`). Loads the
   *  (small) screenings map once, then hands each batch, stitched, to `onBatch`.
   *
   *  Prune-safety: a screenings repo wired but returning an EMPTY map means the bulk
   *  load failed — projecting the stripped (empty-showtime) rows would let a pruning
   *  caller wipe the read model — so bail as "incomplete" (`false`), exactly like a
   *  failed movies batch. The movies pages stay keyset-bounded; only the screenings
   *  map (separate small docs) is held. */
  private def scanStitched(onBatch: Seq[StoredMovieRecord] => Unit): Boolean = {
    val allScr = screenings.map(_.findAll()).getOrElse(Map.empty)
    if (screenings.isDefined && allScr.isEmpty) {
      logger.warn("MovieRepository.scanStitched: screenings load empty — treating scan as incomplete so a reconcile can't prune on stripped rows.")
      false
    } else scanByKeyset(batch =>
      onBatch(batch.map(dto => stitchRow(StoredMovieDto.toDomain(dto), allScr.getOrElse(dto._id, Map.empty)))))
  }

  /** Keyset-paged scan of the whole `movies` collection by `_id`, shared by [[findAll]]
   *  and [[foreachRecord]]. Reads one `findAllBatchSize`-row page at a time — each a
   *  fresh, bounded `find(_id > lastSeen).sort(_id).limit(n)` — and hands every decoded
   *  batch to `onBatch`. Two guarantees both callers rely on:
   *
   *   - Exactly-once: `_id` is unique and immutable and the `gt`/sort run server-side,
   *     so a concurrent write (the worker re-keys years, clears `detailPending`, …) can
   *     neither resurface a visited row nor hide one — no duplicate at a page boundary,
   *     no skip. (The prior single `_id`-sorted cursor gave the same guarantee.)
   *   - Bounded: no single cursor buffers the entire corpus, so the async driver's
   *     synchronous read-completion chain can't recurse into a `StackOverflowError`
   *     (see [[findAll]]).
   *
   *  Each BATCH read is retried independently (keyset pagination makes every batch a
   *  fresh, idempotent `find`) before the scan is declared incomplete. Returns `true`
   *  only when the scan reached the last page; `false` when a batch still failed after
   *  its retries — rows delivered so far still reached `onBatch`, so a PRUNING caller
   *  must treat `false` as "not the complete corpus" and skip its destructive step. */
  private def scanByKeyset(onBatch: Seq[StoredMovieDto] => Unit): Boolean = coll match {
    case Some(c) =>
      Try {
        var afterId: Option[String] = None
        var more = true
        while (more) {
          val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
          val batch  = RetryWithBackoff(
            label          = "MovieRepository keyset batch",
            maxAttempts    = foreachRecordBatchAttempts,
            initialBackoff = foreachRecordBatchBackoff
          ) {
            Await.result(
              c.find(filter).sort(Sorts.ascending("_id")).limit(findAllBatchSize).toFuture(), 60.seconds)
          }
          onBatch(batch)
          afterId = batch.lastOption.map(_._id)
          more    = batch.sizeIs == findAllBatchSize
        }
        true
      }.recover {
        case exception: Throwable =>
          logger.warn(s"MovieRepository keyset scan failed after retries: " +
            s"${exception.getClass.getSimpleName}: ${exception.getMessage} — scan incomplete")
          false
      }.getOrElse(false)
    case None => false
  }

  /** Indexed single-document lookup by `_id` — the `/debug` lazy-details endpoint
   *  fetches one row's per-source breakdown when its table row is expanded.
   *  Mirrors [[findAll]]'s decode; an absent `_id` yields `None`. Best-effort:
   *  failures are logged, not thrown. */
  override def findById(id: String): Option[StoredMovieRecord] = coll match {
    case Some(c) =>
      Try {
        Option(Await.result(c.find(Filters.eq("_id", id)).first().toFuture(), 10.seconds))
          .map(decodeStitched)
      }.recover {
        case exception: Throwable =>
          logger.warn(s"MovieRepository.findById($id) failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          None
      }.getOrElse(None)
    case None => None
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
      Try {
        val stripShowtimes = org.bson.Document.parse(
          """{ "$set": { "sourceData": { "$arrayToObject": { "$map": {
            |  "input": { "$objectToArray": { "$ifNull": ["$sourceData", {}] } },
            |  "as": "kv",
            |  "in": { "k": "$$kv.k", "v": { "$arrayToObject": { "$filter": {
            |    "input": { "$objectToArray": "$$kv.v" },
            |    "as": "f",
            |    "cond": { "$ne": ["$$f.k", "showtimes"] } } } } } } } } } }""".stripMargin)
        val pipeline = Seq[Bson](Aggregates.sort(Sorts.ascending("_id")), stripShowtimes)
        Await.result(c.aggregate[StoredMovieDto](pipeline).toFuture(), 60.seconds).map(StoredMovieDto.toDomain)
      }.recover {
        case exception: Throwable =>
          logger.warn(s"MovieRepository.findAllForListing failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
    case None => Seq.empty
  }

  /** Stream every persisted record through `f`, one keyset page at a time (via
   *  [[scanByKeyset]]), so the caller (the read-model reconcile) never holds more than
   *  one batch — `findAllBatchSize` rows — of the ~13 MB corpus at once. See
   *  [[scanByKeyset]] for the exactly-once + bounded guarantees and the per-batch retry
   *  (the 2026-06-29 served-films flap, where a batch blew its 60s budget under worker
   *  CPU throttle). Returns `true` only when the scan ran to the last page; `false` when
   *  a batch still fails after its retries — so a PRUNING caller
   *  (`ReadModelProjector.reconcile`) doesn't treat the rows-so-far as the full corpus
   *  and delete the live rows it never reached.
   *
   *  Stitches split-read showtimes back in via the SAME [[scanStitched]] path as
   *  [[findAll]] — its callers need them: `ReadModelProjector.reconcile` PROJECTS
   *  screenings (un-stitched empty showtimes would make it prune every film's
   *  `web_screenings`), and `WorkerShowtimesMetrics` counts them. Prune-safety +
   *  bounded-heap guarantees live in `scanStitched`. */
  override def foreachRecord(f: StoredMovieRecord => Unit): Boolean =
    scanStitched(_.foreach(f))

  /** Count-only scan: pages the movies cursor WITHOUT the `screenings` load [[foreachRecord]]
   *  does, so each row's showtimes are empty. Cheap enough to run on a 5-min metrics timer
   *  without a repeated full-collection screenings read. See the trait doc for the invariant. */
  override def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit): Boolean =
    scanByKeyset(_.foreach(dto => f(StoredMovieDto.toDomain(dto))))

  /** Deletes by `_id` (the current `documentId` formula) OR by the legacy `title` +
   *  `year` fields. Current documents no longer persist `title`/`year` (the `_id`
   *  encodes both — see `StoredMovieDto`), so they're caught by the `_id`
   *  branch. The legacy field branch still catches OLD-format documents whose `_id`
   *  used a prior `documentId` formula but which carry the `title`/`year` columns —
   *  `_id`-only would silently miss those orphans and they'd survive every
   *  startup's merge. */
  def delete(title: String, year: Option[Int]): Unit = coll.foreach { c =>
    val yearFilter = year match {
      case Some(y) => Filters.eq("year", y)
      // year=None in the in-memory model lands as either BsonNull() or a
      // missing field in legacy documents; cover both.
      case None    => Filters.or(Filters.eq("year", BsonNull()), Filters.exists("year", false))
    }
    val filter = Filters.or(
      Filters.eq("_id", documentId(title, year)),
      Filters.and(Filters.eq("title", title), yearFilter)
    )
    Try {
      val result = Await.result(c.deleteMany(filter).toFuture(), 10.seconds)
      if (result.getDeletedCount > 1)
        logger.info(s"MovieRepository.delete($title, $year) removed ${result.getDeletedCount} document(s).")
      screenings.foreach(_.deleteFilm(documentId(title, year)))
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"MovieRepository.delete($title, $year) failed: ${exception.getMessage}")
    }
  }

  def deleteById(id: String): Unit = coll.foreach { c =>
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds)
      screenings.foreach(_.deleteFilm(id))
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"MovieRepository.deleteById($id) failed: ${exception.getMessage}")
    }
  }

  def upsert(title: String, year: Option[Int], e: MovieRecord): Unit = coll.foreach { c =>
    val id   = documentId(title, year)
    // Under the read-split, `movies` carries no showtimes (they go to `screenings`).
    val dto  = StoredMovieDto.fromDomain(id, e.copy(data = stripFor(e.data)), Instant.now())
    val opts = new ReplaceOptions().upsert(true)
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", id), dto, opts).toFuture(), 10.seconds)
      // Write this film's cinema showtimes to `screenings` (their authority).
      screenings.foreach(_.replaceFilm(id, ScreeningsRepository.showtimesOf(e.data)))
      ()
    }.recover {
      case exception: Throwable if isClusterClosed(exception) =>
        // Shutdown race — the lifecycle closed the MongoClient while a worker
        // was still mid-write. Harmless: the in-memory cache already has the
        // value and the next refresh will persist it.
        logger.debug(s"MovieRepository.upsert($title, $year) skipped — Mongo client closing.")
      case exception: Throwable =>
        logger.warn(s"MovieRepository.upsert($title, $year) failed: ${exception.getMessage}")
    }
  }

  def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord): Boolean = coll match {
    case None => false
    case Some(c) =>
      val id = documentId(title, year)
      // Showtime deltas → `screenings` (its authority under the split); from the
      // ORIGINAL records. Only when a screenings repo is wired.
      val ops = if (screenings.isDefined) ScreeningsRepository.slotOps(before.data, after.data)
                else Map.empty[String, Option[Seq[Showtime]]]
      // Movies patch from the (split-stripped) records, so a showtimes-only change
      // yields an EMPTY movies patch — movies stays put, no fat change event. When
      // both are empty the row already equals `after`: skip the write (and its no-op
      // `$set` + change event). "Present and up to date" is still success.
      val strippedAfter = after.copy(data = stripFor(after.data))
      val patch = MovieRecordPatch.diff(before.copy(data = stripFor(before.data)), strippedAfter)
      if (patch.isEmpty && ops.isEmpty) true
      else Try {
        // MongoDB update-operator paths treat '.' as a nesting separator, so a
        // per-source `$set` on `sourceData.<displayName>` is rejected when a source's
        // displayName has a dot ("Helios Ostrów Wlkp."); fall back to a conditional
        // full-document replace there. `None` movies patch = a screenings-only change.
        val moviesMatched: Option[Long] =
          if (patch.isEmpty) None
          else Some(
            if (patch.data.keysIterator.exists(_.displayName.contains('.'))) {
              val dto = StoredMovieDto.fromDomain(id, strippedAfter, Instant.now())
              Await.result(c.replaceOne(Filters.eq("_id", id), dto, new ReplaceOptions().upsert(false)).toFuture(), 10.seconds)
                .getMatchedCount
            } else {
              Await.result(c.updateOne(Filters.eq("_id", id), patchToUpdate(patch), new UpdateOptions().upsert(false)).toFuture(), 10.seconds)
                .getMatchedCount
            })
        // Present when the movies write matched, OR a screenings-only change (no movies
        // write, None); false only on Some(0) — the row is absent, so don't apply the
        // screenings deltas (no orphan screenings) and report not-present.
        val present = moviesMatched.forall(_ > 0)
        if (present) screenings.foreach { s =>
          ops.foreach {
            case (k, Some(st)) => s.upsertSlot(id, k, st)
            case (k, None)     => s.deleteSlot(id, k)
          }
        }
        present
      }.recover {
        case exception: Throwable if isClusterClosed(exception) => false
        case exception: Throwable =>
          logger.warn(s"MovieRepository.updateIfPresent($title, $year) failed: ${exception.getMessage}")
          false
      }.getOrElse(false)
  }

  // Translate a `MovieRecordPatch` into a `$set`/`$unset` Mongo update. Each
  // scalar field gets its own atom; the `data` map gets per-source
  // `sourceData.<sourceName>` paths so a Tmdb-only refresh doesn't touch a
  // cinema's slot and vice versa. `updatedAt` bumps alongside the real change;
  // `updateIfPresent` skips an empty patch before reaching here, so this never
  // emits an `updatedAt`-only no-op write.
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
    scalar("searchTitle",       p.searchTitle,       (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("tmdbNoMatch",       p.tmdbNoMatch,       (b: Boolean) => new org.mongodb.scala.bson.BsonBoolean(b))
    scalar("detailPending",     p.detailPending,     (b: Boolean) => new org.mongodb.scala.bson.BsonBoolean(b))
    p.data.foreach {
      case (source, FieldUpdate.SetTo(sd)) => atoms += Updates.set(s"sourceData.${source.displayName}", sd)
      case (source, FieldUpdate.Unset)     => atoms += Updates.unset(s"sourceData.${source.displayName}")
      case (_, FieldUpdate.NoChange)       => ()
    }
    atoms += Updates.set("updatedAt", BsonDateTime(Instant.now().toEpochMilli))
    Updates.combine(atoms.toSeq*)
  }

  /** Open a MongoDB change stream and route each change to `onUpsert` /
   *  `onDelete`. `UPDATE_LOOKUP` makes insert/update/replace events carry the
   *  full post-image (not just the delta), so we always hand a complete row to
   *  `onUpsert`. A DELETE has no `fullDocument`, so we surface its `documentKey._id`
   *  to `onDelete` instead (what the cache's periodic backstop used to be the
   *  only path for, and what the /debug live view needs so a merged-away row
   *  disappears). The driver auto-resumes across transient blips; a terminal
   *  error just logs. Requires a replica set (a single-node RS counts); on a
   *  standalone Mongo the stream errors out and the caller falls back to its
   *  backstop. */
  // ONE shared change-stream cursor feeds every registered listener through this
  // fan-out, rather than a cursor per caller. The worker attaches two consumers
  // (MovieCache + ReadModelProjector); a cursor-per-caller decoded every write
  // twice, and a profiler showed that async change-stream I/O completion was the
  // worker's dominant CPU cost. Decode once here, dispatch to all. The cursor
  // starts on the first listener and stops when the last one detaches.
  private val movieChanges = new ChangeStreamFanout[StoredMovieRecord]("MovieRepository")
  private val changeSub    = new AtomicReference[Subscription]()
  private val changeLock   = new AnyRef
  // Read-split only: a second cursor on `screenings`. A showtime change writes only
  // `screenings` (movies stays put), so without this the projector would never see it.
  private val screeningsWatch = new AtomicReference[Option[AutoCloseable]](None)

  override def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: String => Unit
  ): Option[AutoCloseable] = coll.map { c =>
    val handle = movieChanges.register(onUpsert, onDelete)
    ensureWatching(c)
    new AutoCloseable { override def close(): Unit = { handle.close(); stopWatchingIfIdle() } }
  }

  /** Start the single shared cursor if it isn't already running. Each event is
   *  decoded once and fanned out to every listener; a delete (no post-image) is
   *  surfaced by `_id`. Terminal errors clear the subscription so a later
   *  registration can re-open, and existing listeners fall back to their
   *  periodic backstop (cache rehydrate / projector reconcile) meanwhile. */
  private def ensureWatching(c: MongoCollection[StoredMovieDto]): Unit = changeLock.synchronized {
    if (changeSub.get() == null) {
      c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
        .subscribe(new Observer[ChangeStreamDocument[StoredMovieDto]] {
          override def onSubscribe(s: Subscription): Unit = { changeSub.set(s); s.request(Long.MaxValue) }
          override def onNext(change: ChangeStreamDocument[StoredMovieDto]): Unit = {
            recordChangeMetrics(change)
            Option(change.getFullDocument) match {
              case Some(dto) =>
                // The movies doc has no showtimes — stitch them back from `screenings`
                // (via decodeStitched) before fanning out, so consumers get a full row.
                movieChanges.dispatchUpsert(decodeStitched(dto))
              case None =>
                // No post-image ⇒ a delete (the only op UPDATE_LOOKUP can't
                // back-fill). Surface its _id so consumers can drop the row.
                Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
                  .map(v => if (v.isString) v.asString.getValue else v.toString)
                  .foreach(movieChanges.dispatchDelete)
            }
          }
          override def onError(e: Throwable): Unit = {
            logger.warn(s"MovieRepository change stream ended (${e.getMessage}) — relying on the periodic backstop rehydrate.")
            changeSub.set(null)
          }
          override def onComplete(): Unit = changeSub.set(null)
        })
      logger.info("MongoMovieRepository: watching change stream (shared by all listeners).")
      // Also watch `screenings`: a showtime change fires there, not on `movies`.
      // Re-read + stitch (findById already stitches) + fan out so the projector
      // re-projects the film.
      if (screeningsWatch.get().isEmpty)
        screeningsWatch.set(screenings.flatMap(_.watch(filmId => findById(filmId).foreach(movieChanges.dispatchUpsert))))
    }
  }

  /** Stop the shared cursor once no listener remains, so an idle repository
   *  (e.g. web /debug after every viewer disconnects) doesn't keep decoding
   *  every write for nothing. */
  private def stopWatchingIfIdle(): Unit = changeLock.synchronized {
    if (movieChanges.isEmpty) {
      Option(changeSub.getAndSet(null)).foreach(_.unsubscribe())
      screeningsWatch.getAndSet(None).foreach(h => Try(h.close()))
    }
  }

  /** Whether the single shared change-stream cursor is currently running — for
   *  diagnostics/tests (it starts on the first listener, stops after the last). */
  def isWatchingChangeStream: Boolean = changeSub.get() != null

  /** Count each change event by op, and each UPDATE by which field kind changed,
   *  onto the injected sink (noop unless the worker wired the Prometheus one). Best
   *  effort — instrumentation must never break the stream. */
  private def recordChangeMetrics(change: ChangeStreamDocument[StoredMovieDto]): Unit = Try {
    import scala.jdk.CollectionConverters._
    val op = ChangeStreamMetrics.normalizeOp(Option(change.getOperationType).map(_.getValue).getOrElse(""))
    changeStreamMetrics.recordEvent(op)
    if (op == ChangeStreamMetrics.Op.Update) {
      val desc    = Option(change.getUpdateDescription)
      val updated = desc.flatMap(d => Option(d.getUpdatedFields)).map(_.keySet.asScala.toSet).getOrElse(Set.empty[String])
      val removed = desc.flatMap(d => Option(d.getRemovedFields)).map(_.asScala.toSet).getOrElse(Set.empty[String])
      ChangeStreamMetrics.updateKinds(updated, removed).foreach(changeStreamMetrics.recordUpdateKind)
    }
  }.recover { case exception => logger.warn(s"change-stream metrics failed: ${exception.getMessage}") }.getOrElse(())

  def close(): Unit = clientOpt.foreach(_.close())

  /** Index `(title, year)` so [[delete]]'s `$or(_id, title+year)` filter resolves
   *  by index union instead of a full collection scan. The stored documents no longer
   *  carry `title`/`year` columns (the 2026-06-11 derived-title migration dropped
   *  them), so for current rows the second `$or` branch matches nothing — but
   *  without this index Mongo still COLLSCANs the whole collection to prove that
   *  on every delete (~400ms / ~1100 documents examined per delete; the single largest
   *  source of `movies` read-lock time in prod). With the index the branch is a
   *  1-key IXSCAN. The index stays cheap (currently all-null entries, ~24KB) and
   *  still catches any legacy stale-`_id` document that DOES carry the columns — the
   *  delete-by-(title,year) safety net the change-stream regression depends on.
   *  Idempotent + best-effort: a re-create is a no-op, a failure only logs. */
  private def ensureIndexes(coll: MongoCollection[StoredMovieDto]): Unit =
    Try {
      Await.result(
        coll.createIndex(Indexes.ascending("title", "year"), new IndexOptions().background(true)).toFuture(),
        10.seconds)
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"movies (title, year) index creation failed: ${exception.getMessage}")
    }

  private def init(): (Option[MongoClient], Option[MongoCollection[StoredMovieDto]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoMovieRepository disabled (in-memory cache only).")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName  = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client  = MongoClient(uri)
          val db      = client.getDatabase(dbName).withCodecRegistry(MovieCodecs.registry)
          // Relaxed write concern — see the sharedDb path above.
          val coll    = db.getCollection[StoredMovieDto]("movies")
            .withWriteConcern(WriteConcern.W1.withJournal(false))
          // Touch the collection to surface connectivity errors at startup,
          // not on the first read after the app is "up".
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          ensureIndexes(coll)
          logger.info(s"MongoMovieRepository connected to $dbName.movies")
          (client, coll)
        }.recover {
          case exception: Throwable =>
            logger.error(s"MongoMovieRepository init failed (${exception.getMessage}) — falling back to in-memory cache.")
            null
        }.toOption.filter(_ != null) match {
          case Some((c, coll)) => (Some(c), Some(coll))
          case None            => (None, None)
        }
    }

  // The driver throws IllegalStateException("state should be: open") from
  // BaseCluster / DefaultConnectionPool once MongoClient.close() has fired.
  private def isClusterClosed(exception: Throwable): Boolean =
    Option(exception.getMessage).exists(_.contains("state should be: open"))

  // Match the in-memory CacheKey's normalization rules so case-only and
  // diacritic variants of the same title share a single Mongo record. Without
  // this, "Tom i Jerry: Przygoda w muzeum" and "Tom i jerry: przygoda w
  // muzeum" — both reported by different cinemas for the same film — each get
  // their own row, and only one can be updated per hourly refresh tick (the
  // tick walks the deduplicated Caffeine cache).
  private def documentId(title: String, year: Option[Int]): String =
    StoredMovieRecord.idFor(title, year)
}
