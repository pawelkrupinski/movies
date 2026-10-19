package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.{ReplaceOptions, UpdateOptions}
import models.{MovieRecord, Showtime, SourceData}
import org.mongodb.scala.bson.{BsonDateTime, BsonDocument, BsonString}
import org.mongodb.scala.model.{Aggregates, Filters, Projections, Sorts}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.bson.conversions.Bson
import play.api.Logging

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

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
  clock: java.time.Clock,
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
  // Persist the change-stream resume token so the shared cursor reopens (after a terminal
  // error or a WORKER RESTART) from where it left off — replaying events that landed while
  // this process was down, closing the gap the consumers' periodic backstops exist for. ON
  // only in the WORKER (the durable read-model / cache mirror); OFF for web /debug + scripts,
  // whose ephemeral cursor position must not clobber the worker's in the shared token doc.
  persistResumeToken: Boolean = false,
  // The country whose rules derive a row's `_id` (`sanitize(title)|year`). The
  // document id IS the row's identity, so a repository writing under another
  // country's rules would split or collide rows. REQUIRED, not defaulted: this is
  // a production persistence path, and a silent fallback here is the 2026 incident.
  override val normalizer: TitleNormalizer,
  // How far each change-stream cursor may run ahead of the apply thread. Injected only
  // so a spec can prove the bound with a handful of writes instead of a full window's
  // worth; production never passes it. See [[ChangeStreamDemand]].
  changeDemandWindow: Int = ChangeStreamDemand.DefaultWindow,
  // How long a change waits for the rest of its film's burst before the film is
  // re-read — see `MovieChangeStream.Debounce`. The worker passes its country's; None re-reads at once.
  changeDebounce: Option[MovieChangeStream.Debounce] = None,
  // What the SCREENINGS cursor's apply does — its events, and the ones it coalesced away.
  // Separate from `changeStreamMetrics` (which is the `movies` cursor's) because they are
  // two different streams answering two different questions; the worker happens to satisfy
  // both with one object. See [[ScreeningsMetrics]].
  screeningsMetrics: ScreeningsMetrics = ScreeningsMetrics.noop,
  // The same for the SLOTS cursor — the third stream on the same projector, counted under
  // its own collection label. See [[SideCollectionChangeMetrics]].
  slotsMetrics: SideCollectionChangeMetrics = SideCollectionChangeMetrics.noop,
  // Where a write that THREW is counted — see [[WriteOutcome]]. Noop for scripts/web/tests.
  writeMetrics: RepositoryWriteMetrics = RepositoryWriteMetrics.noop,
  // Where a `movies` document the codec could not decode is counted — the same counter as the
  // read model's skipped documents. Noop for scripts/web/tests.
  decodeFailures: services.readmodel.DecodeFailureMetrics = services.readmodel.DecodeFailureMetrics.noop
) extends MovieRepository with KeyAddressedMovieWrites with Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)
  /** Every `updatedAt` this store writes: strictly increasing within the process, so the stamp
   *  is a version the dotted-replace guard can trust (two writes in one millisecond no longer
   *  share one) and a cursor the catch-up can trust (its floor is a stamp from this same
   *  sequence — see [[ChangeStreamLiveness.now]]). */
  private val stamps = new tools.MonotonicStampSequence(clock)


  override def hasScreenings: Boolean = screenings.isDefined
  override def hasSlots:       Boolean = slots.isDefined

  // How a stored row is made whole again from `movie_slots` and `screenings`.
  private val stitch = new MovieSideStitch(screenings, slots, normalizer, logger)

  /** The film's slots at `cinemas`, read from those venues alone — see [[MovieSideStitch.readVenues]]. */
  private[movies] def readVenues(filmId: String, cinemas: Set[models.Cinema]): Option[VenueSlots] = stitch.readVenues(filmId, cinemas)

  // Lazy so constructing the store touches no server: the index builds run on
  // first use. We apply our own codec registry to the handed-in database (a
  // view, not a clone — the underlying client is the `MongoConnection`'s) and
  // grab our collection from it.
  private lazy val initResult: (Option[MongoDatabase], Option[MongoCollection[StoredMovieDto]]) =
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
        (Some(withRegistry), Some(coll))
      case None => (None, None)
    }
  private def database: Option[MongoDatabase]                   = initResult._1
  private def coll:     Option[MongoCollection[StoredMovieDto]] = initResult._2

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
  def findAll(): Seq[StoredMovieRecord] = findAllChecked().answered.getOrElse(Seq.empty)

  override def findAllChecked(): tools.ReadOutcome[Seq[StoredMovieRecord]] = coll match {
    case Some(_) =>
      val buf = Vector.newBuilder[StoredMovieRecord]
      scanStitched(batch => buf ++= batch).collected(buf.result())
    case None => tools.ReadOutcome.Answered(Seq.empty)
  }

  /** The stitched scan's own pages, as each is read — see the trait. */
  override def foreachPage(onPage: Seq[StoredMovieRecord] => Unit): tools.ScanOutcome = coll match {
    case Some(_) => scanStitched(onPage)
    case None    => onPage(Seq.empty); tools.ScanOutcome.complete
  }

  /** The ONE stitched corpus scan — keyset-paged movies + showtimes re-injected from
   *  `screenings` — shared by [[findAll]] and [[foreachRecord]] so the two can never
   *  disagree on a film's showtimes (the divergence that dropped 129 films: a reader
   *  that forgot to stitch made the reconcile prune live `web_screenings`). Reads each
   *  page's side rows, then hands the page, stitched, to `onBatch`.
   *
   *  Projection-safety: a page whose side read FAILED is never handed on — its rows,
   *  stitched from the empty answer, would read as films with no cinemas or no showtimes,
   *  and a projecting caller writes exactly that. The page is skipped and the scan reports
   *  "incomplete", exactly like a failed movies batch, so a pruning caller skips
   *  its destructive step too.
   *
   *  Side rows are fetched PER PAGE, for exactly the films that page holds, rather than preloaded
   *  whole. Both are one indexed `filmId $in [...]` query. `foreachRecord` promises its callers it
   *  never holds more than a page, and that stopped being true when the bulk of a row moved into the
   *  side collections: this preloaded both entire collections first (7.5 MB each on prod PL, more on
   *  UK) and held them for the whole scan, on a worker with a 320 MB heap and an OOM history. Now the
   *  peak is one page's films, whatever the corpus grows to. Completeness still gates the whole scan,
   *  not the page: a caller that PRUNES on a row's absence must not act on a partial view, so one
   *  failed side read fails the scan exactly as a failed `movies` batch does. */
  private def scanStitched(onBatch: Seq[StoredMovieRecord] => Unit, withShowtimes: Boolean = true,
                           filter: Bson = Filters.empty()): tools.ScanOutcome = {
    var sideReadsComplete = true
    val timing = new ScanTiming
    var films  = 0
    // One pool for the scan: equal instants, rooms and formats across its pages are one object ([[ShowtimePool]]).
    val shared = new ShowtimePool
    val moviesComplete = scanByKeyset(filter) { batch =>
      val ids = batch.map(_._id).toSet
      val (pageScr, pageSlots) = readPageSides(ids, withShowtimes, timing)
      // A page whose side read failed is NOT handed on, stitched from the empty answer: its
      // films would arrive with no cinemas or no showtimes, and "incomplete" only stops a
      // prune — `ReadModelSweeper.reconcile` and the sweep's catch-up PROJECT every row they
      // are given, so each film on the page lost every served showtime (the per-row
      // `MovieSideStitch.decode` declines for the same reason). The page is skipped and the scan
      // reported incomplete; the rows it could stitch are still delivered.
      (pageScr, pageSlots) match {
        case (Some(showtimes), Some(slotRows)) =>
          films += batch.size
          onBatch(timing.stitching(batch.map(dto => stitch.row(StoredMovieDto.toDomain(dto, normalizer),
            showtimes.getOrElse(dto._id, Map.empty).map { case (slot, read) => slot -> shared.showtimes(read) },
            slotRows.getOrElse(dto._id, Map.empty)))))
        case _ =>
          sideReadsComplete = false
          logger.warn(s"MovieRepository.scanStitched: a side-collection read failed for a page of " +
            s"${ids.size} film(s) (screenings ok=${pageScr.isDefined}, slots ok=${pageSlots.isDefined}) — skipping the page and treating the " +
            "scan as incomplete, so no caller acts on films whose cinemas or showtimes it could not see.")
      }
    }
    timing.report(films, wholeCorpus = filter.toBsonDocument.isEmpty)
    moviesComplete.andThen(tools.ScanOutcome.of(sideReadsComplete, "a side-collection read failed for a page of films"))
  }

  /** One page's side rows — its showtimes (empty when `withShowtimes` is off) and its slots — each
   *  None when its read failed. `withShowtimes = false` skips the showtimes read entirely rather
   *  than discarding its result: the caller has said it never looks at a showtime, and this is the
   *  expensive half. The two reads are independent (each is `filmId $in` the page's ids), so they
   *  run at once: in sequence they were most of a US boot's 16–21 s cache hydrate, one round-trip
   *  after the other for every page. */
  private def readPageSides(ids: Set[String], withShowtimes: Boolean, timing: ScanTiming)
      : (Option[Map[String, Map[String, Seq[Showtime]]]], Option[Map[String, Map[String, SourceData]]]) = {
    val slotsRead = scala.concurrent.Future(scala.concurrent.blocking(timing.slotReads(stitch.slotRowsOf(ids))))(using scala.concurrent.ExecutionContext.global)
    val pageScr   = if (!withShowtimes) Some(Map.empty[String, Map[String, Seq[Showtime]]]) else timing.screeningReads(stitch.screeningRowsOf(ids))
    (pageScr, Await.result(slotsRead, Duration.Inf))
  }

  /** Where a scan's time goes, logged once per scan (the boot hydrate, the rare backstop): the
   *  restart's critical path runs through here, and a guess at its bottleneck already shipped once
   *  without moving it. */
  private final class ScanTiming {
    private val wall   = tools.Stopwatch.start()
    val slotReads      = tools.Stopwatch.total()
    val screeningReads = tools.Stopwatch.total()
    val stitching      = tools.Stopwatch.total()

    /** A whole-corpus scan (the hydrate, the backstop rehydrate) at info; a catch-up's
     *  updated-since slice, which runs on every read-model sweep, only at debug. */
    def report(films: Int, wholeCorpus: Boolean): Unit = {
      val line = s"MovieRepository.scanStitched: $films film(s) in ${wall.millis}ms — slot reads " +
        s"${slotReads.elapsed.toMillis}ms, screening reads ${screeningReads.elapsed.toMillis}ms (alongside), " +
        s"stitch ${stitching.elapsed.toMillis}ms over ${stitching.count} page(s)."
      if (wholeCorpus) logger.info(line) else logger.debug(line)
    }
  }

  /** Keyset-paged scan of the whole `movies` collection by `_id`, shared by [[findAll]]
   *  and [[foreachRecord]]. Reads one `findAllBatchSize`-row page at a time — each a
   *  fresh, bounded `find(_id > lastSeen).sort(_id).limit(n)` — and hands every decoded
   *  batch to `onBatch`. Two guarantees both callers rely on:
   *
   *   - Exactly-once: `_id` is unique and immutable and the `gt`/sort run server-side,
   *     so a concurrent write (the worker re-keys years, …) can
   *     neither resurface a visited row nor hide one — no duplicate at a page boundary,
   *     no skip. (The prior single `_id`-sorted cursor gave the same guarantee.)
   *   - Bounded: no single cursor buffers the entire corpus, so the async driver's
   *     synchronous read-completion chain can't recurse into a `StackOverflowError`
   *     (see [[findAll]]).
   *
   *  Each BATCH read is retried independently (keyset pagination makes every batch a
   *  fresh, idempotent `find`) before the scan is declared incomplete — complete only when
   *  the scan reached the last page. Rows delivered before a batch that still failed after
   *  its retries reached `onBatch`, so a PRUNING caller must treat an incomplete scan as
   *  "not the complete corpus" and skip its destructive step. */
  private def scanByKeyset(filter: Bson)(onBatch: Seq[StoredMovieDto] => Unit): tools.ScanOutcome = coll match {
    case Some(c) =>
      keysetScan[StoredMovieDto]("keyset", _._id, countIfUndecodable) { (afterId, limit) =>
        val page = afterId.fold(filter)(id => Filters.and(filter, Filters.gt("_id", id)))
        Await.result(
          c.find(page).sort(Sorts.ascending("_id")).limit(limit).batchSize(tools.MongoReplies.Films).toFuture(), 60.seconds)
      }(onBatch)
    case None => tools.ScanOutcome.Incomplete(new tools.IncompleteScanException("no movies collection"))
  }

  /** The keyset scan both corpus reads share — the documents and the bare `_id`s — with this
   *  store's page size and per-batch retry budget. A scan still failing after its retries is
   *  logged, then handed to `onFailure`. */
  private def keysetScan[A](what: String, keyOf: A => String, onFailure: Throwable => Unit = _ => ())(
      fetchPage: (Option[String], Int) => Seq[A])(onBatch: Seq[A] => Unit): tools.ScanOutcome =
    KeysetScan.scan[A](
      label          = s"MovieRepository $what batch",
      batchSize      = findAllBatchSize,
      maxAttempts    = foreachRecordBatchAttempts,
      initialBackoff = foreachRecordBatchBackoff,
      keyOf          = keyOf,
      fetchPage      = fetchPage,
      onIncomplete   = exception => {
        logger.warn(s"MovieRepository $what scan failed after retries: " +
          s"${exception.getClass.getSimpleName}: ${exception.getMessage} — scan incomplete")
        onFailure(exception)
      }
    )(onBatch)

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

  /** A read that failed on a document the codec refused: counted, since it fails every read that
   *  meets it (one such document leaves the whole corpus scan incomplete) and is otherwise a WARN. */
  private def countIfUndecodable(failure: Throwable): Unit =
    if (services.readmodel.DecodeFailureMetrics.isDecodeFailure(failure))
      decodeFailures.recordDecodeFailure(services.readmodel.DecodeFailureMetrics.SourceMoviesCollection)

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
          countIfUndecodable(cause.exception)
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
   *  [[scanByKeyset]]), so the caller (the read-model reconcile) never holds more than
   *  one batch — `findAllBatchSize` rows — of `movies` at once. The side collections
   *  `scanStitched` preloads are NOT bounded that way; see the trait doc. See
   *  [[scanByKeyset]] for the exactly-once + bounded guarantees and the per-batch retry
   *  (the 2026-06-29 served-films flap, where a batch blew its 60s budget under worker
   *  CPU throttle). Returns `true` only when the scan ran to the last page; `false` when
   *  a batch still fails after its retries — so a PRUNING caller
   *  (`ReadModelSweeper.reconcile`) doesn't treat the rows-so-far as the full corpus
   *  and delete the live rows it never reached.
   *
   *  Stitches split-read showtimes back in via the SAME [[scanStitched]] path as
   *  [[findAll]] — its callers need them: `ReadModelSweeper.reconcile` PROJECTS
   *  screenings (un-stitched empty showtimes would make it prune every film's
   *  `web_screenings`), and `WorkerShowtimesMetrics` counts them. Prune-safety +
   *  bounded-heap guarantees live in `scanStitched`. */
  override def foreachRecord(f: StoredMovieRecord => Unit): tools.ScanOutcome =
    scanStitched(_.foreach(f))

  /** Count-only scan: pages the movies cursor WITHOUT the `screenings` load [[foreachRecord]]
   *  does, so each row's showtimes are empty. Cheap enough to run on a 5-min metrics timer
   *  without a repeated full-collection screenings read. See the trait doc for the invariant. */
  override def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit): tools.ScanOutcome =
    scanByKeyset(Filters.empty())(_.foreach(dto => f(StoredMovieDto.toDomain(dto, normalizer))))

  override def foreachRecordWithSlots(f: StoredMovieRecord => Unit): tools.ScanOutcome =
    scanStitched(_.foreach(f), withShowtimes = false)

  /** The `updatedAt` range read, through the same stitched scan as [[foreachRecord]] — the
   *  catch-up re-projects what it reads, so a row from here must be as whole as one the
   *  change stream would have delivered. */
  override def foreachRecordUpdatedSince(since: Instant)(f: StoredMovieRecord => Unit): tools.ScanOutcome =
    scanStitched(_.foreach(f), filter = Filters.gt("updatedAt", BsonDateTime(since.toEpochMilli)))

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

  private def liveIdsChecked(): Option[Set[String]] = coll.flatMap { c =>
    val ids = Set.newBuilder[String]
    val complete = keysetScan[String]("id keyset", identity) { (afterId, limit) =>
      val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
      Await.result(c.find[Document](filter).projection(Projections.include("_id"))
        .sort(Sorts.ascending("_id")).limit(limit).batchSize(tools.MongoReplies.Default).toFuture(), 60.seconds)
        .map(_("_id").asString.getValue)
    }(ids ++= _)
    Option.when(complete.isComplete)(ids.result())
  }

  def upsert(film: FilmId, cacheKey: CacheKey, e: MovieRecord): WriteOutcome = coll.fold[WriteOutcome](WriteOutcome.Declined("no-store")) { c =>
    val id    = film.value
    val key   = StoredMovieRecord.keyFor(cacheKey)
    val label = s"MovieRepository.upsert(${cacheKey.cleanTitle}, ${cacheKey.year})"
    val restitch = restitchOf(id, e)
    // Read the stored document FIRST, before anything is written. `MoviesUpsert.plan` needs it
    // regardless, but reading it first ALSO lets the duplicate-key guard run before the slots
    // write rather than after — see `collidesWithAnother`. Handed on as the `Try` it is: a read
    // that FAILED and a document that is ABSENT both write, but only one of them is "no such
    // film", and the decision is where that is pinned. Read here, beside the replace it feeds,
    // so `NoUnguardedReadModifyWriteSpec` sees the pair (and its allowlisted WHY).
    val stored   = Try(Await.result(c.find(Filters.eq("_id", id)).limit(1).toFuture(), 10.seconds)).map(_.headOption)
    if (collidesWithAnother(c, id, key, e, stored.toOption.flatten)) {
      logIdentityHeld(label, e, "")
      WriteOutcome.IdentityHeld
    } else {
      // Slots first — see `writeSlots`.
      val slotsWrite  = writeSlots(id, restitch)
      // What to write, and whether the stored document already equals it — the decision is
      // `MoviesUpsert`'s, so it is unit-tested apart from the three reads that feed it.
      val plan        = MoviesUpsert.plan(id, key, e, restitch.data, slotsWrite.contains(WriteOutcome.Written), slotsForStorage, stored, stamps.next())
      val moviesWrite = write("upsert", label) {
        val moviesLanded = plan.unchanged ||
          Try(Await.result(c.replaceOne(Filters.eq("_id", id), plan.document, new ReplaceOptions().upsert(true)).toFuture(), MovieRepository.WriteTimeout))
            .map(_ => true).recover {
              // The pre-check above catches every collision its read could see; this remains as
              // defence for the residual race it cannot — a sibling document landing the same
              // key/tmdbId between that read and this write. Slots have already landed in that
              // narrow window, the accepted "one delayed row" trade `writeSlots` makes on this path.
              case exception: Throwable if isDuplicateKey(exception) =>
                logIdentityHeld(label, e, s" (${exception.getMessage})")
                false
            }.get
        if (!moviesLanded) WriteOutcome.IdentityHeld else writeScreenings(id, restitch)
      }
      // A failed slots write is reported too, after the movies half has had its go: the embedded
      // copy kept the film's cinemas, but only a retry lands the rows themselves, and the caller
      // retries only a write it was told FAILED.
      WriteOutcome.all(moviesWrite +: slotsWrite.toSeq)
    }
  }

  /** The record an upsert writes, its slots' showtimes re-stitched. A whole-record write can carry
   *  slots STRIPPED for the cache; `showtimesOf` would drop them and `replaceFilm` would DELETE their
   *  screenings. …and a re-stitch whose READ failed under-reports the film: every slot it could not
   *  refill looks showtime-less, so the `replaceFilm` would delete it. `complete` carries that
   *  distinction down to the write, and `stored` carries the read itself, so the write can tell an
   *  unchanged film from a changed one without asking again. */
  private def restitchOf(id: String, e: MovieRecord): ScreeningsSplit.ReStitched =
    screenings.fold(ScreeningsSplit.ReStitched(e.data, Map.empty, complete = true))(ScreeningsSplit.reStitchChecked(_, id, e.data))

  /** Whether another document already holds the key or tmdbId this upsert would give the film.
   *
   *  Only asked when THIS call is about to give the film a key or tmdbId it did not already have
   *  stored — i.e. a re-key or a resolution, not the every-venue-every-tick re-merge that is
   *  upsert's normal load. That gates the extra read off the hot path: a tick that changes neither
   *  can't newly collide with a sibling document.
   *
   *  Landing the slots write BEFORE knowing the `movies` write will succeed left three films
   *  stranded on 2026-09-16: the slots write landed a genuine TMDB match, the `movies` write then
   *  hit the OTHER document already holding that tmdbId and was swallowed, and the row was left
   *  with a resolved TMDB slot but `tmdbId` permanently null — invisible on the site, and never
   *  revisited (the since-deleted `UnresolvedTmdbReaper` treated `tmdbId`+`tmdbAttempt` both absent
   *  as "never even tried"). Checking for that sibling before either write means a real collision
   *  refuses the whole upsert — slots included — leaving the film exactly as it was, instead of
   *  leaving the two collections disagreeing about it forever. */
  private def collidesWithAnother(c: MongoCollection[StoredMovieDto], id: String, key: String, e: MovieRecord,
                                  storedDoc: Option[StoredMovieDto]): Boolean =
    MoviesUpsert.identityChanging(storedDoc.map(d => MoviesUpsert.Identity(d.key, d.tmdbId)), key, e.tmdbId) &&
      Try(Await.result(c.find(Filters.and(
        Filters.ne("_id", id),
        Filters.or(List(Some(Filters.eq("key", key)), e.tmdbId.map(t => Filters.eq("tmdbId", t))).flatten*)
      )).limit(1).toFuture(), 10.seconds)).toOption
        .exists(_.exists(d => MoviesUpsert.heldBy(key, e.tmdbId)(MoviesUpsert.Identity(d.key, d.tmdbId))))

  private def logIdentityHeld(label: String, e: MovieRecord, detail: String): Unit =
    logger.warn(s"$label refused: another document already holds its " +
      s"key or its tmdbId=${e.tmdbId.getOrElse("?")} — the film keeps its previous document$detail")

  /** An upsert's slots write — FIRST, and `movies` only drops its embedded copy once it has
   *  actually landed. Dropping it on a FAILED slot write would leave the film with no cinemas in
   *  either place — the one way this migration can lose data — and a slots failure never throws, so
   *  it can't break the movies write, so the write itself has to report back. A film whose slot
   *  write failed keeps the embedded map, and the upsert reports the failure so the caller's next
   *  identical scrape retries it.
   *
   *  Skipped when the stored rows already match. `upsert` is the whole-record path every scrape
   *  merge takes, and `replaceFilm` rewrites EVERY row of the film — 471 of them for a film showing
   *  across the UK — so a scrape that changed only showtimes would otherwise churn the entire slot
   *  set for nothing. One indexed read replaces that. Already-matching counts as LANDED: the rows
   *  are correct, so the embedded copy is still safe to drop. A failed read returns empty, which
   *  reads as "differs" and writes — the safe direction.
   *
   *  That ONE read is used twice ([[SlotsRepository.applyFilm]]): it answers "is anything different
   *  at all" (skip the write entirely) and, handed on, "which rows are different" (write only those)
   *  — which `replaceFilm` would otherwise have to go and ask again, on the hottest write path in
   *  the system. The read is not fenced against a concurrent writer, so a slot that changes between
   *  this read and the write can leave us skipping a row whose value happens to match what we read;
   *  the price is one delayed row, which the film's next scrape writes. Not worth a transaction on
   *  the hottest write path. */
  private def writeSlots(id: String, restitch: ScreeningsSplit.ReStitched): Option[WriteOutcome] =
    slots.map(SlotsRepository.applyFilm(_, id, SlotsRepository.slotsOf(restitch.data)))

  /** Write this film's cinema showtimes to `screenings` (their authority). `replaceFilm` is upsert
   *  PLUS a delete of every slot the record doesn't name, so it may only run on a record we know is
   *  complete. When the re-stitch read failed we still write what this tick positively carries, but
   *  never the delete half — a slot we simply could not read is not a slot that stopped screening.
   *
   *  The whole call is skipped when the stored rows already match — the same guard the slots write
   *  has, and here it is FREE: `reStitchChecked` has already read these rows, so `restitch.stored`
   *  costs no round trip where the slots half pays one. This is the outer of TWO guards, and they
   *  answer different questions. `replaceFilm` itself drops the rows that did not move (see
   *  `changedSlots`), which is what stops one venue's change rewriting all 298 rows of a German
   *  release. This one asks whether ANY row moved, and when none did it saves `replaceFilm` the read
   *  it would need to find that out. `upsert` is the whole-record path EVERY scrape merge takes and
   *  a film at N venues is written by N venues, so that read is worth skipping on its own.
   *
   *  Equality is safe against the delete vector: if the stored rows equal what we would write,
   *  there is no slot for `replaceFilm` to prune. A differing read — including an empty one —
   *  writes, which is the safe direction. The three-way choice lives in `ScreeningsSplit.applyFilm`
   *  so the in-memory repository makes the same one. */
  private def writeScreenings(id: String, restitch: ScreeningsSplit.ReStitched): WriteOutcome =
    screenings.fold[WriteOutcome](WriteOutcome.Written) { s =>
      ScreeningsSplit.applyFilm(s, id, ScreeningsSplit.screeningsOf(restitch.data), restitch)
    }

  def updateIfPresent(film: FilmId, cacheKey: CacheKey, before: MovieRecord, after: MovieRecord): Boolean = coll match {
    case None => false
    case Some(c) =>
      val id     = film.value
      val update = planUpdate(before, after)
      // `false` for an absent row AND for a failed write — both mean the store does not hold
      // `after`, and `MovieCache.putIfPresent` rolls its copy back on either. A side-collection
      // write that failed counts: its row is what the next identical update retries.
      if (update.isEmpty) true
      else write("updateIfPresent", s"MovieRepository.updateIfPresent(${cacheKey.cleanTitle}, ${cacheKey.year})") {
        // Present when the movies write matched, OR a side-collection-only change (no movies
        // write, None); false only on Some(0) — the row is absent, so don't apply the
        // side-collection deltas (no orphan rows) and report not-present.
        if (!patchMovies(c, id, StoredMovieRecord.keyFor(cacheKey), update.patch).forall(_ > 0)) WriteOutcome.Declined("absent")
        else applySideWrites(id, update)
      } == WriteOutcome.Written
  }

  /** The three writes an update from `before` to `after` makes, each from the ORIGINAL records.
   *
   *  Showtime deltas and listing-key restamps → `screenings` (its authority under the split), only
   *  when a screenings repo is wired. Slot deltas → `movie_slots` (dual write): `slotsOf` drops
   *  showtimes itself, so a showtimes-only change yields no slot write and the two side collections
   *  stay independent.
   *
   *  The movies patch is diffed from the (split-stripped) records, so a showtimes-only change yields
   *  an EMPTY movies patch — movies stays put, no fat change event. When all three are empty the row
   *  already equals `after`: the write (and its no-op `$set` + change event) is skipped. "Present and
   *  up to date" is still success. With the slots split on, `movies` is not where slots live, so the
   *  patch must not carry them: `upsert` drops the embedded map once the slots land, and a later
   *  patch that still wrote `sourceData.<slot>` would resurrect it field by field and undo exactly
   *  the shrink this split exists for. Dropping it here means the embedded map simply goes stale —
   *  reads prefer `movie_slots` whenever a film has rows there, and fall back to that stale copy
   *  only for a film with none, which is the same already-correct value it had before. */
  private def planUpdate(before: MovieRecord, after: MovieRecord): MongoMovieRepository.PlannedUpdate = {
    val ops        = if (screenings.isDefined) ScreeningsSplit.writesFor(before.data, after.data) else ScreeningsWrites.none
    val slotWrites = if (slots.isDefined) SlotsRepository.slotOps(before.data, after.data) else Map.empty[String, Option[SourceData]]
    val rawPatch   = MovieRecordPatch.diff(before.copy(data = slotsForStorage(before.data)), after.copy(data = slotsForStorage(after.data)))
    MongoMovieRepository.PlannedUpdate(if (slots.isDefined) rawPatch.copy(data = Map.empty) else rawPatch, ops, slotWrites)
  }

  /** Write the movies half of an update: the documents it matched, or None for a side-collection-only
   *  change, which leaves `movies` alone — `screenings` and `movie_slots` each have a cursor of their
   *  own on the same fan-out (see `MovieChangeStream`). A slots-only change used to TOUCH `movies`
   *  anyway — a one-field `updatedAt` bump standing in for the watcher `movie_slots` did not have —
   *  and once it had one that bump fanned the same change out twice, which is the double projection
   *  the old arrangement existed to avoid.
   *
   *  MongoDB update-operator paths treat '.' as a nesting separator, so a per-source `$set` on
   *  `sourceData.<displayName>` is rejected when a source's displayName has a dot ("Helios Ostrów
   *  Wlkp."); that patch falls back to [[dottedReplace]]. */
  private def patchMovies(c: MongoCollection[StoredMovieDto], id: String, key: String, patch: MovieRecordPatch): Option[Long] =
    Option.when(!patch.isEmpty) {
      if (patch.data.keysIterator.exists(_.displayName.contains('.'))) dottedReplace(c, id, key, patch)
      else Await.result(c.updateOne(Filters.eq("_id", id), patchToUpdate(patch), new UpdateOptions().upsert(false)).toFuture(), MovieRepository.WriteTimeout)
        .getMatchedCount
    }

  /** The dotted-name fallback: the documents a guarded whole-document replace matched.
   *
   *  It can't drive the field-level diff (the dotted `$set` path is rejected), so it replaces the
   *  whole document. Writing the cache row BLINDLY would NULL any Mongo-owned field the cache lacks
   *  — a rating not yet rehydrated after a restart, or an out-of-band edit (FilmwebUrlAudit). It
   *  reads the current doc and applies the SAME patch to it, so the replace carries exactly the
   *  diff the `$set` path would, every other field preserved. Absent row → nothing to replace →
   *  report not-present.
   *
   *  …and the replace lands only over the document as read, WHOLE: a rating refresh landing between
   *  the read and the replace was wiped by it, the same null this fallback exists to avoid. Not over
   *  its `updatedAt` alone — this process's stamps are strictly increasing, but another process's (a
   *  second worker mid-rollout, a script) can equal the one read, and that write would then be taken
   *  for the row as read. On a mismatch the patch is applied again to the document as it now is. */
  private def dottedReplace(c: MongoCollection[StoredMovieDto], id: String, key: String, patch: MovieRecordPatch): Long =
    tools.GuardedWrite(MovieRepository.DottedReplaceAttempts)(() => dottedReplaceRead(c, id)) { asRead =>
      dottedReplaceRecord(asRead.map(read => StoredMovieDto.toDomain(read.stored, normalizer).record), patch)
    } { (asRead, merged) =>
      // An absent row decided nothing above, so only a row as read reaches here.
      asRead.exists(read => services.MongoGuard.replaceIfUnchanged(c,
        services.MongoGuard.wholeUnchanged(BsonString(id), read.whole),
        StoredMovieDto.fromDomain(id, key, merged, stamps.next()), MovieRepository.WriteTimeout, insert = false))
    } match {
      case tools.GuardedWrite.Landed(_)                 => 1L
      case tools.GuardedWrite.Unneeded                  => 0L
      case tools.GuardedWrite.ChangedUnderYou(attempts) =>
        throw new IllegalStateException(s"'$id' changed between its read and its replace $attempts times in a row")
    }

  /** The side-collection deltas of an update whose row is present. */
  private def applySideWrites(id: String, update: MongoMovieRepository.PlannedUpdate): WriteOutcome =
    WriteOutcome.all(
      screenings.toSeq.flatMap(update.screenings.applyTo(_, id)) ++
      slots.toSeq.flatMap(s =>
        s.upsertSlots(id, update.slots.collect { case (k, Some(sd)) => k -> sd }.toMap) +:
          update.slots.collect { case (k, None) => s.deleteSlot(id, k) }.toSeq))

  /** The full-document replacement to write in the dotted-displayName fallback of
   *  [[updateIfPresent]] (a per-source `$set sourceData.<name>` is rejected when the
   *  name holds a '.'). Apply the SAME field-level patch to the CURRENT persisted
   *  record so the replace carries exactly the diff the `$set` path would — every
   *  Mongo-owned field the in-memory cache row lacks (a rating not yet rehydrated
   *  after a restart, an out-of-band audit edit) is preserved rather than nulled by
   *  blindly writing the cache row. `None` when the row is absent (nothing to
   *  replace → report not-present, don't upsert). Pure — no Mongo I/O. */
  private[movies] def dottedReplaceRecord(persisted: Option[MovieRecord], patch: MovieRecordPatch): Option[MovieRecord] =
    persisted.map(patch.applyTo)

  /** The stored document the dotted-name fallback patches — a seam a spec uses to land another write
   *  between this read and the replace. */
  protected def dottedReplaceRead(c: MongoCollection[StoredMovieDto], id: String): Option[MovieRepository.ReadDocument] =
    Option(Await.result(c.withDocumentClass[org.bson.BsonDocument]().find(Filters.eq("_id", id)).first().toFuture(), 10.seconds))
      .map(whole => MovieRepository.ReadDocument(
        c.codecRegistry.get(classOf[StoredMovieDto]).decode(new org.bson.BsonDocumentReader(whole), org.bson.codecs.DecoderContext.builder().build()),
        whole))

  /** `private[movies]` so `MovieRecordFieldWiringSpec` can assert that every field the
   *  patch carries actually reaches the WIRE — see [[MovieUpdates]]. */
  private[movies] def patchToUpdate(p: MovieRecordPatch): Bson = MovieUpdates.of(p, stamps.next())

  // The `movies` change-stream subscription — shared cursor, fan-out, backpressure,
  // resume token, reopen — lives in [[MovieChangeStream]]; this class only lends it the
  // by-id re-read. None on a disabled store, so `watchChanges` returns None and the
  // caller relies on its periodic backstop.
  private val changeStream: Option[MovieChangeStream] = coll.map { c =>
    new MovieChangeStream(
      source              = MovieChangeStream.Source.ofCollection(c),
      screenings          = screenings,
      slots               = slots,
      reread              = id => findByIdChecked(FilmId(id)),
      fence               = writeFence,
      // The shared cursor reopens (after a terminal error, and — the big win — after a WORKER
      // RESTART) from the last-seen token instead of "now", REPLAYING writes that landed while
      // this process was down — the gap the consumers' periodic backstops exist for. See
      // [[ChangeStreamResumeToken]]; the `screenings` and `movie_slots` streams each persist
      // their own sibling token.
      resumeToken         = new ChangeStreamResumeToken("movies", database, persistResumeToken),
      changeStreamMetrics = changeStreamMetrics,
      screeningsMetrics   = screeningsMetrics,
      slotsMetrics        = slotsMetrics,
      changeDemandWindow  = changeDemandWindow,
      clock               = clock,
      stamps              = stamps,
      decodeFailures      = decodeFailures,
      debounce            = changeDebounce,
      // Only where both side collections are split out is a venue readable alone.
      readVenues          = Option.when(screenings.isDefined && slots.isDefined)(readVenues))
  }

  /** Change events handed to the apply thread but not yet applied — see
   *  [[MovieChangeStream.applyBacklog]]; 0 on a disabled store. */
  def changeApplyBacklog: Int = changeStream.fold(0)(_.applyBacklog)

  override def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = changeStream.map(_.watch(onUpsert, id => onDelete(FilmId(id))))

  override def watchChangesFenced(
    onUpsert: (StoredMovieRecord, Long) => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = changeStream.map(_.watchFenced(onUpsert, id => onDelete(FilmId(id))))

  override def watchChangesWithVenues(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit,
    onVenues: VenueSlots => VenueVerdict
  ): Option[AutoCloseable] = changeStream.map(_.watchFenced((film, _) => onUpsert(film), id => onDelete(FilmId(id)), (venues, _) => onVenues(venues)))

  override def watchChangesFencedWithVenues(
    onUpsert: (StoredMovieRecord, Long) => Unit,
    onDelete: FilmId => Unit,
    onVenues: (VenueSlots, Long) => VenueVerdict
  ): Option[AutoCloseable] = changeStream.map(_.watchFenced(onUpsert, id => onDelete(FilmId(id)), onVenues))

  override def attachChangesWithVenues(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit,
    onVenues: VenueSlots => VenueVerdict
  ): Option[AutoCloseable] = changeStream.map(_.attachFenced((film, _) => onUpsert(film), id => onDelete(FilmId(id)), (venues, _) => onVenues(venues)))

  override def openChanges(): Unit = changeStream.foreach(_.open())

  /** Whether the single shared change-stream cursor is currently running — for
   *  diagnostics/tests (it starts on the first listener, stops after the last). */
  def isWatchingChangeStream: Boolean = changeStream.exists(_.isWatching)

  override def changeStreamLiveness: ChangeStreamLiveness =
    changeStream.fold(unwatchedLiveness)(_.liveness)

  /** A disabled store's: no cursor, nothing ever delivered. */
  private lazy val unwatchedLiveness: ChangeStreamLiveness = ChangeStreamLiveness.unwatched(clock)
  override def heldChanges: Int          = changeStream.fold(0)(_.held)
  override def releaseHeldChanges(): Unit = changeStream.foreach(_.releaseHeld())

  def close(): Unit = changeStream.foreach(_.close())

  /** Mongo's duplicate-key error — a second document claiming a tmdbId the unique index
   *  already holds. */
  private def isDuplicateKey(exception: Throwable): Boolean =
    exception.isInstanceOf[com.mongodb.MongoWriteException] &&
      exception.asInstanceOf[com.mongodb.MongoWriteException].getError.getCategory == com.mongodb.ErrorCategory.DUPLICATE_KEY
}

object MongoMovieRepository {
  /** What an update writes: the `movies` patch, the showtimes deltas and the slot deltas. */
  private final case class PlannedUpdate(patch: MovieRecordPatch, screenings: ScreeningsWrites,
                                         slots: Map[String, Option[SourceData]]) {
    def isEmpty: Boolean = patch.isEmpty && screenings.isEmpty && slots.isEmpty
  }
}
