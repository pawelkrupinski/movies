package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.{ReplaceOptions, UpdateOptions}
import models.{MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.model.{Aggregates, Filters, IndexOptions, Indexes, Projections, Sorts, Updates}
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.bson.conversions.Bson
import play.api.Logging
import tools.Env

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** One persisted film row: its display `title` and `year` (together, its lookup
 *  [[CacheKey]]), the record, and the [[FilmId]] it is stored under. Used as the return
 *  type of `MovieRepository.findAll` and `MovieCache.snapshot` so callers iterate
 *  named fields instead of destructuring an anonymous tuple.
 *
 *  `id` is the row's Mongo `_id` — permanent, see [[FilmId]]. A row synthesised
 *  without storage (tests, the odd in-memory construction) defaults to the legacy
 *  form, its key, which is what every row stored before ids existed carries. */
case class StoredMovieRecord(title: String, year: Option[Int], record: MovieRecord, id: FilmId,
                             storedKey: Option[String] = None) {
  /** The row's lookup key as STORED in the document's `key` field (for a row written
   *  before keys were stored, its `_id`); derived from the title and year only for a
   *  row synthesised without storage. NOT the identity: a retitle changes it, the `id`
   *  stays. */
  def key(normalizer: TitleNormalizer): String = storedKey.getOrElse(StoredMovieRecord.keyFor(title, year, normalizer))

  /** The cache key this row answers to — the stored key, labelled with the display title. */
  private[services] def cacheKey(normalizer: TitleNormalizer): CacheKey =
    storedKey.fold(CacheKey(title, year, normalizer))(CacheKey.stored(title, _))
}

object StoredMovieRecord {
  /** A row synthesised without storage: its id is the legacy form of its key. */
  def apply(title: String, year: Option[Int], record: MovieRecord): StoredMovieRecord =
    StoredMovieRecord(title, year, record, FilmId.legacy(title, year, TitleNormalizer.deployment))

  /** The lookup KEY of a `(title, year)` row: `sanitize(title)|year` — the `key` field of
   *  the document (and, for a row stored before ids existed, its `_id` too). Matches
   *  the in-memory `CacheKey` normalization (case/diacritic-folded). */
  def keyFor(title: String, year: Option[Int], normalizer: TitleNormalizer): String =
    s"${normalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"

  /** The same key, for a caller that already holds it. A [[CacheKey]] carries the
   *  normalised form it was BUILT with, so this needs no normalizer and cannot re-derive
   *  a different one. */
  def keyFor(k: CacheKey): String =
    s"${k.normalized}|${k.year.map(_.toString).getOrElse("")}"

  /** Rebuild a stored row from its persisted `_id`, its `key` field (absent on a
   *  document written before keys were stored — then the `_id` IS the key) and its
   *  `MovieRecord`, deriving the display `title` and `year` rather than reading pinned
   *  columns — used by the Mongo codec (`MovieCodecs.toDomain`), whose BSON drops the
   *  `title`/`year` fields. The key is `sanitize(title)|year`: `sanitize` never emits
   *  `|`, so the suffix is the year and the prefix is the cache key's sanitized form.
   *  Every spelling in a row sanitizes to that prefix (the `CacheKey` identity), so
   *  `displayTitle(prefix)` sanitizes back to it.
   *
   *  CALL IT WITH A COMPLETE RECORD. `displayTitle` names the film from its SLOTS,
   *  so a record whose slots have not been stitched back in yet has nothing to name
   *  it with and falls through to the key prefix — a real title only by accident
   *  ("Interstellar"), otherwise a mangled "Thecabinetofdrcaligari". That is exactly
   *  the state `MovieCodecs.toDomain` decodes into now that the slots live in
   *  `movie_slots`, which is why `MongoMovieRepository.stitchSlots` calls this again
   *  once the record is whole. */
  /** A document with no `key` field — written before ids existed, its `_id` is its key. */
  def fromStorage(id: String, record: MovieRecord, normalizer: TitleNormalizer): StoredMovieRecord =
    fromStorage(id, None, record, normalizer)

  def fromStorage(id: String, key: Option[String], record: MovieRecord, normalizer: TitleNormalizer): StoredMovieRecord = {
    val k        = key.getOrElse(id)
    val sep      = k.lastIndexOf('|')
    val idPrefix = if (sep >= 0) k.substring(0, sep) else k
    val year     = if (sep >= 0) k.substring(sep + 1).toIntOption else None
    StoredMovieRecord(record.displayTitle(idPrefix, normalizer), year, record, FilmId(id), Some(k))
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
  /** Whether the read-split is active — i.e. a `screenings` collection is wired, so
   *  showtimes are stored there and the MovieCache can strip them from resident records.
   *  Without it the cache MUST keep showtimes (there is nowhere else to hold them). */
  def hasScreenings: Boolean = false

  /** Whether the slot split is active — i.e. a `movie_slots` repository is wired, so
   *  each film's per-cinema SourceData is mirrored into its own row. Readers use this
   *  to decide whether a film's slots may live outside the `movies` document. */
  def hasSlots: Boolean = false

  /** The slot map a `movies` document is allowed to carry — showtimes removed once
   *  `screenings` is their authority, kept when there is nowhere else to hold them.
   *
   *  On the trait rather than in the Mongo class because it is not this repository's
   *  private business: `MongoStagingFolder` writes `movies` DIRECTLY, bypassing
   *  `upsert` because its upserts and its staging deletes have to commit in one
   *  session, and it has to write the same shape. It did not, and the difference is
   *  not cosmetic. A slot map with its showtimes inline grows with the number of
   *  venues screening the film, and the United States has 5,031 of them: on
   *  2026-09-01 the fold of `Avengers: Doomsday` threw
   *  `BsonMaximumSizeExceededException` on every single attempt, so the group was
   *  never consumed, so `StagingReaper` re-enqueued it every tick — forever, with no
   *  backoff and no give-up, since the exception carries no transient label to retry
   *  on and none to abandon on either.
   *
   *  The stripped shape loses nothing: under the split `ScreeningsSplit.stitch`
   *  treats `screenings` as authoritative and empties the showtimes of any slot it
   *  has no row for, so an embedded board is discarded on the way back out. It is
   *  weight the reader was already throwing away.
   *
   *  `final` because the rule is the storage contract, not an implementation
   *  detail — a repository that answered differently would be a repository whose
   *  documents mean something else. */
  final def slotsForStorage(data: Map[Source, SourceData]): Map[Source, SourceData] =
    if (hasScreenings) ScreeningsSplit.stripShowtimes(data) else data

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
  def findById(id: FilmId): Option[StoredMovieRecord] = findByIdChecked(id)._1

  /** Like [[findById]] but says whether the READ succeeded, so `None` can be told from
   *  "could not look".
   *
   *  `findById` collapses both into `None`, and one caller cannot afford that:
   *  `MovieCache.stored` uses it as the merge base for a scrape whose film is not in the
   *  cache, and `None` there means "brand-new film". Building a record from scratch makes
   *  it carry ONLY the cinema just scraped, and `MovieRepository.upsert` then writes that
   *  as the whole film — `screenings.replaceFilm` prunes every other cinema's showtimes
   *  with its `$nin`. So a read failure silently empties a live film of every cinema but
   *  one, and a cold cache (every restart) routes the WHOLE corpus through that branch.
   *  That is what emptied the boards on 2026-07-27: a decode bug made this read throw, and
   *  the logs filled with `MovieRepository.findById(…) failed` while the showtime volume
   *  fell to a third across every country.
   *
   *  The in-memory store cannot fail, so the default reports `true`. */
  def findByIdChecked(id: FilmId): (Option[StoredMovieRecord], Boolean) =
    (findAll().find(_.id == id), true)

  /** The row whose `key` field is this lookup key — a cold cache asking "is this
   *  film stored?" before it knows the id. Same checked contract as
   *  [[findByIdChecked]]. At most one row holds a key (the cache keeps its map by it);
   *  the default scans [[findAll]], `MongoMovieRepository` uses the `key` index. */
  def findByKeyChecked(key: CacheKey): (Option[StoredMovieRecord], Boolean) = {
    val k = StoredMovieRecord.keyFor(key)
    (findAll().find(_.key(normalizer) == k), true)
  }

  /** The country whose rules derive a row's `_id`. Defaulted so the in-memory and
   *  inline test implementations need not carry one; `MongoMovieRepository`
   *  overrides it from its constructor, which is the only place the choice is
   *  load-bearing (the id it writes IS the row's identity). ABSTRACT, so a new
   *  implementation cannot inherit a process default by omission. */
  def normalizer: TitleNormalizer

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

  /** Stream every persisted record through `f`, one row at a time. The default loads via
   *  [[findAll]] — fine for the in-memory store — while `MongoMovieRepository` overrides
   *  it to page the `movies` cursor by `_id`. Ordering and the concurrent-write
   *  no-duplicate/no-skip guarantee match [[findAll]] (keyset pagination on the unique,
   *  immutable `_id` index).
   *
   *  Only the `movies` pages are bounded. Under the split, the bulk of what used to be a
   *  `movies` row now lives in the side collections, and `scanStitched` loads BOTH whole
   *  (`screenings` + `movie_slots`) before it starts paging — measured on prod PL
   *  2026-07-27, `movies` fell to 0.4 MB while the two side collections hold 7.5 MB each.
   *  So this bounds the cursor, not the scan's peak heap; the earlier "never holds the
   *  full corpus at once" claim stopped being true when the slots moved out. Splitting
   *  the side loads into per-page lookups would trade that ~15 MB for one round-trip per
   *  page, which is a real change and not one to smuggle into a doc comment.
   *
   *  Returns `true` when the WHOLE corpus was scanned, `false` when a read failed
   *  mid-scan and the iteration stopped early (rows delivered so far still reached
   *  `f`). A caller that PRUNES on a row's absence — the read-model reconcile —
   *  MUST treat `false` as "this set is not the complete corpus" and skip the
   *  destructive step, or a transient Mongo read failure deletes live rows. The
   *  in-memory store never fails, so the default reports `true`. */
  def foreachRecord(f: StoredMovieRecord => Unit): Boolean = { findAll().foreach(f); true }

  /** Like [[foreachRecord]] but stitching NEITHER side collection — so under the split
   *  each row has empty showtimes AND NO CINEMA SLOTS AT ALL. The name undersells that:
   *  `sourceData` moved to `movie_slots`, so a row from here carries only what `movies`
   *  itself stores (ids, ratings, resolution state). For callers that read exactly those
   *  (`WorkerCorpusMetrics`, ad-hoc rating/audit scripts) it is the cheapest scan there is.
   *
   *  A caller that reads `.showtimes` must use [[foreachRecord]]; one that reads `.data` /
   *  `cinemaSlots` — or ANYTHING derived from them, `ReadModelProjection.filmIds` and the
   *  display-title variants included — must use [[foreachRecordWithSlots]], or it will
   *  compute its answer from a film that appears to screen nowhere. Default delegates to
   *  the (fully stitched, safe) [[foreachRecord]]. */
  def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit): Boolean = foreachRecord(f)

  /** Like [[foreachRecord]] but WITHOUT re-injecting showtimes from `screenings` — the
   *  cinema slots ARE stitched, so `.data` is complete and everything derived from it is
   *  correct; only `.showtimes` is empty.
   *
   *  For callers that need to know WHICH FILMS AND CARDS EXIST but never look at a
   *  showtime — the read-model orphan prune is the one that matters. That sweep runs every
   *  30 minutes per country and, through [[foreachRecord]], pulled the entire `screenings`
   *  collection through WiredTiger to compute a set of ids: 177,676 rows / 129 MB across
   *  the five countries as of 2026-09-05, against a 1.07 GB cache, twice an hour, every
   *  byte of it discarded.
   *
   *  NOT a cheaper [[foreachRecordWithoutShowtimes]]: the slots are what
   *  `ReadModelProjection.filmIds` derives display-title variants from, so dropping them
   *  too would have the prune compute FEWER live ids than the read model holds and delete
   *  live cards — the shape that has already cost this repository a 129-film outage.
   *  Default delegates to the (fully stitched, safe) [[foreachRecord]]. */
  def foreachRecordWithSlots(f: StoredMovieRecord => Unit): Boolean = foreachRecord(f)

  /** Remove every record matching the given (title, year). Best-effort —
   *  failures are logged, never thrown. */
  /** Remove the film stored under this id, with its side-collection rows. Best-effort
   *  — failures are logged, never thrown. */
  def delete(id: FilmId): Unit

  /** Move a film's SIDE-COLLECTION rows (`screenings`, `movie_slots`) from one document
   *  id to another — a MERGE of two documents that turned out to be one film (the
   *  tmdbId fold, an imdbId fold, a staging retirement, the hydrate's duplicate
   *  reconcile). A retitle never comes here: the film keeps its id (see [[FilmId]]).
   *
   *  A merge loser is not a departure: its showtimes are filed under ITS id, and nothing
   *  else moves them — `upsert` re-stitches from the id it is writing TO, so at the winner
   *  it finds nothing and stores nothing, while the loser is deleted with its row. The
   *  showtimes are destroyed in between. That is not hypothetical: when every re-key was
   *  such a move, on 2026-07-27 prod shed ~10k upcoming showtimes per cycle in PL alone,
   *  films left intact, rebuilt only by the next scrape — the sawtooth this method ended.
   *
   *  Rows already at `newId` are kept, with the moved ones taking precedence on a shared
   *  slot key (the same direction `rekey`'s record merge carries state forward).
   *  Best-effort and a no-op without side collections wired.
   *
   *  Returns whether the rename may now PROCEED — i.e. whether `oldId` is safe to delete.
   *  `false` means a read or write the move depends on did not happen, so the caller must
   *  leave the film where it is and try again next pass; deleting `oldId` on the strength
   *  of a move that didn't land destroys the film's only copy. See [[SideCollectionMove]]
   *  for the rule. A store with no side collections has nothing to move and reports true. */
  def moveFilm(oldId: FilmId, newId: FilmId): Boolean = true

  /** Remove every side-collection row (`screenings`, `movie_slots`) whose film has no
   *  document in this store any more — the leftovers of deletes and merges from before
   *  [[delete]] cascaded and [[moveFilm]] carried rows, which nothing else ever clears.
   *  The rule, its refusals (an incomplete or empty corpus scan deletes nothing) and the
   *  reason it exists are [[StrandedSideRows]]'s, shared with the in-memory fake.
   *  Best-effort; returns what was removed. A store with no side collections has
   *  nothing stranded and reports [[StrandedSideRows.none]]. */
  def deleteStrandedSideRows(): StrandedSideRows = StrandedSideRows.none

  /** Write-through upsert of the film `id`, stored under the lookup key
   *  `sanitize(title)|year`. The same id under a new key is a RETITLE — the document
   *  stays, its `key` moves. Best-effort — failures are logged, never thrown. */
  def upsert(id: FilmId, key: CacheKey, e: MovieRecord): Unit

  /** [[upsert]] for a caller outside `services` that holds only a title and year (a
   *  fixture re-seeding rows it read back); the key is derived by this store's rules. */
  def upsert(id: FilmId, title: String, year: Option[Int], e: MovieRecord): Unit =
    upsert(id, CacheKey(title, year, normalizer), e)

  def updateIfPresent(id: FilmId, key: CacheKey, before: MovieRecord, after: MovieRecord): Boolean

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
   *  gets the deleted row's id. Default: not supported (returns None), same as
   *  [[watchUpserts]]. */
  def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = None

  /** When each change-stream cursor last DELIVERED an event — see [[ChangeStreamLiveness]].
   *  The worker's `/metrics` ages every cursor off it, and the read-model prune sweep uses
   *  the `movies` cursor's instant as the floor of what a silent stream may have missed.
   *  Default: a repository with no stream, whose cursors age from creation and are never
   *  stamped — [[MongoMovieRepository]] and [[InMemoryMovieRepository]] each answer with the
   *  stream's own. */
  def changeStreamLiveness: ChangeStreamLiveness = unwatchedLiveness
  private lazy val unwatchedLiveness: ChangeStreamLiveness = ChangeStreamLiveness.unwatched()

  /** Release any underlying resources. No-op when nothing to release. */
  def close(): Unit
}

/**
 * Writes addressed by the lookup key rather than the film id — for a caller that has
 * no id in hand: seeding a spec, a one-off script. NOT part of [[MovieRepository]]'s
 * contract: production code holds ids (the cache's index, a stored row) and the
 * contract stays id-addressed; the implementations carry this so a test can write by
 * title and year. The row already stored under the key keeps its id; otherwise the
 * row is created under the key's LEGACY id (the key string itself, the shape every
 * document written before ids existed has), or a fresh id if a document already owns
 * that string. A store that cannot say whether a document holds the key is left alone
 * — a failed read is not "absent", and writing could make a second document.
 */
trait KeyAddressedMovieWrites { self: MovieRepository =>
  def upsert(title: String, year: Option[Int], e: MovieRecord): Unit = {
    val key = CacheKey(title, year, normalizer)
    findByKeyChecked(key) match {
      case (Some(row), _) => upsert(row.id, key, e)
      case (None, true)   =>
        val taken: FilmId => Boolean = id => findByIdChecked(id)._1.isDefined
        upsert(Some(FilmId.legacy(key)).filterNot(taken).getOrElse(FilmId.fresh(key, taken)), key, e)
      case (None, false)  => ()
    }
  }

  def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord): Boolean =
    findByKeyChecked(CacheKey(title, year, normalizer))._1.exists(row => updateIfPresent(row.id, row.cacheKey(normalizer), before, after))

  def delete(title: String, year: Option[Int]): Unit =
    findByKeyChecked(CacheKey(title, year, normalizer))._1.foreach(row => delete(row.id))
}

object MovieRepository {
  /** The corpus collection. Named here rather than inline so
   *  [[services.DebugMirror]] can state what the local /debug mirror has to carry. */
  val Collection = "movies"

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
  // What the SCREENINGS cursor's apply does — its events, and the ones it coalesced away.
  // Separate from `changeStreamMetrics` (which is the `movies` cursor's) because they are
  // two different streams answering two different questions; the worker happens to satisfy
  // both with one object. See [[ScreeningsMetrics]].
  screeningsMetrics: ScreeningsMetrics = ScreeningsMetrics.noop,
  // The same for the SLOTS cursor — the third stream on the same projector, counted under
  // its own collection label. See [[SideCollectionChangeMetrics]].
  slotsMetrics: SideCollectionChangeMetrics = SideCollectionChangeMetrics.noop
) extends MovieRepository with KeyAddressedMovieWrites with Logging {


  override def hasScreenings: Boolean = screenings.isDefined
  override def hasSlots:       Boolean = slots.isDefined


  /** Re-inject a stored row's showtimes from `screenings` (its authority under the
   *  split), given that film's `slotKey -> showtimes` map. No-op without a split. */
  private def stitchRow(r: StoredMovieRecord, scr: Map[String, Seq[Showtime]],
                        storedSlots: Map[String, SourceData] = Map.empty): StoredMovieRecord = {
    val withSlots = stitchSlots(r, storedSlots)
    if (screenings.isEmpty) withSlots
    else withSlots.copy(record = withSlots.record.copy(
      data = ScreeningsSplit.stitch(withSlots.record.data, scr)))
  }

  /** Union a row's stored `movie_slots` rows with whatever its `movies` document still
   *  embeds — see [[SlotsRepository.merge]] for why a union rather than "stored wins",
   *  and for the prod measurement that forced it.
   *
   *  An EMPTY `storedSlots` still means "fall back to the embedded map", but it no longer
   *  has to carry the weight of distinguishing a genuinely slot-less film from a failed
   *  read: [[SlotsRepository.findForFilmChecked]] answers that directly, and callers
   *  refuse to build a record at all when the read failed.
   *
   *  Re-derives the DISPLAY TITLE from the stitched record, because
   *  `StoredMovieDto.toDomain` could not: it named the film from the `movies`
   *  document's own `sourceData`, which the slot split leaves EMPTY, so
   *  `displayTitle` fell through to its fallback — the sanitized `_id` prefix —
   *  and every hydrated row came back as "Thecabinetofdrcaligari". The cache keyed
   *  the corpus under those mangled spellings (harmless for lookups, since
   *  `CacheKey` compares by `sanitize`), and the first `SettleReaper` pass after
   *  each boot then "re-spelled" every one of them back to the real title: 1240 of
   *  1603 UK rows rewritten under byte-identical `_id`s per deploy (prod,
   *  2026-07-28), plus the change-stream and read-model fan-out behind them. Only
   *  a single-word ASCII title ("Interstellar") round-tripped `sanitize` and stayed
   *  quiet — which is why ~22% of the corpus never churned. */
  private def stitchSlots(r: StoredMovieRecord, storedSlots: Map[String, SourceData]): StoredMovieRecord =
    if (storedSlots.isEmpty) r
    else {
      val stitched = r.record.copy(data = SlotsRepository.merge(r.record.data, storedSlots))
      StoredMovieRecord.fromStorage(r.id.value, Some(r.key(normalizer)), stitched, normalizer)
    }

  /** Decode one stored row and re-inject its slots from `movie_slots` and its showtimes
   *  from `screenings` — the per-film read-stitch shared by [[findById]] and the
   *  change-stream fan-out. Slots first: the showtime stitch keys off the slot map.
   *
   *  `None` when the SLOT read failed. A migrated film's `movies` document holds no
   *  cinemas of its own, so a failed slot read would decode to a film with none at all —
   *  and this is precisely the record the change-stream fan-out hands the read-model
   *  projector, whose `diffScreenings` then deletes every `web_screening` the film has.
   *  Declining to produce a record costs one missed re-projection, which the film's next
   *  write repeats; producing an empty one empties a live film off the site. */
  private def decodeStitched(dto: StoredMovieDto): Option[StoredMovieRecord] = {
    val (storedSlots, slotsRead) = slots.map(_.findForFilmChecked(dto._id))
      .getOrElse((Map.empty[String, SourceData], true))
    if (!slotsRead) {
      logger.warn(s"MovieRepository: skipping ${dto._id} — its movie_slots read failed, and serving the row " +
        "without them would present a live film as having no cinemas.")
      None
    } else Some(stitchRow(StoredMovieDto.toDomain(dto, normalizer),
      screenings.map(_.findForFilm(dto._id)).getOrElse(Map.empty), storedSlots))
  }

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
  private lazy val initResult: (Option[MongoClient], Option[MongoDatabase], Option[MongoCollection[StoredMovieDto]]) =
    sharedDb match {
      case Some(db) =>
        val withRegistry = db.withCodecRegistry(MovieCodecs.registry)
        // Relaxed write concern (w:1, j:false): `movies` is re-scraped continuously,
        // so a write lost to a crash is recovered by the next scrape pass. Skipping
        // the journal sync cuts per-write cost on the shared-CPU Mongo — the worker's
        // write rate is what throttles it. Same trade `MongoTaskQueue` already makes.
        val coll = withRegistry.getCollection[StoredMovieDto](MovieRepository.Collection)
          .withWriteConcern(WriteConcern.W1.withJournal(false))
        ensureIndexes(coll)
        (None, Some(withRegistry), Some(coll))
      case None if fallbackToOwnInit => init()
      case None                      => (None, None, None)
    }
  private def clientOpt: Option[MongoClient]                     = initResult._1
  private def database:  Option[MongoDatabase]                   = initResult._2
  private def coll:      Option[MongoCollection[StoredMovieDto]] = initResult._3

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
  private def scanStitched(onBatch: Seq[StoredMovieRecord] => Unit, withShowtimes: Boolean = true): Boolean = {
    // Side rows are fetched PER PAGE, for exactly the films that page holds, rather than
    // preloaded whole. Both are one indexed `filmId $in [...]` query.
    //
    // `foreachRecord` promises its callers it never holds more than a page, and that
    // stopped being true when the bulk of a row moved into the side collections: this
    // preloaded both entire collections first (7.5 MB each on prod PL, more on UK) and
    // held them for the whole scan, on a worker with a 320 MB heap and an OOM history. Now
    // the peak is one page's films, whatever the corpus grows to.
    //
    // Completeness still gates the whole scan, not the page: a caller that PRUNES on a
    // row's absence must not act on a partial view, so one failed side read fails the scan
    // exactly as a failed `movies` batch does.
    var sideReadsComplete = true
    val moviesComplete = scanByKeyset { batch =>
      val ids = batch.map(_._id).toSet
      // `withShowtimes = false` skips this read entirely rather than discarding its result:
      // the caller has said it never looks at a showtime, and this is the expensive half.
      val (pageScr, scrOk) = if (!withShowtimes) (Map.empty[String, Map[String, Seq[Showtime]]], true)
        else screenings.map(_.findForFilmsChecked(ids))
          .getOrElse((Map.empty[String, Map[String, Seq[Showtime]]], true))
      val (pageSlots, slotsOk) = slots.map(_.findForFilmsChecked(ids))
        .getOrElse((Map.empty[String, Map[String, SourceData]], true))
      if (!scrOk || !slotsOk) {
        sideReadsComplete = false
        logger.warn(s"MovieRepository.scanStitched: a side-collection read failed for a page of " +
          s"${ids.size} film(s) (screenings ok=$scrOk, slots ok=$slotsOk) — treating the scan as " +
          "incomplete so a reconcile cannot prune films whose cinemas it could not see.")
      }
      onBatch(batch.map(dto => stitchRow(StoredMovieDto.toDomain(dto, normalizer),
        pageScr.getOrElse(dto._id, Map.empty), pageSlots.getOrElse(dto._id, Map.empty))))
    }
    moviesComplete && sideReadsComplete
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
      KeysetScan.scan[StoredMovieDto](
        label          = "MovieRepository keyset batch",
        batchSize      = findAllBatchSize,
        maxAttempts    = foreachRecordBatchAttempts,
        initialBackoff = foreachRecordBatchBackoff,
        keyOf          = _._id,
        fetchPage      = (afterId, limit) => {
          val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
          Await.result(
            c.find(filter).sort(Sorts.ascending("_id")).limit(limit).toFuture(), 60.seconds)
        },
        onIncomplete   = exception =>
          logger.warn(s"MovieRepository keyset scan failed after retries: " +
            s"${exception.getClass.getSimpleName}: ${exception.getMessage} — scan incomplete")
      )(onBatch)
    case None => false
  }

  /** Indexed single-document lookup by `_id` — the `/debug` lazy-details endpoint
   *  fetches one row's per-source breakdown when its table row is expanded.
   *  Mirrors [[findAll]]'s decode; an absent `_id` yields `None`. Best-effort:
   *  failures are logged, not thrown, and reported as `false` so a caller can tell an
   *  absent row from an unreadable one — see the trait doc for what conflating them
   *  costs. A row whose SLOT read failed counts as unreadable too: `decodeStitched`
   *  declines to build it, and that `None` means "could not look", not "no such film". */
  override def findByIdChecked(id: FilmId): (Option[StoredMovieRecord], Boolean) =
    findOneChecked(Filters.eq("_id", id.value), s"findById($id)")

  /** Indexed lookup by the `key` field — see the trait. */
  override def findByKeyChecked(key: CacheKey): (Option[StoredMovieRecord], Boolean) =
    findOneChecked(Filters.eq("key", StoredMovieRecord.keyFor(key)), s"findByKey(${StoredMovieRecord.keyFor(key)})")

  private def findOneChecked(filter: Bson, what: String): (Option[StoredMovieRecord], Boolean) = coll match {
    case Some(c) =>
      Try(Option(Await.result(c.find(filter).first().toFuture(), 10.seconds))) match {
        case scala.util.Success(None)      => (None, true)   // genuinely absent
        case scala.util.Success(Some(dto)) =>
          decodeStitched(dto) match {
            case some @ Some(_) => (some, true)
            case None           => (None, false)             // slots unreadable
          }
        case scala.util.Failure(exception) =>
          logger.warn(s"MovieRepository.$what failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          (None, false)
      }
    case None => (None, true)
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
        val rows = Await.result(c.aggregate[StoredMovieDto](pipeline).toFuture(), 60.seconds)
        // Stitch slots like every other reader. This one reads `movies.sourceData`
        // straight out of an aggregation, so a migrated film — whose slots have moved to
        // `movie_slots` — would otherwise list with NO cinemas at all. Showtimes stay
        // stripped: slots are stored without them, which is exactly what this path wants.
        // A failed slots load can't be recovered from here (the listing has no "partial"
        // shape to return), but it MUST NOT pass silently: every migrated film would
        // render cinema-less and the page would read as a corpus-wide outage. This one is
        // the dev /debug table, so it degrades loudly instead of refusing to render.
        val (allSlots, slotsRead) = slots.map(_.findAllChecked())
          .getOrElse((Map.empty[String, Map[String, SourceData]], true))
        if (!slotsRead)
          logger.warn("MovieRepository.findAllForListing: movie_slots load failed — every migrated film will " +
            "list with no cinemas. The listing is stale, not the corpus.")
        rows.map(dto => stitchSlots(StoredMovieDto.toDomain(dto, normalizer), allSlots.getOrElse(dto._id, Map.empty)))
      }.recover {
        case exception: Throwable =>
          logger.warn(s"MovieRepository.findAllForListing failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
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
    scanByKeyset(_.foreach(dto => f(StoredMovieDto.toDomain(dto, normalizer))))

  override def foreachRecordWithSlots(f: StoredMovieRecord => Unit): Boolean =
    scanStitched(_.foreach(f), withShowtimes = false)

  /** Remove the film `id` with its side-collection rows. */
  def delete(id: FilmId): Unit = coll.foreach { c =>
    Try {
      val deleted = Await.result(c.deleteOne(Filters.eq("_id", id.value)).toFuture(), 10.seconds).getDeletedCount
      if (deleted > 0) RemovalAudit.filmRemoved("movies.delete", id.value, reason = "by-id")
      screenings.foreach(_.deleteFilm(id.value))
      slots.foreach(_.deleteFilm(id.value))
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"MovieRepository.delete($id) failed: ${exception.getMessage}")
    }
  }

  /** The sweep is [[StrandedSideRows.sweep]]'s; this store only supplies the live `_id`
   *  set, read through the same keyset paging as every other corpus scan but projected
   *  to `_id` alone — a few thousand short strings, not the documents. `None` when a page
   *  still failed after its retries, which the sweep treats as "delete nothing". */
  override def deleteStrandedSideRows(): StrandedSideRows =
    StrandedSideRows.sweep(screenings, slots, liveIds = () => liveIdsChecked())

  private def liveIdsChecked(): Option[Set[String]] = coll.flatMap { c =>
    val ids = Set.newBuilder[String]
    val complete = KeysetScan.scan[String](
      label          = "MovieRepository id keyset batch",
      batchSize      = findAllBatchSize,
      maxAttempts    = foreachRecordBatchAttempts,
      initialBackoff = foreachRecordBatchBackoff,
      keyOf          = identity,
      fetchPage      = (afterId, limit) => {
        val filter = afterId.fold(Filters.empty())(Filters.gt("_id", _))
        Await.result(c.find[Document](filter).projection(Projections.include("_id"))
          .sort(Sorts.ascending("_id")).limit(limit).toFuture(), 60.seconds)
          .map(_("_id").asString.getValue)
      },
      onIncomplete   = exception =>
        logger.warn(s"MovieRepository id keyset scan failed after retries: " +
          s"${exception.getClass.getSimpleName}: ${exception.getMessage} — scan incomplete")
    )(ids ++= _)
    if (complete) Some(ids.result()) else None
  }

  /** Carry a film's screenings + slots across a merge, so the loser's rows don't stay
   *  stranded under an id that is about to be deleted. The read/verify/delete rule is
   *  [[SideCollectionMove]]'s, shared with the in-memory fake so a merge spec cannot pass
   *  against rules production doesn't follow. See the trait doc for what it cost. */
  override def moveFilm(oldFilm: FilmId, newFilm: FilmId): Boolean = if (oldFilm == newFilm) true else {
    val (oldId, newId) = (oldFilm.value, newFilm.value)
    val screeningsMoved = screenings.forall(s => SideCollectionMove.move[Seq[Showtime]](
      oldId, newId,
      read       = s.findForFilmChecked,
      replace    = (id, rows) => { s.replaceFilm(id, rows); true },
      deleteFilm = s.deleteFilm,
      onSkip     = message => logger.warn(s"merge $oldId -> $newId (screenings): $message."),
      onMoved    = moved => logger.info(s"merge $oldId -> $newId: carried $moved screenings slot(s) across.")))
    val slotsMoved = slots.forall(sl => SideCollectionMove.move[SourceData](
      oldId, newId,
      read       = sl.findForFilmChecked,
      replace    = (id, rows) => sl.replaceFilm(id, rows),
      deleteFilm = sl.deleteFilm,
      onSkip     = message => logger.warn(s"merge $oldId -> $newId (slots): $message.")))
    screeningsMoved && slotsMoved
  }

  def upsert(film: FilmId, cacheKey: CacheKey, e: MovieRecord): Unit = coll.foreach { c =>
    val id    = film.value
    val key   = StoredMovieRecord.keyFor(cacheKey)
    val title = cacheKey.cleanTitle
    val year  = cacheKey.year
    // A whole-record write can carry slots STRIPPED for the cache; `showtimesOf` would
    // drop them and `replaceFilm` would DELETE their screenings. Re-stitch first.
    // …and a re-stitch whose READ failed under-reports the film: every slot it could not
    // refill looks showtime-less, so the `replaceFilm` below would delete it.
    // `stitch.complete` carries that distinction down to the write, and `stitch.stored`
    // carries the read itself, so the write can tell an unchanged film from a changed one
    // without asking again.
    val stitch = screenings.fold(ScreeningsSplit.ReStitched(e.data, Map.empty, complete = true))(
      ScreeningsSplit.reStitchChecked(_, id, e.data))
    val restitched = stitch.data
    // Slots go FIRST, and `movies` only drops its embedded copy once they have actually
    // landed. Dropping it on a FAILED slot write would leave the film with no cinemas in
    // either place — the one way this migration can lose data — and a slots failure is
    // deliberately swallowed so it can't break the movies write, so the write itself has
    // to report back. A film whose slot write failed simply keeps the embedded map and is
    // retried on the next scrape.
    // Skip the write when the stored rows already match. `upsert` is the whole-record
    // path every scrape merge takes, and `replaceFilm` rewrites EVERY row of the film —
    // 471 of them for a film showing across the UK — so a scrape that changed only
    // showtimes would otherwise churn the entire slot set for nothing. One indexed read
    // replaces that; the film's screenings are already read here for `reStitch`, so this
    // is a second small read, not a new round-trip pattern.
    //
    // Already-matching counts as LANDED: the rows are correct, so the embedded copy is
    // still safe to drop. A failed read returns empty, which reads as "differs" and
    // writes — the safe direction.
    val slotPayload = SlotsRepository.slotsOf(restitched)
    val slotsLanded = slots.exists { s =>
      // ONE read, used twice. It answers "is anything different at all" (skip the write
      // entirely) and, handed on, "which rows are different" (write only those) — which
      // `replaceFilm` would otherwise have to go and ask again, on the hottest write path
      // in the system. A read that FAILED is passed as None, not as an empty map: empty
      // would read as "every row is new" and be trusted, where None makes `replaceFilm`
      // read for itself.
      //
      // The read is not fenced against a concurrent writer, so a slot that changes between
      // this read and the write can leave us skipping a row whose value happens to match
      // what we read. Reading here rather than inside `replaceFilm` widens that window by
      // the length of this method, and the price is one delayed row: the film's next scrape
      // reads again and writes it. Not worth a transaction on the hottest write path.
      SlotsRepository.applyFilm(s, id, slotPayload)
    }
    val now  = Instant.now()
    val opts = new ReplaceOptions().upsert(true)
    // The film document AS STORED, so a re-write that changes nothing can be skipped.
    //
    // `upsert` is the whole-record path every scrape merge takes, so each of a film's
    // venues wrote this document once per tick whether or not anything about the film had
    // changed. Mongo does not collapse that: a byte-identical `replaceOne` still reports
    // `modifiedCount: 1` and still writes an oplog entry — measured, not assumed. Each of
    // those entries is a change-stream delivery, and every delivery re-decodes the film
    // document and re-dispatches it downstream, which is the cost the read-split exists to
    // keep small.
    //
    // This is the third guard in this method and the only one that pays a round trip of its
    // own for the privilege: the screenings check reuses `reStitchChecked`'s read, the slots
    // check reuses nothing but reads a different collection. One indexed `_id` read to drop
    // a write, its oplog entry and its fanout is the same trade the slots guard already
    // makes here.
    //
    // Handed on as the `Try` it is: a read that FAILED and a document that is ABSENT both
    // write, but only one of them is "no such film", and the decision is where that is pinned.
    val stored = Try(Await.result(c.find(Filters.eq("_id", id)).limit(1).toFuture(), 10.seconds)).map(_.headOption)
    // What to write, and whether the stored document already equals it — the decision is
    // `MoviesUpsert`'s, so it is unit-tested apart from the three reads that feed it.
    val plan = MoviesUpsert.plan(id, key, e, restitched, slotsLanded, slotsForStorage, stored, now)
    Try {
      if (!plan.unchanged) Await.result(c.replaceOne(Filters.eq("_id", id), plan.document, opts).toFuture(), 10.seconds)
      // Write this film's cinema showtimes to `screenings` (their authority). `replaceFilm`
      // is upsert PLUS a delete of every slot the record doesn't name, so it may only run on
      // a record we know is complete. When the re-stitch read failed we still write what this
      // tick positively carries, but never the delete half — a slot we simply could not read
      // is not a slot that stopped screening.
      screenings.foreach { s =>
        val showtimes = ScreeningsSplit.showtimesOf(restitched)
        // Skip the whole call when the stored rows already match — the same guard the slots
        // write above has, and here it is FREE: `reStitchChecked` has already read these
        // rows, so `stitch.stored` costs no round trip where the slots half pays one.
        //
        // This is now the outer of TWO guards, and they answer different questions.
        // `replaceFilm` itself drops the rows that did not move (see `changedSlots`), which
        // is what stops one venue's change rewriting all 298 rows of a German release. This
        // one asks whether ANY row moved, and when none did it saves `replaceFilm` the read
        // it would need to find that out. `upsert` is the whole-record path EVERY scrape
        // merge takes and a film at N venues is written by N venues, so that read is worth
        // skipping on its own.
        //
        // Equality is safe against the delete vector: if the stored rows equal what we
        // would write, there is no slot for `replaceFilm` to prune. A differing read —
        // including an empty one — writes, which is the safe direction.
        // `stitch.stored` is this film's rows as they are NOW, already read above, and is handed
        // on rather than making `replaceFilm` read them a second time. The three-way choice lives
        // in `ScreeningsSplit.applyFilm` so the in-memory repository makes the same one.
        ScreeningsSplit.applyFilm(s, id, showtimes, stitch)
      }
      ()
    }.recover {
      case exception: Throwable if isClusterClosed(exception) =>
        // Shutdown race — the lifecycle closed the MongoClient while a worker
        // was still mid-write. Harmless: the in-memory cache already has the
        // value and the next refresh will persist it.
        logger.debug(s"MovieRepository.upsert($title, $year) skipped — Mongo client closing.")
      case exception: Throwable if isDuplicateKey(exception) =>
        logger.warn(s"MovieRepository.upsert($title, $year) refused: another document already holds its " +
          s"key or its tmdbId=${e.tmdbId.getOrElse("?")} — the film keeps its previous document (${exception.getMessage})")
      case exception: Throwable =>
        logger.warn(s"MovieRepository.upsert($title, $year) failed: ${exception.getMessage}")
    }
  }

  def updateIfPresent(film: FilmId, cacheKey: CacheKey, before: MovieRecord, after: MovieRecord): Boolean = coll match {
    case None => false
    case Some(c) =>
      val id    = film.value
      val key   = StoredMovieRecord.keyFor(cacheKey)
      val title = cacheKey.cleanTitle
      val year  = cacheKey.year
      // Showtime deltas → `screenings` (its authority under the split); from the
      // ORIGINAL records. Only when a screenings repo is wired.
      val ops = if (screenings.isDefined) ScreeningsSplit.slotOps(before.data, after.data)
                else Map.empty[String, Option[Seq[Showtime]]]
      // Slot deltas → `movie_slots` (dual write). Also from the ORIGINAL records:
      // `slotsOf` drops showtimes itself, so a showtimes-only change yields no slot
      // write and the two side collections stay independent.
      val slotWrites = if (slots.isDefined) SlotsRepository.slotOps(before.data, after.data)
                       else Map.empty[String, Option[SourceData]]
      // Movies patch from the (split-stripped) records, so a showtimes-only change
      // yields an EMPTY movies patch — movies stays put, no fat change event. When
      // both are empty the row already equals `after`: skip the write (and its no-op
      // `$set` + change event). "Present and up to date" is still success.
      val strippedAfter = after.copy(data = slotsForStorage(after.data))
      // With the slots split on, `movies` is not where slots live, so the patch must not
      // carry them: `upsert` drops the embedded map once the slots land, and a later patch
      // that still wrote `sourceData.<slot>` would resurrect it field by field and undo
      // exactly the shrink this split exists for. Dropping it here means the embedded map
      // simply goes stale — reads prefer `movie_slots` whenever a film has rows there, and
      // fall back to that stale copy only for a film with none, which is the same
      // already-correct value it had before.
      val rawPatch = MovieRecordPatch.diff(before.copy(data = slotsForStorage(before.data)), strippedAfter)
      val patch    = if (slots.isDefined) rawPatch.copy(data = Map.empty) else rawPatch
      if (patch.isEmpty && ops.isEmpty && slotWrites.isEmpty) true
      else Try {
        // MongoDB update-operator paths treat '.' as a nesting separator, so a
        // per-source `$set` on `sourceData.<displayName>` is rejected when a source's
        // displayName has a dot ("Helios Ostrów Wlkp."); fall back to a conditional
        // full-document replace there. `None` movies patch = a side-collection-only change:
        // `screenings` and `movie_slots` each have a cursor of their own on the same fan-out
        // (see `MovieChangeStream`), so `movies` is left alone. A slots-only change used to
        // TOUCH `movies` anyway — a one-field `updatedAt` bump standing in for the watcher
        // `movie_slots` did not have — and once it had one that bump fanned the same change
        // out twice, which is the double projection the old arrangement existed to avoid.
        val moviesMatched: Option[Long] =
          if (patch.isEmpty) None
          else Some(
            if (patch.data.keysIterator.exists(_.displayName.contains('.'))) {
              // Can't drive the field-level diff (the dotted `$set` path is rejected), so
              // replace the whole document. Writing `strippedAfter` (built from the
              // in-memory cache row) BLINDLY would NULL any Mongo-owned field the cache
              // lacks — a rating not yet rehydrated after a restart, or an out-of-band edit
              // (FilmwebUrlAudit). Read the current doc and apply the SAME patch to it, so
              // the replace carries exactly the diff the `$set` path would, every other
              // field preserved. Absent row → nothing to replace → report not-present.
              val current = Option(Await.result(c.find(Filters.eq("_id", id)).first().toFuture(), 10.seconds))
                .map(dto => StoredMovieDto.toDomain(dto, normalizer).record)
              dottedReplaceRecord(current, patch) match {
                case Some(merged) =>
                  Await.result(c.replaceOne(Filters.eq("_id", id),
                    StoredMovieDto.fromDomain(id, key, merged, Instant.now()),
                    new ReplaceOptions().upsert(false)).toFuture(), 10.seconds).getMatchedCount
                case None => 0L
              }
            } else {
              Await.result(c.updateOne(Filters.eq("_id", id), patchToUpdate(patch), new UpdateOptions().upsert(false)).toFuture(), 10.seconds)
                .getMatchedCount
            })
        // Present when the movies write matched, OR a side-collection-only change (no movies
        // write, None); false only on Some(0) — the row is absent, so don't apply the
        // side-collection deltas (no orphan rows) and report not-present.
        val present = moviesMatched.forall(_ > 0)
        if (present) screenings.foreach { s =>
          ops.foreach {
            case (k, Some(st)) => s.upsertSlot(id, k, st)
            case (k, None)     => s.deleteSlot(id, k)
          }
        }
        if (present) slots.foreach { s =>
          slotWrites.foreach {
            case (k, Some(sd)) => s.upsertSlot(id, k, sd)
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

  // Translate a `MovieRecordPatch` into a `$set`/`$unset` Mongo update. Each
  // scalar field gets its own atom; the `data` map gets per-source
  // `sourceData.<sourceName>` paths so a Tmdb-only refresh doesn't touch a
  // cinema's slot and vice versa. `updatedAt` bumps alongside the real change — and
  // under the slots split that bump is sometimes the ONLY atom: a slots-only change
  // reaches here with an empty patch precisely so the write fires a change event on the
  // channel the projector listens to. `updateIfPresent` still skips a patch that is empty
  // with nothing else to announce.
  /** `private[movies]` so `MovieRecordFieldWiringSpec` can assert that every field the
   *  patch carries actually reaches the WIRE. A field present in `MovieRecordPatch` but
   *  missing here passes `applyTo` — and therefore every in-memory-repository test —
   *  while Mongo never receives it. */
  private[movies] def patchToUpdate(p: MovieRecordPatch): Bson = {
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
    scalar("tmdbBasis",         p.tmdbBasis,         (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("wikidataId",        p.wikidataId,        (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("metacriticUrl",     p.metacriticUrl,     (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("rottenTomatoesUrl", p.rottenTomatoesUrl, (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("searchTitle",       p.searchTitle,       (s: String) => new org.mongodb.scala.bson.BsonString(s))
    scalar("tmdbAttempt",       p.tmdbAttempt,       (a: services.resolution.TmdbAttempt) =>
      new org.bson.BsonDocument("evidence", new org.mongodb.scala.bson.BsonString(a.evidence))
        .append("at", BsonDateTime(a.at.toEpochMilli)))
    scalar("detailPending",     p.detailPending,     (b: Boolean) => new org.mongodb.scala.bson.BsonBoolean(b))
    scalar("retainedSynopses", p.retainedSynopses, (m: Map[Source, String]) => {
      val doc = new org.bson.BsonDocument()
      m.foreach { case (source, synopsis) => doc.put(source.displayName, new org.mongodb.scala.bson.BsonString(synopsis)) }
      doc
    })
    p.data.foreach {
      case (source, FieldUpdate.SetTo(sd)) => atoms += Updates.set(s"sourceData.${source.displayName}", sd)
      case (source, FieldUpdate.Unset)     => atoms += Updates.unset(s"sourceData.${source.displayName}")
      case (_, FieldUpdate.NoChange)       => ()
    }
    atoms += Updates.set("updatedAt", BsonDateTime(Instant.now().toEpochMilli))
    Updates.combine(atoms.toSeq*)
  }

  // The `movies` change-stream subscription — shared cursor, fan-out, backpressure,
  // resume token, reopen — lives in [[MovieChangeStream]]; this class only lends it the
  // stitched decode and the by-id re-read. None on a disabled store, so `watchChanges`
  // returns None and the caller relies on its periodic backstop.
  private val changeStream: Option[MovieChangeStream] = coll.map { c =>
    new MovieChangeStream(
      source              = MovieChangeStream.Source.ofCollection(c),
      screenings          = screenings,
      slots               = slots,
      decode              = decodeStitched,
      reread              = id => findById(FilmId(id)),
      // The shared cursor reopens (after a terminal error, and — the big win — after a WORKER
      // RESTART) from the last-seen token instead of "now", REPLAYING writes that landed while
      // this process was down — the gap the consumers' periodic backstops exist for. See
      // [[ChangeStreamResumeToken]]; the `screenings` and `movie_slots` streams each persist
      // their own sibling token.
      resumeToken         = new ChangeStreamResumeToken("movies", database, persistResumeToken),
      changeStreamMetrics = changeStreamMetrics,
      screeningsMetrics   = screeningsMetrics,
      slotsMetrics        = slotsMetrics,
      changeDemandWindow  = changeDemandWindow)
  }

  /** Change events handed to the apply thread but not yet applied — see
   *  [[MovieChangeStream.applyBacklog]]; 0 on a disabled store. */
  def changeApplyBacklog: Int = changeStream.fold(0)(_.applyBacklog)

  override def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = changeStream.map(_.watch(onUpsert, id => onDelete(FilmId(id))))

  /** Whether the single shared change-stream cursor is currently running — for
   *  diagnostics/tests (it starts on the first listener, stops after the last). */
  def isWatchingChangeStream: Boolean = changeStream.exists(_.isWatching)

  override def changeStreamLiveness: ChangeStreamLiveness =
    changeStream.fold(super.changeStreamLiveness)(_.liveness)

  def close(): Unit = {
    changeStream.foreach(_.close())
    clientOpt.foreach(_.close())
  }

  /** The `key` index and its one-time backfill. Idempotent + best-effort: a re-create is
   *  a no-op, a failure only logs. (The `(title, year)` index that used to serve
   *  `delete`'s column-matching fallback is not created any more — nothing queries
   *  those columns; the index left on prod is inert.) */
  private def ensureIndexes(coll: MongoCollection[StoredMovieDto]): Unit = {
    // The lookup key lives in its own field now that `_id` is the permanent `FilmId`
    // (see [[FilmId]]). A document written before then has no `key` — its `_id` IS its
    // key — so backfill it once, here, before anything queries by key: the staging fold's
    // sanitize-group read and the cache's cold lookup both filter on `key`. Idempotent
    // (only documents lacking the field) and a pipeline update, so one round trip.
    Try {
      val backfilled = Await.result(coll.updateMany(Filters.exists("key", false),
        Seq(org.mongodb.scala.bson.collection.immutable.Document("$set" -> org.mongodb.scala.bson.collection.immutable.Document("key" -> "$_id")))).toFuture(), 60.seconds)
      if (backfilled.getModifiedCount > 0)
        logger.info(s"movies: backfilled `key` from `_id` on ${backfilled.getModifiedCount} document(s) written before film ids.")
    }.recover {
      case exception: Throwable => logger.warn(s"movies `key` index/backfill failed: ${exception.getMessage}")
    }
    // One document per film: `tmdbId` unique over the documents that HAVE one. The
    // write-time fold (`MovieCache.put`) already merges a duplicate before it is written;
    // this is the store refusing the one that slips past a race, so the settle never has
    // a same-tmdbId pair to merge. A refused write is logged by `upsert` and the film
    // keeps its previous document until the next scrape writes it again. Measured
    // 2026-09-06: zero duplicate tmdbIds in any country, so the index builds clean.
    //
    // PARTIAL on `tmdbId` being a number, not SPARSE: the codec writes an absent option
    // as `tmdbId: null`, and a sparse index indexes null as a value — every unresolved
    // row would collide on it (found by the integration suite, not production).
    // `createIndex` cannot alter an existing index's options, so a conflicting earlier
    // definition is dropped and rebuilt — the same rule `MongoTtlIndex.reconcile` follows.
    uniqueIndex(coll, "tmdbId", new IndexOptions().unique(true).background(true)
      .partialFilterExpression(org.mongodb.scala.bson.collection.immutable.Document(
        "tmdbId" -> org.mongodb.scala.bson.collection.immutable.Document("$type" -> "number"))))
    // One document per stored key: the key is the lookup identity now, and the cache
    // refuses a second row under a key another film holds (`MovieCache.persist`); this
    // is the store refusing the one a race lets through. Measured 2026-09-07: zero
    // duplicate keys in any country, so the index builds clean. Every document has
    // the field after the backfill above, so plain unique — no partial filter.
    uniqueIndex(coll, "key", new IndexOptions().unique(true).background(true))
  }

  /** Create a unique index on `field`, rebuilding it when an earlier definition of the
   *  same name carries different options: `createIndex` cannot alter an existing
   *  index (IndexOptionsConflict, code 85), so the old one is dropped first — the
   *  same rule `MongoTtlIndex.reconcile` follows. A failure is logged, never fatal:
   *  the film store works without the index, it merely stops refusing duplicates. */
  private def uniqueIndex(coll: MongoCollection[StoredMovieDto], field: String, options: IndexOptions): Unit =
    Try {
      def create() = Await.result(coll.createIndex(Indexes.ascending(field), options).toFuture(), 30.seconds)
      Try(create()).recover {
        case exception: com.mongodb.MongoCommandException if exception.getErrorCode == 85 =>   // IndexOptionsConflict
          Await.result(coll.dropIndex(s"${field}_1").toFuture(), 30.seconds)
          create()
      }.get
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"movies unique `$field` index creation failed: ${exception.getMessage}")
    }

  /** Mongo's duplicate-key error — a second document claiming a tmdbId the unique index
   *  already holds. */
  private def isDuplicateKey(exception: Throwable): Boolean =
    exception.isInstanceOf[com.mongodb.MongoWriteException] &&
      exception.asInstanceOf[com.mongodb.MongoWriteException].getError.getCategory == com.mongodb.ErrorCategory.DUPLICATE_KEY

  private def init(): (Option[MongoClient], Option[MongoDatabase], Option[MongoCollection[StoredMovieDto]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MongoMovieRepository disabled (in-memory cache only).")
        (None, None, None)
      case Some(uri) =>
        Try {
          val dbName  = models.Country.resolvedDbName
          val client  = MongoClient(uri)
          val db      = client.getDatabase(dbName).withCodecRegistry(MovieCodecs.registry)
          // Relaxed write concern — see the sharedDb path above.
          val coll    = db.getCollection[StoredMovieDto](MovieRepository.Collection)
            .withWriteConcern(WriteConcern.W1.withJournal(false))
          // Touch the collection to surface connectivity errors at startup,
          // not on the first read after the app is "up".
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          ensureIndexes(coll)
          logger.info(s"MongoMovieRepository connected to $dbName.movies")
          (client, db, coll)
        }.recover {
          case exception: Throwable =>
            logger.error(s"MongoMovieRepository init failed (${exception.getMessage}) — falling back to in-memory cache.")
            null
        }.toOption.filter(_ != null) match {
          case Some((c, db, coll)) => (Some(c), Some(db), Some(coll))
          case None                => (None, None, None)
        }
    }

  // The driver throws IllegalStateException("state should be: open") from
  // BaseCluster / DefaultConnectionPool once MongoClient.close() has fired.
  private def isClusterClosed(exception: Throwable): Boolean =
    Option(exception.getMessage).exists(_.contains("state should be: open"))

}
