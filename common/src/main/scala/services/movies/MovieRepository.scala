package services.movies

import com.mongodb.WriteConcern
import com.mongodb.client.model.{ReplaceOptions, UpdateOptions}
import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import models.{MovieRecord, Showtime, Source, SourceData}
import org.mongodb.scala.bson.{BsonDateTime, BsonNull}
import org.mongodb.scala.model.{Aggregates, Filters, IndexOptions, Indexes, Sorts, Updates}
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase, Observer, ObservableFuture, SingleObservableFuture, Subscription}
import org.bson.conversions.Bson
import play.api.Logging
import tools.Env

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
  def idFor(title: String, year: Option[Int], normalizer: TitleNormalizer): String =
    s"${normalizer.sanitize(title)}|${year.map(_.toString).getOrElse("")}"

  /** The same `_id`, for a caller that already holds the key. A [[CacheKey]]
   *  carries the normalised form it was BUILT with, so this needs no normalizer
   *  and cannot re-derive a different one: re-sanitizing `k.cleanTitle` with
   *  today's rules would silently disagree with the key's own identity if the
   *  two rule sets ever differed. Most callers are in this shape. */
  def idFor(k: CacheKey): String =
    s"${k.normalized}|${k.year.map(_.toString).getOrElse("")}"

  /** The `_id` of a stored row. Prefers the actual `persistedId` over re-deriving
   *  `idFor(title, year)`: the display `title` is derived from `sourceData`, so a
   *  clean doc whose cinema reports the title WITH the year baked in (e.g.
   *  "Zabriskie Point (1970)") re-sanitizes to a DIFFERENT prefix than its `_id`
   *  — colliding with whatever doc that prefix actually belongs to. Two distinct
   *  Mongo documents then render the same `data-id` and the /debug live view's
   *  first-match DOM lookup opens whichever row is first. The persisted `_id` is
   *  unique by construction, so keying on it keeps the rows independent. */
  def idOf(row: StoredMovieRecord, normalizer: TitleNormalizer): String =
    row.persistedId.getOrElse(idFor(row.title, row.year, normalizer))

  /** Rebuild a stored row from its persisted `_id` and `MovieRecord`, deriving
   *  the display `title` and `year` rather than reading pinned columns — used by
   *  the Mongo codec (`MovieCodecs.toDomain`), whose BSON drops the `title`/
   *  `year` fields. The `_id` is `sanitize(title)|year`: `sanitize` never emits
   *  `|`, so the suffix is the year and the prefix is the cache key's sanitized
   *  form. Every spelling in a row sanitizes to that prefix (the `CacheKey`
   *  identity), so `displayTitle(prefix)` sanitizes back to it — the rebuilt key
   *  recomputes to the same `_id`, no re-keying churn. (The in-memory repository keeps
   *  the full record in memory and returns its title verbatim, so it needs no
   *  recovery step; for realistic rows the two agree.)
   *
   *  CALL IT WITH A COMPLETE RECORD. `displayTitle` names the film from its SLOTS,
   *  so a record whose slots have not been stitched back in yet has nothing to name
   *  it with and falls through to the `_id` prefix — a real title only by accident
   *  ("Interstellar"), otherwise a mangled "Thecabinetofdrcaligari". That is exactly
   *  the state `MovieCodecs.toDomain` decodes into now that the slots live in
   *  `movie_slots`, which is why `MongoMovieRepository.stitchSlots` calls this again
   *  once the record is whole. */
  def fromStorage(id: String, record: MovieRecord, normalizer: TitleNormalizer): StoredMovieRecord = {
    val sep      = id.lastIndexOf('|')
    val idPrefix = if (sep >= 0) id.substring(0, sep) else id
    val year     = if (sep >= 0) id.substring(sep + 1).toIntOption else None
    StoredMovieRecord(record.displayTitle(idPrefix, normalizer), year, record, persistedId = Some(id))
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
   *  The stripped shape loses nothing: under the split `ScreeningsRepository.stitch`
   *  treats `screenings` as authoritative and empties the showtimes of any slot it
   *  has no row for, so an embedded board is discarded on the way back out. It is
   *  weight the reader was already throwing away.
   *
   *  `final` because the rule is the storage contract, not an implementation
   *  detail — a repository that answered differently would be a repository whose
   *  documents mean something else. */
  final def slotsForStorage(data: Map[Source, SourceData]): Map[Source, SourceData] =
    if (hasScreenings) ScreeningsRepository.stripShowtimes(data) else data

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
  def findById(id: String): Option[StoredMovieRecord] = findByIdChecked(id)._1

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
  def findByIdChecked(id: String): (Option[StoredMovieRecord], Boolean) = {
    (findAll().find(row => StoredMovieRecord.idOf(row, normalizer) == id), true)
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

  /** Move a film's SIDE-COLLECTION rows (`screenings`, `movie_slots`) from one document
   *  id to another — the re-key.
   *
   *  A re-key is a rename, not a departure: `foo|` becomes `foo|2026` the moment TMDB
   *  concludes the year, and the film keeps screening throughout. But its showtimes are
   *  filed under the OLD id, and nothing else moves them — `upsert` re-stitches from the
   *  id it is writing TO, so at the new id it finds nothing and stores nothing, while the
   *  old id is deleted with the old row. The showtimes are destroyed in between.
   *
   *  That is not hypothetical: the 30-minute `SettleReaper` re-keys continuously, and on
   *  2026-07-27 prod shed ~10k upcoming showtimes per cycle in PL alone, films left
   *  intact, rebuilt only by the next scrape — the sawtooth this method exists to end.
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
  def moveFilm(oldId: String, newId: String): Boolean = true

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
  override val normalizer: TitleNormalizer
) extends MovieRepository with Logging {


  override def hasScreenings: Boolean = screenings.isDefined
  override def hasSlots:       Boolean = slots.isDefined


  /** Re-inject a stored row's showtimes from `screenings` (its authority under the
   *  split), given that film's `slotKey -> showtimes` map. No-op without a split. */
  private def stitchRow(r: StoredMovieRecord, scr: Map[String, Seq[Showtime]],
                        storedSlots: Map[String, SourceData] = Map.empty): StoredMovieRecord = {
    val withSlots = stitchSlots(r, storedSlots)
    if (screenings.isEmpty) withSlots
    else withSlots.copy(record = withSlots.record.copy(
      data = ScreeningsRepository.stitch(withSlots.record.data, scr)))
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
      r.persistedId.fold(r.copy(record = stitched))(StoredMovieRecord.fromStorage(_, stitched, normalizer))
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
  private def scanStitched(onBatch: Seq[StoredMovieRecord] => Unit): Boolean = {
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
      val (pageScr, scrOk) = screenings.map(_.findForFilmsChecked(ids))
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
  override def findByIdChecked(id: String): (Option[StoredMovieRecord], Boolean) = coll match {
    case Some(c) =>
      Try(Option(Await.result(c.find(Filters.eq("_id", id)).first().toFuture(), 10.seconds))) match {
        case scala.util.Success(None)      => (None, true)   // genuinely absent
        case scala.util.Success(Some(dto)) =>
          decodeStitched(dto) match {
            case some @ Some(_) => (some, true)
            case None           => (None, false)             // slots unreadable
          }
        case scala.util.Failure(exception) =>
          logger.warn(s"MovieRepository.findById($id) failed: ${exception.getClass.getSimpleName}: ${exception.getMessage}")
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
      if (result.getDeletedCount > 0)
        RemovalAudit.filmRemoved("movies.delete", documentId(title, year),
          reason = if (result.getDeletedCount > 1) s"title+year (${result.getDeletedCount} docs)" else "title+year")
      screenings.foreach(_.deleteFilm(documentId(title, year)))
      slots.foreach(_.deleteFilm(documentId(title, year)))
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"MovieRepository.delete($title, $year) failed: ${exception.getMessage}")
    }
  }

  def deleteById(id: String): Unit = coll.foreach { c =>
    Try {
      val deleted = Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds).getDeletedCount
      if (deleted > 0) RemovalAudit.filmRemoved("movies.deleteById", id, reason = "orphan-id-reap")
      screenings.foreach(_.deleteFilm(id))
      slots.foreach(_.deleteFilm(id))
      ()
    }.recover {
      case exception: Throwable => logger.warn(s"MovieRepository.deleteById($id) failed: ${exception.getMessage}")
    }
  }

  /** Carry a film's screenings + slots across a re-key, so the rename doesn't strand them
   *  under an id that is about to be deleted. The read/verify/delete rule is
   *  [[SideCollectionMove]]'s, shared with the in-memory fake so a re-key spec cannot pass
   *  against rules production doesn't follow. See the trait doc for what it cost. */
  override def moveFilm(oldId: String, newId: String): Boolean = if (oldId == newId) true else {
    val screeningsMoved = screenings.forall(s => SideCollectionMove.move[Seq[Showtime]](
      oldId, newId,
      read       = s.findForFilmChecked,
      replace    = (id, rows) => { s.replaceFilm(id, rows); true },
      deleteFilm = s.deleteFilm,
      onSkip     = message => logger.warn(s"re-key $oldId -> $newId (screenings): $message."),
      onMoved    = moved => logger.info(s"re-key $oldId -> $newId: carried $moved screenings slot(s) across.")))
    val slotsMoved = slots.forall(sl => SideCollectionMove.move[SourceData](
      oldId, newId,
      read       = sl.findForFilmChecked,
      replace    = sl.replaceFilm,
      deleteFilm = sl.deleteFilm,
      onSkip     = message => logger.warn(s"re-key $oldId -> $newId (slots): $message.")))
    screeningsMoved && slotsMoved
  }

  def upsert(title: String, year: Option[Int], e: MovieRecord): Unit = coll.foreach { c =>
    val id   = documentId(title, year)
    // A whole-record write can carry slots STRIPPED for the cache; `showtimesOf` would
    // drop them and `replaceFilm` would DELETE their screenings. Re-stitch first.
    // …and a re-stitch whose READ failed under-reports the film: every slot it could not
    // refill looks showtime-less, so the `replaceFilm` below would delete it.
    // `stitch.complete` carries that distinction down to the write, and `stitch.stored`
    // carries the read itself, so the write can tell an unchanged film from a changed one
    // without asking again.
    val stitch = screenings.fold(ScreeningsRepository.ReStitched(e.data, Map.empty, complete = true))(
      ScreeningsRepository.reStitchChecked(_, id, e.data))
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
      if (s.findForFilm(id) == slotPayload) true else s.replaceFilm(id, slotPayload)
    }
    // Under the read-split `movies` carries no showtimes (they go to `screenings`), and
    // once the slots have landed it carries no sourceData either — which is what shrinks
    // the document the change stream re-decodes on every write.
    val dataForMovies = if (slotsLanded) Map.empty[Source, SourceData] else slotsForStorage(restitched)
    val dto  = StoredMovieDto.fromDomain(id, e.copy(data = dataForMovies), Instant.now())
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
    val storedDto = Try(Await.result(c.find(Filters.eq("_id", id)).limit(1).toFuture(), 10.seconds))
      .toOption.flatMap(_.headOption)
    // Both timestamps are normalised away before comparing. `updatedAt` is stamped
    // `Instant.now()` on every call, so comparing it would make every document differ and
    // the guard dead on arrival. `slotsUpdatedAt` is subtler: `fromDomain` never sets it,
    // but `updateIfPresent`'s slot path does — so leaving it in would make the guard miss
    // every film whose slots had ever been patched, which is most of them. Skipping the
    // write PRESERVES the stored marker rather than clearing it, which is the harmless
    // direction: it only helps the change stream classify a later write.
    //
    // A read that FAILED yields None, which reads as "changed" and writes. A failed read is
    // not evidence that the stored document matches.
    val unchanged = storedDto.exists(stored =>
      stored.copy(updatedAt = dto.updatedAt, slotsUpdatedAt = dto.slotsUpdatedAt) == dto)
    Try {
      if (!unchanged) Await.result(c.replaceOne(Filters.eq("_id", id), dto, opts).toFuture(), 10.seconds)
      // Write this film's cinema showtimes to `screenings` (their authority). `replaceFilm`
      // is upsert PLUS a delete of every slot the record doesn't name, so it may only run on
      // a record we know is complete. When the re-stitch read failed we still write what this
      // tick positively carries, but never the delete half — a slot we simply could not read
      // is not a slot that stopped screening.
      screenings.foreach { s =>
        val showtimes = ScreeningsRepository.showtimesOf(restitched)
        // Skip the rewrite when the stored rows already match — the same guard the slots
        // write above has, and here it is FREE: `reStitchChecked` has already read these
        // rows, so `stitch.stored` costs no round trip where the slots half pays one.
        //
        // It is worth having twice over. `replaceFilm` is one request but carries a
        // `ReplaceOneModel` per slot of the film — 471 for a film showing across the UK —
        // and `upsert` is the whole-record path EVERY scrape merge takes. So a venue's
        // scrape rewrote every screening row of every film it touched, whether or not a
        // showtime had moved, and a film at N venues is written by N venues.
        //
        // Equality is safe against the delete vector: if the stored rows equal what we
        // would write, there is no slot for `replaceFilm` to prune. A differing read —
        // including an empty one — writes, which is the safe direction.
        if (!stitch.complete) showtimes.foreach { case (slotKey, st) => s.upsertSlot(id, slotKey, st) }
        else if (showtimes != stitch.stored) s.replaceFilm(id, showtimes)
      }
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
        // full-document replace there. `None` movies patch = a screenings-only change.
        // A slots-only change must still TOUCH `movies`. That document is the single
        // change-notification channel the projector listens on, and `movie_slots`
        // deliberately has no watcher of its own: a second stream would fan out a second
        // time for one logical change and double the projections. `patchToUpdate` always
        // bumps `updatedAt`, so an otherwise-empty patch becomes exactly the one-field
        // write that fires one event — the same count as before the split, not one more.
        val moviesMatched: Option[Long] =
          if (patch.isEmpty && slotWrites.isEmpty) None
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
                    StoredMovieDto.fromDomain(id, merged, Instant.now()),
                    new ReplaceOptions().upsert(false)).toFuture(), 10.seconds).getMatchedCount
                case None => 0L
              }
            } else {
              // Mark the write as a SLOT change, not a bare `updatedAt` bump. Without this
              // the change stream cannot tell it from the no-op writes `updated_at_only`
              // exists to catch, and the split would silently retire that canary while
              // driving `source_data` to zero on the dashboard.
              val update =
                if (slotWrites.isEmpty) patchToUpdate(patch)
                else Updates.combine(patchToUpdate(patch),
                  Updates.set("slotsUpdatedAt", BsonDateTime(Instant.now().toEpochMilli)))
              Await.result(c.updateOne(Filters.eq("_id", id), update, new UpdateOptions().upsert(false)).toFuture(), 10.seconds)
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
   *  disappears). The driver auto-resumes across transient blips; a TERMINAL
   *  error is reopened on a backoff by [[ChangeStreamReopen]]. Requires a replica
   *  set (a single-node RS counts); on a standalone Mongo the stream errors out
   *  and the caller falls back to its backstop. */
  // ONE shared change-stream cursor feeds every registered listener through this
  // fan-out, rather than a cursor per caller. The worker attaches two consumers
  // (MovieCache + ReadModelProjector); a cursor-per-caller decoded every write
  // twice, and a profiler showed that async change-stream I/O completion was the
  // worker's dominant CPU cost. Decode once here, dispatch to all. The cursor
  // starts on the first listener and stops when the last one detaches.
  private val movieChanges = new ChangeStreamFanout[StoredMovieRecord]("MovieRepository")
  private val changeSub    = new AtomicReference[Subscription]()
  private val changeLock   = new AnyRef
  // Applies change-stream events OFF the Mongo driver's Netty I/O event loops. The
  // apply does a blocking stitch read (`decodeStitched` / `findById`) plus the
  // synchronized read-model projection; running that on the I/O loops made the two
  // loops contend the projection monitor and busy-spin their wakeup eventfds (~24cc,
  // ~0 voluntary ctx-switches — proven on-box), flooring the shared-CPU credit. A
  // SINGLE thread keeps events applied strictly in order.
  private val changeApply  = tools.DaemonExecutors.singleThreadExecutor("movie-change-apply")
  // Read-split only: a second cursor on `screenings`. A showtime change writes only
  // `screenings` (movies stays put), so without this the projector would never see it.
  private val screeningsWatch = new AtomicReference[Option[AutoCloseable]](None)

  // The shared cursor reopens (after a terminal error, and — the big win — after a WORKER
  // RESTART) from the last-seen token instead of "now", REPLAYING writes that landed while
  // this process was down — the gap the consumers' periodic backstops exist for. See
  // [[ChangeStreamResumeToken]]; the `screenings` stream persists its own sibling token.
  private val resumeToken = new ChangeStreamResumeToken("movies", database, persistResumeToken)

  // A change stream's onError is TERMINAL — nothing brings the cursor back on its own, and
  // `ensureWatching` only runs on REGISTRATION, which the worker does twice at boot and never
  // again. Without this driver one terminal error killed the worker's stream until the process
  // restarted (see [[ChangeStreamReopen]] for the outage that proved it). Skips the reopen once
  // the last listener has detached, so an idle repository stays idle.
  private val changeReopen = ChangeStreamReopen.onDaemonScheduler("MovieRepository",
    () => if (!movieChanges.isEmpty) coll.foreach(ensureWatching))

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
   *  surfaced by `_id`. A terminal error clears the subscription and schedules a
   *  reopen on a backoff (a later registration re-opens too), and existing listeners
   *  fall back to their periodic backstop (cache rehydrate / projector reconcile)
   *  meanwhile. */
  private def ensureWatching(c: MongoCollection[StoredMovieDto]): Unit = changeLock.synchronized {
    if (changeSub.get() == null) {
      // Resume from the last persisted token if we have one (a restart / prior terminal
      // error) so events missed while down are replayed; else open at "now".
      val resumeFrom = resumeToken.load()
      val base       = c.watch().fullDocument(FullDocument.UPDATE_LOOKUP)
      resumeFrom.fold(base)(t => base.resumeAfter(Document(t)))
        .subscribe(new Observer[ChangeStreamDocument[StoredMovieDto]] {
          override def onSubscribe(s: Subscription): Unit = { changeSub.set(s); s.request(Long.MaxValue) }
          override def onNext(change: ChangeStreamDocument[StoredMovieDto]): Unit = {
            changeReopen.opened() // a delivered event is what proves the cursor healthy — reset the backoff
            recordChangeMetrics(change)
            // Advance the resume position BEFORE fanning out, so a consumer signal (a
            // downstream latch / write) can never observe an event before the token moves.
            // This stays on the I/O thread (a cheap atomic set) to preserve that ordering.
            resumeToken.advance(change.getResumeToken)
            val fullDocument = Option(change.getFullDocument)
            val deletedId    = Option(change.getDocumentKey).flatMap(k => Option(k.get("_id")))
              .map(v => if (v.isString) v.asString.getValue else v.toString)
            // Apply OFF the Netty I/O loop: the stitch read + projection must not run
            // there (they made the loops contend + spin — see `changeApply`).
            changeApply.execute { () =>
              fullDocument match {
                // The movies doc has no showtimes — stitch them back from `screenings`
                // (via decodeStitched) before fanning out, so consumers get a full row.
                // A failed slot read yields None and we fan out NOTHING: an empty-cinema
                // record here is what the projector turns into a screenings wipe.
                case Some(dto) => decodeStitched(dto).foreach(movieChanges.dispatchUpsert)
                // No post-image ⇒ a delete (the only op UPDATE_LOOKUP can't back-fill).
                // Surface its _id so consumers can drop the row.
                case None      => deletedId.foreach(movieChanges.dispatchDelete)
              }
              // Persist the advanced position (time-throttled, fire-and-forget).
              resumeToken.save(force = false)
            }
          }
          override def onError(e: Throwable): Unit = {
            if (ChangeStreamResumeToken.isInvalid(e)) {
              logger.warn(s"MovieRepository change stream: resume token invalid (${e.getMessage}) — clearing it; " +
                "the next open starts fresh and the backstop resyncs the gap.")
              resumeToken.clear()
            } else
              logger.warn(s"MovieRepository change stream ended (${e.getMessage}) — a reopen resumes from the " +
                "persisted token; the backstop covers the meantime.")
            changeSub.set(null)
            changeReopen.failed()
          }
          override def onComplete(): Unit = { changeSub.set(null); changeReopen.failed() }
        })
      logger.info(s"MongoMovieRepository: watching change stream (shared by all listeners)" +
        s"${if (resumeFrom.isDefined) ", resumed from persisted token" else ""}.")
      // Also watch `screenings`: a showtime change fires there, not on `movies`.
      // Re-read + stitch (findById already stitches) + fan out so the projector
      // re-projects the film — the findById is a BLOCKING read, so run it (and the
      // fanout) on `changeApply`, never on the screenings cursor's I/O event loop.
      if (screeningsWatch.get().isEmpty)
        screeningsWatch.set(screenings.flatMap(_.watch(filmId =>
          changeApply.execute(() => findById(filmId).foreach(movieChanges.dispatchUpsert)))))
    }
  }

  /** Stop the shared cursor once no listener remains, so an idle repository
   *  (e.g. web /debug after every viewer disconnects) doesn't keep decoding
   *  every write for nothing. */
  private def stopWatchingIfIdle(): Unit = changeLock.synchronized {
    if (movieChanges.isEmpty) {
      // Persist the final position synchronously so the next process resumes from here.
      resumeToken.save(force = true)
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

  def close(): Unit = {
    changeReopen.close(); resumeToken.save(force = true); changeApply.shutdown(); clientOpt.foreach(_.close())
  }

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

  // Match the in-memory CacheKey's normalization rules so case-only and
  // diacritic variants of the same title share a single Mongo record. Without
  // this, "Tom i Jerry: Przygoda w muzeum" and "Tom i jerry: przygoda w
  // muzeum" — both reported by different cinemas for the same film — each get
  // their own row, and only one can be updated per hourly refresh tick (the
  // tick walks the deduplicated Caffeine cache).
  private def documentId(title: String, year: Option[Int]): String =
    StoredMovieRecord.idFor(title, year, normalizer)
}
