package services.movies

import models.{MovieRecord, Source, SourceData}

import scala.annotation.unused
import scala.concurrent.duration._

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
   *  On the trait because it is the storage contract, not this repository's private
   *  business: the (since deleted) staging fold wrote `movies` directly, in a shape
   *  that did not strip, and the difference was not cosmetic. A slot map with its showtimes inline grows with the number of
   *  venues screening the film, and the United States has 5,031 of them: on
   *  2026-09-01 the fold of `Avengers: Doomsday` threw
   *  `BsonMaximumSizeExceededException` on every single attempt, so the group was
   *  never consumed, so the staging reaper re-enqueued it every tick — forever, with no
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

  /** [[findAll]] as the read it is: failed when the scan was incomplete. `findAll` answers an
   *  incomplete scan with `Seq.empty`, which a caller deciding "is the corpus empty?" must not
   *  take at its word. The in-memory store cannot fail. */
  def findAllChecked(): tools.ReadOutcome[Seq[StoredMovieRecord]] = tools.ReadOutcome.Answered(findAll())

  /** [[findAllChecked]] a page at a time: every row, fully stitched, handed to `onPage` as each
   *  page is read, so a caller that keeps less than a row (the cache hydrate keeps rows without
   *  their showtimes) never holds the whole stitched corpus. Same completeness contract as
   *  [[foreachRecord]]: on `Incomplete` the pages delivered so far are not the corpus. The default
   *  hands [[findAllChecked]]'s answer over as one page. */
  def foreachPage(onPage: Seq[StoredMovieRecord] => Unit): tools.ScanOutcome = findAllChecked() match {
    case tools.ReadOutcome.Answered(rows) => onPage(rows); tools.ScanOutcome.complete
    case tools.ReadOutcome.Failed(cause)  => tools.ScanOutcome.Incomplete(cause.exception)
    case absent                           => tools.ScanOutcome.of(whole = false, s"the corpus read was ${absent.explain}")
  }

  /** The single row stored under this exact `_id` (the [[StoredMovieRecord.idOf]]
   *  form), or `None` when absent. Lets the dev `/debug` page render ONE row's
   *  heavy per-source breakdown lazily, on expand, instead of every row's
   *  up front — rendering the whole corpus's details in one Twirl pass OOM'd the
   *  view. The default scans [[findAll]] (fine for the in-memory store);
   *  `MongoMovieRepository` overrides it with an indexed `_id` lookup. */
  def findById(id: FilmId): Option[StoredMovieRecord] = findByIdChecked(id).toOptionOrThrow

  /** [[findById]] as the read it is — absent ("no such film") told apart from failed ("could
   *  not look").
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
   *  The in-memory store cannot fail. */
  def findByIdChecked(id: FilmId): tools.ReadOutcome[StoredMovieRecord] =
    findAll().find(_.id == id).fold[tools.ReadOutcome[StoredMovieRecord]](tools.ReadOutcome.none(s"film $id"))(tools.ReadOutcome.Answered(_))

  /** [[findByIdChecked]] stitching the cinema slots but NOT the showtimes — the by-id
   *  counterpart of [[foreachRecordWithSlots]], with the same caveat: `.data` is complete,
   *  `.showtimes` is empty. For a re-projection of CARDS only (a derivation change that moved
   *  nothing but card fields): a card never reads a showtime (`ReadModelProjection.metadataHash`),
   *  and skipping the `screenings` read skips most of a whole-row read's bytes — 260 of the US
   *  corpus's 341 MB (2026-09-26). Default delegates to the fully stitched read. */
  def findByIdWithSlotsChecked(id: FilmId): tools.ReadOutcome[StoredMovieRecord] = findByIdChecked(id)

  /** Whether a live document holds `id` — the question minting a fresh id asks of each
   *  candidate. THROWS when the row cannot be read: counting "unknown" as free lets the
   *  write that follows replace the film that holds it. */
  final def holdsId(id: FilmId): Boolean = findByIdChecked(id) match {
    case tools.ReadOutcome.Answered(_)          => true
    case tools.ReadOutcome.Absent(_)   => false
    case tools.ReadOutcome.Failed(cause) => throw new IllegalStateException(s"cannot read whether id $id is taken", cause.exception)
  }

  /** The row whose `key` field is this lookup key — a cold cache asking "is this
   *  film stored?" before it knows the id. Same checked contract as
   *  [[findByIdChecked]]. At most one row holds a key (the cache keeps its map by it);
   *  the default scans [[findAll]], `MongoMovieRepository` uses the `key` index. */
  def findByKeyChecked(key: CacheKey): tools.ReadOutcome[StoredMovieRecord] = {
    val k = StoredMovieRecord.keyFor(key)
    findAll().find(_.key(normalizer) == k).fold[tools.ReadOutcome[StoredMovieRecord]](tools.ReadOutcome.none(s"film keyed $k"))(tools.ReadOutcome.Answered(_))
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
   *  Answers [[tools.ScanOutcome.Complete]] when the WHOLE corpus was scanned, and
   *  `Incomplete` when a read failed mid-scan and the iteration stopped early (rows
   *  delivered so far still reached `f`). A caller that PRUNES on a row's absence — the
   *  read-model reconcile — MUST treat an incomplete scan as "this set is not the complete
   *  corpus" and skip the destructive step, or a transient Mongo read failure deletes live
   *  rows. The in-memory store never fails, so the default reports a complete scan. */
  def foreachRecord(f: StoredMovieRecord => Unit): tools.ScanOutcome = { findAll().foreach(f); tools.ScanOutcome.complete }

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
  def foreachRecordWithoutShowtimes(f: StoredMovieRecord => Unit): tools.ScanOutcome = foreachRecord(f)

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
  def foreachRecordWithSlots(f: StoredMovieRecord => Unit): tools.ScanOutcome = foreachRecord(f)

  /** Like [[foreachRecord]] — fully stitched, showtimes included — but only the rows whose
   *  `updatedAt` is strictly after `since`. The BOUNDED catch-up read for a silent change
   *  stream: the read-model prune sweep hands it the instant of the `movies` cursor's last
   *  delivered event and re-projects exactly the rows written after it, whatever the cursor
   *  failed to deliver. `updatedAt` is bumped on every write (see `MovieCodecs`), and the
   *  Mongo store indexes it so this is a range read, never a collection scan.
   *
   *  Strictly after, because a delivery is stamped AFTER the write it delivers: a row the
   *  cursor did deliver has `updatedAt` before that stamp and is not read again. Same
   *  completeness contract as [[foreachRecord]]. Default: every row — a store that keeps no
   *  timestamp can only over-approximate "changed since", never under. */
  def foreachRecordUpdatedSince(@unused since: java.time.Instant)(f: StoredMovieRecord => Unit): tools.ScanOutcome = foreachRecord(f)

  /** Remove the film stored under this id, with its side-collection rows. Never
   *  throws: a failure is logged, counted and reported as [[WriteOutcome.Failed]]. */
  def delete(id: FilmId): WriteOutcome

  /** Remove every side-collection row (`screenings`, `movie_slots`) whose film has no
   *  document in this store any more — the leftovers of deletes and merges from before
   *  [[delete]] cascaded, which nothing else ever clears.
   *  The rule, its refusals (an incomplete or empty corpus scan deletes nothing) and the
   *  reason it exists are [[StrandedSideRows]]'s, shared with the in-memory fake.
   *  Best-effort; returns what was removed. A store with no side collections has
   *  nothing stranded and reports [[StrandedSideRows.none]]. */
  def deleteStrandedSideRows(): StrandedSideRows = StrandedSideRows.none

  /** Write-through upsert of the film `id`, stored under the lookup key
   *  `sanitize(title)|year`. The same id under a new key is a RETITLE — the document
   *  stays, its `key` moves. Never throws: a failure is logged, counted and reported as
   *  [[WriteOutcome.Failed]] — including a failed side-collection write, since the next
   *  identical upsert is what retries it — and a caller holding an in-memory copy of the
   *  row must roll that copy back on one (see `MovieCache.persist`). */
  def upsert(id: FilmId, key: CacheKey, e: MovieRecord): WriteOutcome

  /** [[upsert]] for a caller outside `services` that holds only a title and year (a
   *  fixture re-seeding rows it read back); the key is derived by this store's rules. */
  def upsert(id: FilmId, title: String, year: Option[Int], e: MovieRecord): WriteOutcome =
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

  /** [[watchChanges]] for a consumer that also WRITES the films it mirrors — the cache. Each
   *  upsert comes with the [[FilmWriteFence]] mark its re-read was taken under, to be applied
   *  only through `writeFence.ifUndisturbed`, so a read taken before one of the consumer's own
   *  writes cannot roll it back. Default: [[watchChanges]] with every delivery
   *  [[FilmWriteFence.Unfenced]] — right for a store that notifies synchronously with the
   *  write, where no delivery can predate a write. */
  def watchChangesFenced(
    onUpsert: (StoredMovieRecord, Long) => Unit,
    onDelete: FilmId => Unit
  ): Option[AutoCloseable] = watchChanges(onUpsert(_, FilmWriteFence.Unfenced), onDelete)

  /** [[watchChanges]] for a consumer that can also take a change confined to some venues'
   *  showtimes as those venues alone ([[VenueSlots]]), answering whether it applied it — a false
   *  answer brings it the whole film. A store that cannot read venues alone hands every change
   *  over whole. */
  def watchChangesWithVenues(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit,
    @unused onVenues: VenueSlots => VenueVerdict
  ): Option[AutoCloseable] = watchChanges(onUpsert, onDelete)

  /** [[watchChangesWithVenues]] fenced as [[watchChangesFenced]] is. */
  def watchChangesFencedWithVenues(
    onUpsert: (StoredMovieRecord, Long) => Unit,
    onDelete: FilmId => Unit,
    @unused onVenues: (VenueSlots, Long) => VenueVerdict
  ): Option[AutoCloseable] = watchChangesFenced(onUpsert, onDelete)

  /** [[watchChangesWithVenues]] attached WITHOUT opening the stream: the consumer is dispatched
   *  every change from the stream's first, whenever another consumer's watch — or [[openChanges]] —
   *  opens it. For a consumer that must not miss what the stream applies before it is ready to
   *  apply changes itself (the read-model projector, attached as its boot reads begin, while the
   *  cache opens the stream). Default: [[watchChangesWithVenues]] — right for a store that notifies
   *  synchronously with the write, which has no stream to open. */
  def attachChangesWithVenues(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: FilmId => Unit,
    onVenues: VenueSlots => VenueVerdict
  ): Option[AutoCloseable] = watchChangesWithVenues(onUpsert, onDelete, onVenues)

  /** Open the change stream for the consumers [[attachChangesWithVenues]] attached, if it is not
   *  running. Default: nothing to open. */
  def openChanges(): Unit = ()

  /** Where a consumer of [[watchChangesFenced]] fences its own writes of a film. */
  lazy val writeFence: FilmWriteFence = new FilmWriteFence()

  /** When each change-stream cursor last DELIVERED an event — see [[ChangeStreamLiveness]].
   *  The worker's `/metrics` ages every cursor off it, and the read-model prune sweep uses
   *  the `movies` cursor's instant as the floor of what a silent stream may have missed.
   *  A repository with no stream answers one per instance from [[ChangeStreamLiveness.unwatched]],
   *  on its own clock: its cursors age from creation and are never stamped. */
  def changeStreamLiveness: ChangeStreamLiveness
  /** Re-reads the change stream holds back for the rest of their film's burst — see
   *  [[MovieChangeStream.Debounce]]; 0 where nothing is watched or nothing debounces. */
  def heldChanges: Int = 0
  /** Queue every held re-read now — for a caller that must see the stream settled. */
  def releaseHeldChanges(): Unit = ()

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
  def upsert(title: String, year: Option[Int], e: MovieRecord): WriteOutcome = {
    val key = CacheKey(title, year, normalizer)
    findByKeyChecked(key) match {
      case tools.ReadOutcome.Answered(row)       => upsert(row.id, key, e)
      case tools.ReadOutcome.Absent(_)  =>
        scala.util.Try(Some(FilmId.legacy(key)).filterNot(holdsId).getOrElse(FilmId.fresh(key, holdsId))) match {
          case scala.util.Success(id) => upsert(id, key, e)
          case scala.util.Failure(_)  => WriteOutcome.Declined("id-unreadable")
        }
      case tools.ReadOutcome.Failed(_)  => WriteOutcome.Declined("key-unreadable")
    }
  }

  def updateIfPresent(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord): Boolean =
    findByKeyChecked(CacheKey(title, year, normalizer)).answered.exists(row => updateIfPresent(row.id, row.cacheKey(normalizer), before, after))

  def delete(title: String, year: Option[Int]): WriteOutcome =
    findByKeyChecked(CacheKey(title, year, normalizer)) match {
      case tools.ReadOutcome.Answered(row)      => delete(row.id)
      case tools.ReadOutcome.Absent(_) => WriteOutcome.Written                   // nothing under the key: already gone
      case tools.ReadOutcome.Failed(_) => WriteOutcome.Declined("key-unreadable")
    }
}

object MovieRepository {
  /** How many times the dotted-name fallback reads and patches again when the document moved under it. */
  val DottedReplaceAttempts = 3

  /** How long a `movies` write is waited for before it is given up on. */
  val WriteTimeout: FiniteDuration = 10.seconds

  /** How far before its floor a catch-up read starts. A row's `updatedAt` is stamped BEFORE its
   *  write is sent, so a write stamped just under a catch-up's floor can commit after that
   *  catch-up's scan has passed — and the next one, reading only past the floor, would skip it
   *  for good. Twice the time a write is waited for, so a write still in flight when a scan ran
   *  is past the floor's overlap by the time it can commit. Re-reading the rows inside it is
   *  harmless: a projection writes only what changed. */
  val CatchUpOverlap: java.time.Duration = java.time.Duration.ofMillis(2 * WriteTimeout.toMillis)

  /** A `movies` document as the dotted-name fallback read it: decoded, to patch, and `whole`, as
   *  stored, for the replace to be guarded on. */
  final case class ReadDocument(stored: StoredMovieDto, whole: org.bson.BsonDocument)

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
