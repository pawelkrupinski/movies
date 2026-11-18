package services.movies

import services.titles.TitleNormalizer

import scala.annotation.unused

/**
 * The read side of the `movies` store: every lookup and scan, and the facts about the
 * store's shape (`enabled`, `hasScreenings`, `hasSlots`, `normalizer`) a reader needs to
 * interpret what it read. A component that only reads the corpus — the read-model sweeper,
 * the orphan and unscreened sweeps, the /debug listing — takes this, not [[MovieRepository]],
 * so the type says it cannot write a film (CLAUDE.md, "Interface segregation").
 * [[MovieRepository]] extends it with the writes; [[MovieChangeFeed]] is the change stream.
 */
trait MovieRepositoryReader {
  /** Whether the read-split is active — i.e. a `screenings` collection is wired, so
   *  showtimes are stored there and the MovieCache can strip them from resident records.
   *  Without it the cache MUST keep showtimes (there is nowhere else to hold them). */
  def hasScreenings: Boolean = false

  /** Whether the slot split is active — i.e. a `movie_slots` repository is wired, so
   *  each film's per-cinema SourceData is mirrored into its own row. Readers use this
   *  to decide whether a film's slots may live outside the `movies` document. */
  def hasSlots: Boolean = false

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
}
