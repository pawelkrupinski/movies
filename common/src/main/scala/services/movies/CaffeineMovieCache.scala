package services.movies

import java.util.Locale

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.{MovieRecord, Source, SourceData}
import play.api.Logging
import services.Stoppable
import settings.{BootHydrateMaxAttempts, BootHydrateRetryInterval, CacheRehydrateInterval}
import tools.DaemonExecutors

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.duration.DurationInt
import scala.util.Try

/**
 * Caffeine-backed `MovieCache` with write-through to `MovieRepository`.
 *
 * One cache, `positive`: every stored film, never expiring in-process (films
 * change slowly; restarts re-warm via `rehydrate`).
 *
 * Pure reads (`get`, `snapshot`, `entries`) have no side
 * effects — callers that want to *trigger* a lookup on miss go through
 * `MovieService` (which owns the worker pool + dedup).
 *
 * What this class OWNS is the resident corpus and the way into it: the Caffeine
 * map and its [[CorpusIndex]], the write funnels (`put`'s tmdbId / imdbId identity
 * gate, `putIfPresent`), the per-title locks, the cache-or-store reads, and
 * hydration and the change-stream mirror. See `docs/stable-film-id.md`.
 */
class CaffeineMovieCache(
  repository: MovieRepository,
  // Boot-hydrate retry — OFF by default (0 attempts) so tests and a genuine
  // cold start pay nothing. Prod turns it on via the Fly env
  // `KINOWO_BOOT_HYDRATE_MAX_ATTEMPTS` so a not-ready Mongo at boot can't leave
  // the cache empty (see `bootHydrate`).
  bootHydrateMaxAttempts: BootHydrateMaxAttempts   = BootHydrateMaxAttempts(0),
  bootHydrateRetry:       BootHydrateRetryInterval = BootHydrateRetryInterval(1.second),
  // Called after a merge whose inputs changed an enrichment's resolution, to
  // re-kick that enrichment as a worker task (per case). Default no-op for unit
  // tests + non-worker builds; the worker wires `QueueEnrichmentRetrigger`.
  retrigger: EnrichmentRetrigger = EnrichmentRetrigger.noop,
  // Measures what the periodic backstop rehydrate catches that the incremental change
  // stream missed — the redundancy signal for retiring the rehydrate. No-op for web/tests.
  cacheMetrics: CacheSyncMetrics = CacheSyncMetrics.noop,
  // The country's title rules, wired from the worker's `country`. Every `CacheKey` this cache
  // builds — including the ones built on the change-stream driver thread and in
  // the rehydrate scheduler — keys through THIS instance, which is what makes
  // the cache's identity country-correct rather than process-global. REQUIRED,
  // not defaulted: this is the production cache, and every CacheKey it builds
  // is a row identity.
  override val normalizer: TitleNormalizer,
  // The silent-write-skip counter (`putIfPresent`) — see `ListingIntakeMetrics`. No-op for
  // web/tests; the worker wires `WorkerTaskMetrics`.
  listingIntakeMetrics: ListingIntakeMetrics = ListingIntakeMetrics.noop,
  // Stamps `lastModified`. System time in production; specs pin or step it.
  val clock: java.time.Clock,
  // Where the cache interns the strings a slot repeats across cinemas. The worker hands every
  // country's cache the process's one pool (owned by `WorkerMetrics`, whose gauges read it); a
  // lone cache — tests included — gets its own.
  val stringPool: StringPool = new StringPool,
  // The backstop rehydrate (`KINOWO_CACHE_REHYDRATE_SECONDS`, resolved by the worker's root);
  // the compiled-in 6 hours for specs.
  rehydrateInterval: CacheRehydrateInterval = CacheRehydrateInterval(6.hours),
  // The boot's other whole-corpus readers, handed the boot hydrate's read so the boot reads
  // the corpus once (see [[BootCorpusReader]]). None for specs and a lone cache.
  bootReaders: Seq[BootCorpusReader] = Nil
) extends MovieCache with Stoppable with Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)

  // `recordStats` so the resident corpus can report its hit ratio — a read served
  // here is a Mongo read not made. Unbounded, so its eviction count stays 0 by
  // construction rather than by luck.
  private val positive: Cache[CacheKey, MovieRecord] = Caffeine.newBuilder().recordStats().build()
  // How a row is shaped to be held resident: its slots stripped and pooled (see [[ResidentRecords]]).
  // Above the boot hydrate below, which shapes every row it reads.
  private val residentShape = new ResidentRecords(() => repository.hasScreenings, stringPool)

  // Writes deferred this process's lifetime because the store could not say whether a document
  // holds their key. Read by the specs: a non-zero value means scrapes are landing against an
  // unreadable corpus, which is the state that used to silently prune boards.
  private[services] val skippedUnreadable = new java.util.concurrent.atomic.AtomicLong(0)

  /** The film id behind each resident key, and back ([[CorpusIndex]]): what every write asks
   *  before it lands and every change-stream apply asks to find a film's key, without a store
   *  round-trip.
   *
   *  It shadows `positive`, so it is only as correct as the funnels below: EVERY write
   *  to `positive` goes through `store` / `evict` / the `computeResident` compute, and
   *  nothing else may call `positive.put` or `positive.invalidate` directly. The same
   *  funnels `announce` each change to the resident listeners (`onResident`); a rollback
   *  that puts a row back by hand announces it itself. The cache
   *  is unbounded, so there is no eviction path to miss. */
  private[movies] val corpusIndex: CorpusIndex =
    new CorpusIndex

  /** Write a row and keep the index with it. The ONLY way into `positive`. */
  private def store(key: CacheKey, record: MovieRecord, id: FilmId): Unit = {
    positive.put(key, record)
    corpusIndex.put(key, id)
    announce(key)
  }

  private val residentListeners = new java.util.concurrent.CopyOnWriteArrayList[(CacheKey, Option[StoredMovieRecord]) => Unit]()

  def onResident(listener: (CacheKey, Option[StoredMovieRecord]) => Unit): Unit = {
    residentListeners.add(listener)
    positive.asMap().keySet().forEach(key => announce(key, java.util.List.of(listener)))
  }

  /** Tell the resident listeners what `key` holds now. Read under the key's own lock, so two writes to one key cannot
   *  reach a listener in the opposite order; `positive` itself is left as it is. Every change to `positive` calls it
   *  once the change is made — `store` and `evict`, and the writes that cannot go through them. */
  private def announce(key: CacheKey, to: java.util.List[(CacheKey, Option[StoredMovieRecord]) => Unit] = residentListeners): Unit =
    if (!to.isEmpty) { positive.asMap().compute(key, (_, held) => {
      val film = Option(held).map(storedAt(key, _))
      // A listener that throws must not fail the write it is told of: the row is already held.
      to.forEach(listener => try listener(key, film) catch {
        case scala.util.control.NonFatal(e) => logger.warn(s"MovieCache: a resident listener failed on '${key.cleanTitle}': ${e.getMessage}")
      })
      held
    }); () }

  /** The film `key` holds as `record`, under its indexed id. */
  private def storedAt(key: CacheKey, record: MovieRecord): StoredMovieRecord =
    StoredMovieRecord(key.cleanTitle, key.year, record, corpusIndex.idOf(key).getOrElse(FilmId.legacy(key)), Some(StoredMovieRecord.keyFor(key)))

  /** The permanent id behind `key`: the one the index holds, else the stored row's, else
   *  a fresh one for a row this cache is about to create. Ids are never re-derived from
   *  a key — see [[FilmId]]. */
  private def idFor(key: CacheKey): Option[FilmId] =
    corpusIndex.idOf(key).orElse(repository.findByKeyChecked(key) match {
      case tools.ReadOutcome.Answered(row)      => Some(row.id)
      case tools.ReadOutcome.Absent(_) => Some(FilmId.fresh(key, corpusIndex.holdsId))
      // The store could not say whether a document holds this key. Minting an id
      // here would write a SECOND document for the key once the store recovers
      // (a failed read is not "absent") — the caller defers instead.
      case tools.ReadOutcome.Failed(_) => None
    })

  /** The id of a RESIDENT row. Every write into `positive` goes through `store`, which
   *  indexes the id, so a resident row without one is a broken funnel — say so, rather
   *  than mint an id for a row that has a document. */
  private def residentIdOf(key: CacheKey): FilmId =
    corpusIndex.idOf(key).getOrElse(throw new IllegalStateException(s"resident row '${key.cleanTitle}' (${key.year.getOrElse("—")}) has no film id"))

  private def deferUnreadableWrite(key: CacheKey): WriteOutcome = {
    logger.warn(s"Deferring write of '${key.cleanTitle}' (${key.year.getOrElse("—")}): the store could not say " +
      "whether a document already holds the key, and writing would risk a second one.")
    skippedUnreadable.incrementAndGet()
    WriteOutcome.Declined("key-unreadable")
  }

  private[services] def idOf(key: CacheKey): Option[FilmId] = corpusIndex.idOf(key)

  /** What the index currently believes, for `VenueSlotsEquivalenceSpec`: a venue apply must leave
   *  it exactly as a whole-film apply does. */
  private[services] def indexSnapshot: Map[CacheKey, FilmId] = corpusIndex.snapshot

  /** Drop a row and keep the index with it. The ONLY way out of `positive`. */
  private def evict(key: CacheKey): Unit = {
    positive.invalidate(key)
    corpusIndex.remove(key)
    announce(key)
  }

  /** The resident corpus, for `kinowo_worker_cache_*`. UNBOUNDED by design — it is
   *  the hydrated corpus, not a working set — so it reports entries and no maximum:
   *  a maximum of zero would render as "full" on a ratio panel. What it is worth
   *  watching for is the SHAPE, a count that tracks the corpus rather than climbing
   *  past it. */
  def occupancy: services.metrics.CacheOccupancy =
    services.metrics.CacheOccupancy.of(positive, weighted = false)

  // Read from the injected clock, like every other stamp this cache takes — strictly monotonic, so a
  // put in the same instant as the last one still moves it.
  private val _lastModified = new java.util.concurrent.atomic.AtomicReference[java.time.Instant](clock.instant())
  def lastModified: java.time.Instant = _lastModified.get()
  private def touch(): Unit = { _lastModified.updateAndGet(tools.MonotonicStamp.after(_, clock)); () }

  // Per-normalised-title locks for `recordCinemaScrape`. Two cinemas
  // first-scraping the same brand-new film concurrently used to each see an
  // empty cache in their redirect check and each created its own row at a
  // different `(title, year)` key. Serialising the redirect-then-put step
  // per normalised title eliminates that race; films with different
  // normalised titles still scrape in parallel.
  private val titleLocks = new ConcurrentHashMap[String, AnyRef]()
  private def lockFor(rawTitle: String): AnyRef =
    titleLocks.computeIfAbsent(normalizer.sanitize(rawTitle), _ => new Object())

  /** The lock that serialises every read-modify-write on rows whose
   *  normalised cleanTitle matches `cleanTitle`. Reentrant (JVM
   *  `synchronized`), so a body that itself calls `rekey` or
   *  `recordCinemaScrape` for the same title doesn't self-deadlock. */
  private[services] def withTitleLock[A](cleanTitle: String)(body: => A): A =
    lockFor(cleanTitle).synchronized(body)

  // Hydrate from Mongo on construction. Synchronous: Wiring builds the cache
  // during `start()`, so the first HTTP request only lands after the initial
  // findAll has completed. Pages render against a fully-populated cache; no
  // first-request flicker, no scrape-vs-hydrate race.
  //
  // RETRY an empty result (prod only): the worker boots alongside its Mongo, so
  // an empty or failed read at boot is almost always "Mongo not ready yet" (`rehydrate`
  // loads nothing either way). Without retry the cache starts empty and the
  // change stream only ever delivers rows written AFTER boot — leaving every
  // quiescent row (one not re-scraped since) Mongo-only and invisible to the
  // in-memory fold / settle, so its duplicate sits stranded forever. Bounded, so
  // a genuinely empty corpus still proceeds after the attempts. Default 0
  // attempts = one plain hydrate (tests, cold start); prod sets the env.
  // Set once a rehydrate's corpus read has been answered whole — an empty corpus included — and
  // never cleared: from then on a failed read only leaves the cache a little stale. Declared
  // BEFORE the boot hydrate below, whose write a later initialiser would reset.
  @volatile private var wholeCorpusRead = false
  def hydrated: Boolean = wholeCorpusRead

  bootHydrate()

  private def bootHydrate(): Unit = {
    // Each read's pages go to the boot readers as they are read; the first complete, non-empty read is
    // the corpus. An empty answer is not the corpus: at boot it is as likely a Mongo that is not ready
    // yet as an empty store, so its pages are forgotten and the hydrate reads again.
    val offer: Seq[StoredMovieRecord] => Unit = page => bootReaders.foreach(_.bootPage(page))
    var attempt = 0
    var read    = rehydrateFrom(offer)
    while (read == 0 && attempt < bootHydrateMaxAttempts.value) {
      bootReaders.foreach(_.bootReadEnded(BootReadEnd.Retrying))
      attempt += 1
      Thread.sleep(bootHydrateRetry.value.toMillis) // an interrupt (shutdown mid-boot) ends the construction
      read = rehydrateFrom(offer)
    }
    bootReaders.foreach(_.bootReadEnded(if (read > 0) BootReadEnd.Whole else BootReadEnd.GaveUp))
  }

  // Key by the title's OWN form — the same input the display vote
  // (`MovieRecord.displayTitle`) sanitizes — so a record's identity always
  // matches what it shows, and two listings are merged only when they resolve
  // to the same title key on their own. `CacheKey.normalized` still applies
  // `sanitize` (= `normalize` Arabic→Roman + `canonical` & → i / "Gwiezdne
  // Wojny:" + deburr), so the GLOBAL canonical folds stay in the key; only the
  // GlobalStructural decoration strip (anniversary / "- wersja X" / slash
  // suffix / Cykl prefix / restored) is left out — that tier now only feeds
  // `apiQuery` for external lookups, not identity.
  private[services] def keyOf(title: String, year: Option[Int]): CacheKey =
    CacheKey(title, year, normalizer)

  private[services] def get(key: CacheKey): Option[MovieRecord] =
    Option(positive.getIfPresent(key))

  /** Persist a row at `key`, under the film id the key already has (the index's, else the stored
   *  document's, else a fresh one). Deferred when the store cannot say whether a document holds
   *  the key, and refused when another film does (`persist`). Which listings are one film is the
   *  identity projection's to decide ([[writeProjected]]); this write folds nothing. */
  private[services] def put(key: CacheKey, e: MovieRecord): WriteOutcome =
    idFor(key).fold(deferUnreadableWrite(key)) { id =>
      val outcome = persist(key, e, id)
      if (outcome == WriteOutcome.Written) changed(id)
      outcome
    }

  private val changeListeners = new java.util.concurrent.CopyOnWriteArrayList[FilmId => Unit]()
  def onChanged(listener: FilmId => Unit): Unit = { changeListeners.add(listener); () }
  private def changed(id: FilmId): Unit = changeListeners.forEach(_(id))

  private def persist(key: CacheKey, e: MovieRecord, id: FilmId): WriteOutcome = corpusIndex.idOf(key).filter(_ != id) match {
    case Some(holder) =>
      // A DIFFERENT film already answers to this key: two films sharing a title and a
      // year. Writing would put a second document under one key and drop the holder out
      // of the cache — the property spec's first find. The row stays where it is instead.
      logger.warn(s"Refusing to write '${key.cleanTitle}' (${key.year.getOrElse("—")}) as $id: another film " +
        s"($holder) holds that key. The row keeps its current key.")
      WriteOutcome.Declined("key-held-by-another-film")
    case None => repository.writeFence.writing(id) {
      val clean  = ResidentRecords.withoutZeroRatings(e)
      val cached = residentShape.of(clean)
      val prior  = Option(positive.getIfPresent(key))
      store(key, cached, id)
      // `clean` may carry stripped slots (folds/canonicalize read from the stripped cache);
      // `upsert` re-stitches those from the film's screenings so a full write never deletes them.
      val outcome = repository.upsert(id, key, clean)
      if (outcome.failed || outcome == WriteOutcome.IdentityHeld) {
        // …and a write that FAILED — or that `movies` DECLINED because another document holds
        // the key or tmdbId, which writes nothing just the same — leaves the cache as it was
        // before it. Kept, the unwritten row made every later identical scrape diff as a no-op
        // (`putIfPresent`'s write guard compares against it), so the write was never retried:
        // on 2026-09-24 a codec bug failed 34 upserts over ~6h and two new films never reached
        // the site until a restart. Rolled back, the next scrape finds the row as Mongo has it
        // — absent for a new film, so it `put`s again — and retries (a held identity is usually
        // a merge's losers, gone by then). Only if the entry is still the one written here: a
        // concurrent writer or an eviction may have replaced it since. A failure itself is
        // logged and counted by the repository.
        prior match {
          case Some(previous) => restore(key, cached, previous, id)
          case None           => if (positive.asMap().remove(key, cached)) corpusIndex.remove(key)
        }
        announce(key)
      }
      touch()
      outcome
    }
  }

  private[services] def writeProjected(id: FilmId, key: CacheKey, e: MovieRecord): WriteOutcome =
    corpusIndex.idOf(key).filter(_ != id) match {
      case Some(_) => WriteOutcome.IdentityHeld
      case None => repository.writeFence.writing(id) {
        val clean   = ResidentRecords.withoutZeroRatings(e)
        val outcome = repository.upsert(id, key, clean)
        if (outcome == WriteOutcome.Written) {
          corpusIndex.keyOf(id).filter(_ != key).foreach(evict)
          store(key, residentShape.of(clean), id)
          touch()
        }
        outcome
      }
    }

  private[services] def holdsSlotsLean: Boolean = repository.hasScreenings && repository.hasSlots

  private[services] def patchProjected(id: FilmId, key: CacheKey, before: MovieRecord, after: MovieRecord,
                                       only: Option[Set[Source]]): WriteOutcome = {
    // Only over the production storage split: a store holding showtimes inline in the record compares slots
    // showtime-blind, so a patch would not see a slot whose showtimes alone moved. And never a film changing TMDB id:
    // that write's order against the other films' is the unique index's, which the whole write is ordered by.
    val patched = repository.hasScreenings && repository.hasSlots && before.tmdbId == after.tmdbId && withTitleLock(key.cleanTitle) {
      corpusIndex.idOf(key).contains(id) && repository.writeFence.writing(id) {
        // Resident as the projection read it: what the store holds, so the patch from it is the whole change.
        (positive.getIfPresent(key) eq before) && {
          val clean = ResidentRecords.withoutZeroRatings(after)
          // Diffed only `only` when given: the sources that may differ, every other the same slot on both sides.
          repository.updateIfPresent(id, key, only.fold(before)(LeanRecords.only(before, _)), only.fold(clean)(LeanRecords.only(clean, _))) &&
            { store(key, residentShape.over(before, clean), id); touch(); true }
        }
      }
    }
    if (patched) WriteOutcome.Written else writeProjected(id, key, after)
  }

  private[services] def retireProjected(id: FilmId): WriteOutcome = {
    val outcome = repository.delete(id)
    if (outcome == WriteOutcome.Written) {
      corpusIndex.keyOf(id).foreach(evict)
      RemovalAudit.filmRemoved("identity.projection", id.value, reason = "retired-by-overlap")
      touch()
    }
    outcome
  }

  /** Re-kick (as worker tasks) the enrichments whose input fields a merge
   *  changed — `before` is the pre-merge survivor, `after` the merged record now
   *  stored under `afterKey`. Pure decision in [[MergeRetrigger]]; the injected
   *  [[EnrichmentRetrigger]] does the freshness-invalidate + enqueue. */
  private def retriggerChangedEnrichments(
    before: MovieRecord, beforeKey: CacheKey, after: MovieRecord, afterKey: CacheKey
  ): Unit = {
    val kinds = MergeRetrigger.changedEnrichments(before, beforeKey, after, afterKey)
    if (kinds.nonEmpty) retrigger.retrigger(afterKey, after, kinds)
  }

  // An enrichment write (Filmweb slot, IMDb slot, …) at a stable key: before/after
  // share the key, so the merge decision reduces to the input-field deltas.
  private[services] def retriggerAfterEnrichment(key: CacheKey, before: MovieRecord, after: MovieRecord): Unit =
    retriggerChangedEnrichments(before, key, after, key)

  /** The film's currently-stored record for `key`: the live cache entry when
   *  resident, else a direct `movies` read. The TMDB resolve's carry-forward
   *  (`MovieService.runTmdbStageSync`) reads this rather than the Caffeine-only
   *  `get`, so a cold / evicted / re-keyed entry doesn't make the rebuild read
   *  EMPTY and null the scores the `*Ratings` refreshers own (and the cinema
   *  slots) — the "ratings keep disappearing" clobber. On a warm cache it's just
   *  `get`; the `movies` read only happens on a miss. */
  private[services] def storedChecked(key: CacheKey): tools.ReadOutcome[MovieRecord] =
    get(key).fold(findStoredChecked(key).map(_.record))(tools.ReadOutcome.Answered(_))

  /** The stored row behind `key` — by id when the index knows it, by key otherwise. */
  private def findStoredChecked(key: CacheKey): tools.ReadOutcome[StoredMovieRecord] =
    corpusIndex.idOf(key).fold(repository.findByKeyChecked(key))(repository.findByIdChecked)

  /** Conditional write — applies `updater` to the row if it currently exists
   *  in the cache, otherwise a no-op. Returns true if the write landed.
   *
   *  Used by the rating listeners (`ImdbRatings`, `FilmwebRatings`,
   *  `MetascoreRatings`, `RottenTomatoesRatings`) so a rating fetch that
   *  finishes after a concurrent `cache.invalidate` doesn't resurrect the row.
   *  The updater runs inside Caffeine's per-key compute lock so two
   *  concurrent rating updates on the same key serialize cleanly. The Mongo
   *  side uses `replaceOne(upsert=false)` for the same no-resurrect
   *  guarantee against a concurrent `repository.delete`.
   *
   *  The updater takes the CURRENT value (the freshest cached row) rather
   *  than a captured snapshot — so a listener that read the row, made a
   *  slow network call, and now wants to update one field doesn't clobber
   *  concurrent updates to other fields. */
  private[services] def putIfPresent(key: CacheKey, updater: MovieRecord => MovieRecord): Boolean =
    updateResident[MovieRecord](key) { current =>
      val u = updater(current)
      // An updater that hands the row back untouched changed nothing: keep the resident
      // value as it is, rather than stripping, re-indexing and diffing every slot of it
      // to find that out. The landing's duplicate-slot drop does this once per listing,
      // on a row that can carry thousands of venues. A zero rating still takes the long
      // way, because `withoutZeroRatings` would change it.
      // Cache stores the STRIPPED record; the write uses the FULL one so the
      // screenings-diff has real showtimes.
      Option.unless((u eq current) && !ResidentRecords.carriesZeroRating(current)) {
        val full = ResidentRecords.withoutZeroRatings(u)
        (residentShape.of(full), full)
      }
    } { (id, prior, updated, fullAfter) =>
      // Write-guard by DIGEST: equal non-showtime fields AND equal per-slot showtime digest
      // ⇒ no real change, skip the write. See ShowtimesDigest.leanEqual.
      // Equal is the common case — an unchanged re-scrape re-asserting its slot — and it
      // skips the repository entirely: each no-op write would be an `updatedAt`-only
      // `updateOne`, an oplog entry and a change-stream `updateLookup` per row, per pass,
      // the dominant load on the shared-CPU Mongo.
      ShowtimesDigest.leanEqual(fullAfter, prior) || writeThrough(key, id, prior, updated, prior, fullAfter)
    }

  /** The read-modify-write both [[putIfPresent]] and [[putSlotsIfPresent]] make of a resident row.
   *
   *  Serialised under the per-title lock that `recordCinemaScrape` and `rekey` also hold.
   *  Caffeine's `computeIfPresent` is atomic for a single KEY, but a rating update racing a
   *  concurrent `rekey` (which invalidates the old key and re-puts under a new one) could land on a
   *  key being torn out from under it — the write silently lost. Sharing the title lock (reentrant;
   *  always acquired before Caffeine's compute lock, so the order stays title→Caffeine and can't
   *  deadlock) makes every read-modify-write on a row — scrape, rekey, rating — mutually exclusive.
   *  The slow rating HTTP fetch already happened in the caller; only the cache write is held here.
   *
   *  `compute` runs inside Caffeine's compute lock on the resident row: the row to keep resident and
   *  what the write needs (`W`), or None to keep the row as it is. Both `before` and its result are
   *  captured there so the pair is atomic: the repository write uses the pair to compute a per-field
   *  diff — out-of-band Mongo edits to fields the updater didn't touch (e.g. `FilmwebUrlAudit`
   *  clearing `filmwebUrl` while we're bumping `filmwebRating`) survive the write. `persist` writes
   *  a row that changed, under the id the index holds for its key — `computeIfPresent` writes inside
   *  Caffeine's own lock, so it cannot go through `store`, and need not. */
  private def updateResident[W](key: CacheKey)(compute: MovieRecord => Option[(MovieRecord, W)])(
      persist: (FilmId, MovieRecord, MovieRecord, W) => Boolean): Boolean =
    withTitleLock(key.cleanTitle) { fencingResident(key) {
      val before = new java.util.concurrent.atomic.AtomicReference[MovieRecord]()
      val work   = new java.util.concurrent.atomic.AtomicReference[W]()
      val updated = computeResident(key) { current =>
        before.set(current)
        compute(current).fold(current) { case (next, w) => work.set(w); next }
      }
      updated.fold(false) { updated =>
        val prior = before.get()
        (updated eq prior) || persist(residentIdOf(key), prior, updated, work.get())
      }
    } }

  /**
   * [[putIfPresent]] of an update to some of a row's slots — each of `sources` set to `slotOf` what it held — in work
   * independent of how many OTHER slots the row carries: a venue page landing on the slots that name it (the detail
   * handler), or one listing's slot.
   *
   * THE BUG THIS REPLACES. The landing wrote through `putIfPresent`, which re-indexes the
   * whole row, strips every slot and compares every slot: O(slots) per landing. A film
   * shown at N venues lands N times a tick, so that was O(N²) per film per tick — a re-scrape
   * of the US corpus took ~12x its first scrape, and prod paid it on every landing of a
   * widely shown film. The detail handler paid the same on every page read — 40% of a US
   * detail drain's CPU, its other claimants queued behind the title lock (JFR, 2026-10-07). Here the no-op guard
   * compares only the slots the write touches, the cache strips those alone, and the repository is handed the two
   * records narrowed to them — every diff the repository makes is per source, so the narrowed pair yields exactly the
   * patch the whole pair did.
   *
   * Identical results to `putIfPresent` by construction — the store, the cache and the films announced changed: the
   * other slots and every top-level field are the resident row's own on both sides of the diff. A row carrying a zero
   * rating, or a source that is not a cinema slot, takes `putIfPresent` itself — those are the writes that change more
   * than the slots.
   */
  private[services] override def putSlotsIfPresent(key: CacheKey, sources: Seq[Source])(slotOf: (Source, Option[SourceData]) => SourceData): Boolean =
    if (sources.exists(Source.cinemaOf(_).isEmpty) || get(key).exists(ResidentRecords.carriesZeroRating))
      putIfPresent(key, current => current.copy(data = sources.foldLeft(current.data)((data, source) => data + (source -> slotOf(source, data.get(source))))))
    else updateResident[Map[Source, SourceData]](key) { current =>
      // The write guard, asked of each slot written: `leanEqual` of the whole pair is exactly
      // this, since nothing else differs between them.
      val written = sources.distinct.flatMap { source =>
        val held = current.data.get(source)
        val slot = slotOf(source, held)
        Option.unless(held.exists(ShowtimesDigest.slotLeanEqual(_, slot)))(source -> slot)
      }.toMap
      Option.when(written.nonEmpty)((current.copy(data = current.data ++ written.view.mapValues(residentShape.slot)), written))
    } { (id, prior, updated, written) =>
      def only(data: Map[Source, SourceData]) = prior.copy(data = data)
      writeThrough(key, id, prior, updated, only(prior.data.view.filterKeys(written.contains).toMap), only(written))
    }

  /** Run a local write of the row RESIDENT at `key` inside the repository's write fence, so a
   *  change-stream read taken before it cannot roll it back (see [[FilmWriteFence]]). A key
   *  with no resident row has nothing a read could roll back — and nothing to write. */
  private def fencingResident[A](key: CacheKey)(write: => A): A =
    corpusIndex.idOf(key).fold(write)(repository.writeFence.writing(_)(write))

  /** Caffeine's `computeIfPresent` on `key`: the value `f` produced, or `None` — metered —
   *  when the key had left the cache by the time it computed. */
  private def computeResident(key: CacheKey)(f: MovieRecord => MovieRecord): Option[MovieRecord] = {
    // Whether `f` handed back another row than the one held: a write that changes nothing is not announced, since the
    // landing hands every listing's unchanged row back, once per venue of a film shown at thousands.
    var moved = false
    val computed = Option(positive.asMap().computeIfPresent(key, new java.util.function.BiFunction[CacheKey, MovieRecord, MovieRecord] {
      override def apply(k: CacheKey, current: MovieRecord): MovieRecord = { val next = f(current); moved = next ne current; next }
    }))
    if (moved) announce(key)
    computed.orElse {
      // The Caffeine-level race the since-deleted `ScrapeLanding`'s comment on `landed` named: a
      // concurrent `rekey` of some OTHER title invalidated this key between the
      // read and this compute. Recorded here, not at each caller, because this is
      // the one place that KNOWS it happened — every `putIfPresent` caller
      // (scrape, rating refresh, rekey) shares the same race.
      listingIntakeMetrics.recordWriteSkipped(ListingIntakeMetrics.SkipReason.CacheMissRace)
      None
    }
  }

  /** The repository half of [[putIfPresent]] / [[putSlotsIfPresent]]: write the `before` →
   *  `after` diff and announce the film changed, or on failure put the resident `prior` back in
   *  place of `updated`. */
  private def writeThrough(key: CacheKey, id: FilmId, prior: MovieRecord, updated: MovieRecord,
                           before: MovieRecord, after: MovieRecord): Boolean = {
    // ITS RESULT, not `true` — fixed 2026-09-13. This discarded
    // `repository.updateIfPresent`'s answer and reported success unconditionally,
    // so a genuine persistence failure (the Mongo document didn't match, or the
    // write threw and was caught into `false`) was invisible at every caller:
    // the index update already made the CACHE look correct, the since-deleted `ScrapeLanding`'s
    // `landed` gate — which existed precisely "to read the write", per its own
    // comment — saw `true` regardless, and the title was never spared from that
    // tick's prune nor counted anywhere. A row this happens to can look perfectly
    // healthy in-memory while Mongo silently never catches up.
    val wrote = repository.updateIfPresent(id, key, before, after)
    if (!wrote) {
      logger.warn(s"MovieCache.putIfPresent(${key.cleanTitle}, ${key.year.getOrElse("—")}): " +
        "the repository write reported failure for a row the cache still holds resident " +
        "— the Mongo document didn't match, or the write itself failed.")
      listingIntakeMetrics.recordWriteSkipped(ListingIntakeMetrics.SkipReason.RepositoryWriteFailed)
      // …and put the resident row BACK. The write guard diffs against it, so a
      // cache left holding the state Mongo never took would make the next identical
      // update (the next tick re-asserting the same slot) look like a no-op: skipped,
      // reported as success, the failure healed in memory and never in Mongo. `replace`
      // only if it is still ours — under the title lock nothing else wrote this key,
      // but an eviction may have.
      if (restore(key, updated, prior, id)) announce(key)
    }
    touch()
    if (wrote) changed(id)
    wrote
  }

  /** Put `previous` back at `key` in place of `written`, if `written` is still what is held there
   *  (a concurrent writer or an eviction may have replaced it since): whether it did. */
  private def restore(key: CacheKey, written: MovieRecord, previous: MovieRecord, id: FilmId): Boolean =
    positive.asMap().replace(key, written, previous) && { corpusIndex.put(key, id); true }

  /** Drop a row from positive cache + Mongo — used by the TMDB stage to clear
   *  a stale row before re-keying it under a corrected (title, year). */
  private[services] def invalidate(key: CacheKey): Unit = {
    val id = corpusIndex.idOf(key)
    evict(key)
    id.orElse(repository.findByKeyChecked(key).answered.map(_.id)).foreach(repository.delete)
    touch()
  }

  def snapshot(): Seq[StoredMovieRecord] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.iterator
      .map { case (k, e) => storedAt(k, e) }
      .toSeq
      .sortBy(_.title.toLowerCase(Locale.ROOT))
  }

  /** Snapshot of (key, enrichment) pairs for the IMDb refresh loop. Copy so a
   *  concurrent `put` mid-iteration doesn't surprise the caller. */
  private[services] def entries: Seq[(CacheKey, MovieRecord)] = {
    import scala.jdk.CollectionConverters._
    positive.asMap().asScala.toSeq
  }

  /** Put every stored row and evict the keys gone from the store; how many rows it put. An incomplete
   *  read (a page the scan could not read is skipped whole) changes nothing — no row put, none
   *  evicted: its missing films are not gone. The next backstop tick reads again. */
  def rehydrate(): Int = rehydrateFrom(_ => ())

  /** [[rehydrate]], handing each page of its read to `onPage` as it is read.
   *
   *  Streamed, not collected: each page is stripped to what the cache keeps ([[forCache]]) as it
   *  arrives and then dropped, so the read never holds the whole stitched corpus — every showtime
   *  of every film — at once. Collected, it did, for the whole read and then for the boot readers'
   *  derivation after it: long enough at boot that the corpus's showtimes were promoted into the old
   *  generation only to die there (see [[BootCorpusReader]]). Nothing is put or evicted until the
   *  read is complete. */
  private def rehydrateFrom(onPage: Seq[StoredMovieRecord] => Unit): Int = {
    // Additive sync — never blank the cache mid-rehydrate. The backstop
    // tick (see `start()` below) runs while readers walk `snapshot()`;
    // an `invalidateAll()` window would briefly show them an empty corpus. Instead: put every Mongo row (cache's
    // copy gets overwritten if it changed), then evict only the keys
    // that disappeared from Mongo since the last sync.
    val findingAll = tools.Stopwatch.start()
    // Marked BEFORE the read: a row this cache writes while `findAll` runs is newer than the
    // snapshot, and storing (or evicting) it from the snapshot would undo the write — see
    // [[FilmWriteFence]]. Such a row is left as the write made it; its own change-stream event,
    // or the next backstop, reconciles it.
    val marks = repository.writeFence.markAll()
    readByKey(onPage, findingAll).fold(0) { case (byKey, rows) =>
      val tFindAllMs = findingAll.millis
      if (refusesEmpty(rows, tFindAllMs)) 0
      else {
        val populating = tools.Stopwatch.start()
        val changed    = populate(byKey, marks)
        val removed    = evictGone(byKey, marks)
        cacheMetrics.recordRehydrate(changed, removed)
        if (changed > 0 || removed > 0)
          logger.info(s"MovieCache rehydrate: caught $changed changed row(s) + $removed orphan-delete(s) " +
            "the change stream missed.")
        // Hydrate is a PURE LOAD: it rebuilds the cache from Mongo and stops. Which listings are one
        // film is the identity projection's to decide, never the load's.
        if (rows > 0)
          logger.info(s"Hydrated $rows enrichment(s) from Mongo — findAll=${tFindAllMs}ms populate=${populating.millis}ms.")
        touch()
        rows
      }
    }
  }

  /** Whether a complete read of `rows` documents is one the rehydrate must not act on.
   *
   *  An ANSWERED empty corpus while the cache holds rows would evict every one of them — a real Mongo
   *  wipe is a degenerate manual operation, acceptable to handle only on a restart, so the cache is
   *  left intact rather than trusted to that one answer. On a cold cache an empty answer is acted on
   *  (a no-op), but said out loud: don't silently start serving an empty repertoire — a Mongo timeout
   *  / disabled connection / projection bug is then obvious in the boot log instead of hiding behind
   *  a 200 response with zero films on it. */
  private def refusesEmpty(rows: Int, tFindAllMs: Long): Boolean = {
    val cachedSize = positive.estimatedSize()
    if (rows == 0 && cachedSize > 0) {
      logger.warn(s"MovieCache rehydrate: the corpus read answered empty while the cache holds $cachedSize row(s) — " +
                  "cache left intact.")
      true
    } else {
      if (rows == 0)
        logger.warn(s"MovieCache rehydrate: findAll() returned empty on a cold cache (findAll=${tFindAllMs}ms) — " +
                    "Mongo connection disabled, query timed out, or repository genuinely empty. " +
                    "Pages will render with no films until the next successful tick.")
      false
    }
  }

  /** Store every key's record; how many differed from the resident one. Count what this backstop
   *  reload catches that the incremental change stream missed: a put whose cached value DIFFERED (a
   *  missed upsert) — with `evictGone`'s missed deletes, after resume tokens + delete-apply these
   *  should be ~0 in steady state — the signal (kinowo_worker_cache_rehydrate_changes) that the
   *  rehydrate is redundant. */
  private def populate(byKey: java.util.LinkedHashMap[CacheKey, CaffeineMovieCache.HydratedRows], marks: FilmWriteFence.Marks): Int = {
    var changed = 0
    byKey.forEach { (k, held) =>
      // Several documents under one key are the same film twice; the lowest id is the
      // survivor, deterministically, and the others are reconciled later.
      val survivor = held.ids.minBy(_.value)
      repository.writeFence.ifUndisturbed(survivor.value, marks.of(survivor.value)) {
        if (!Option(positive.getIfPresent(k)).exists(ShowtimesDigest.leanEqual(_, held.record))) changed += 1
        store(k, held.record, survivor)
      }
    }
    changed
  }

  /** Evict every resident key the read no longer holds; how many. Only a key whose film nobody wrote
   *  since the snapshot: one this cache created or retitled meanwhile is absent from the snapshot
   *  because it is NEWER, not gone. */
  private def evictGone(byKey: java.util.LinkedHashMap[CacheKey, CaffeineMovieCache.HydratedRows], marks: FilmWriteFence.Marks): Int = {
    import scala.jdk.CollectionConverters._
    positive.asMap().keySet().asScala.toSeq.filterNot(byKey.containsKey).count { k =>
      corpusIndex.idOf(k).fold { evict(k); true }(id => repository.writeFence.ifUndisturbed(id.value, marks.of(id.value))(evict(k)))
    }
  }

  /** Read the whole corpus, by key, as the cache keeps it — or None, having logged why, when the read
   *  was incomplete or a key two documents share could not be read again whole.
   *
   *  Grouped by key BEFORE putting: a merge-key rule added after these documents were written (a new
   *  GlobalStructural strip) makes two stored titles collide on `CacheKey`, and a bare `put`-per-row
   *  is last-write-wins — it would silently drop one document's showtimes until the next scrape.
   *  The colliding rows are unioned instead, so the cache is lossless the moment the rule lands (the
   *  orphaned Mongo `_id` is reconciled by a later scrape / the reaper). Each row is kept as the cache
   *  keeps it; a key two rows share is read again whole, since their union needs the showtimes. */
  private def readByKey(onPage: Seq[StoredMovieRecord] => Unit, reading: tools.Stopwatch.Started)
      : Option[(java.util.LinkedHashMap[CacheKey, CaffeineMovieCache.HydratedRows], Int)] = {
    import scala.jdk.CollectionConverters._
    val byKey = new java.util.LinkedHashMap[CacheKey, CaffeineMovieCache.HydratedRows]()
    var rows  = 0
    val scan  = repository.foreachPage { page =>
      page.foreach { row =>
        val held = CaffeineMovieCache.HydratedRows(row.id, residentShape.of(row.record))
        byKey.merge(row.cacheKey(normalizer), held, (first, next) => first.and(next.ids))
        ()
      }
      rows += page.size
      onPage(page)
    }
    val collided = byKey.asScala.collect { case (key, held) if held.ids.sizeIs > 1 => key -> held.ids }
    val read     = if (!scan.isComplete) None else unionCollided(collided.toSeq)
    if (read.isEmpty)
      logger.warn(s"MovieCache rehydrate: the corpus read ${if (scan.isComplete) "could not re-read the rows two documents share a key with" else scan.explain} " +
        s"(in ${reading.millis}ms) — nothing put or evicted; the next tick reads again.")
    read.map { unions =>
      unions.foreach { case (key, record) => byKey.computeIfPresent(key, (_, held) => held.copy(record = record)); () }
      wholeCorpusRead = true
      (byKey, rows)
    }
  }
  /** The union of each collided key's documents, read again whole — as the cache keeps it — or None
   *  when one of them could not be read: a union of what could be read would drop the rest's
   *  showtimes, the loss the union exists to prevent. A document deleted since the scan is no
   *  longer part of the film. */
  private def unionCollided(collided: Seq[(CacheKey, Seq[FilmId])]): Option[Seq[(CacheKey, MovieRecord)]] =
    collided.foldLeft(Option(Vector.empty[(CacheKey, MovieRecord)])) { case (acc, (key, ids)) =>
      acc.flatMap { done =>
        val reads = ids.map(repository.findByIdChecked)
        Option.when(reads.forall(read => read.answered.isDefined || read.isInstanceOf[tools.ReadOutcome.Absent])) {
          reads.flatMap(_.answered).map(_.record) match {
            case Seq()  => done
            case whole  => done :+ (key -> residentShape.of(MovieRecordMerge.unionAll(whole)))
          }
        }
      }
    }

  // ── Mongo → cache sync ─────────────────────────────────────────────────────
  //
  // Out-of-band Mongo edits — `FilmwebUrlAudit` clearing a stale URL, a manual
  // `db.movies.update(...)` to fix one row — bypass the in-memory cache. Two
  // mechanisms keep the cache current:
  //
  //  1. INCREMENTAL (primary): a change stream (`repository.watchChanges`) applies
  //     each inserted/updated/replaced row (`applyUpsert`) AND each DELETE
  //     (`applyDelete`) to the cache the moment it lands in Mongo — O(changes),
  //     near-instant, and costs nothing when nothing changes. This replaced a
  //     periodic full `findAll()` that re-read the ENTIRE collection on a timer: at
  //     ~200 rows that was ~150 ms (so the old 30-s cadence felt free), but at 500+
  //     rows it climbed to 6–9 s and, run twice a minute, burned 20–30% of the single
  //     shared vCPU continuously and starved page renders. The change stream makes the
  //     cost proportional to real edits instead of corpus size.
  //
  //  2. BACKSTOP (safety net): a RARE full `rehydrate()` — now that upserts (`applyUpsert`),
  //     deletes (`applyDelete`) and CacheKey collisions (the settle-reaper's
  //     `canonicalizeBySanitize` merges them in the source, and the stream propagates the
  //     result) all resolve incrementally, and the stream resumes from a persisted token
  //     across restarts, its three former jobs (deletes / gaps / collisions) are covered
  //     elsewhere. What's left is belt-and-suspenders + the rare orphan-reap (drifted-`_id`
  //     Mongo hygiene, below), so the expensive findAll dropped from every 30 s → 30 min →
  //     now every `BackstopIntervalSeconds` (default 6 h). A full retire would need a
  //     source-id↔key reverse-index across all 9 cache write paths — not worth it for a
  //     case the reaper already self-heals. Tunable via KINOWO_CACHE_REHYDRATE_SECONDS.
  //
  // The scheduler + watch only start when `start()` is called (Wiring does,
  // tests don't), so unit tests still get a single one-shot hydrate at
  // construction unless they opt into the live sync.
  private val refreshScheduler        = DaemonExecutors.scheduler("movie-cache-refresh")
  private val BackstopIntervalSeconds = rehydrateInterval.value.toSeconds
  @volatile private var watchHandle: Option[AutoCloseable] = None

  /** Apply one out-of-band upsert from the change stream to the in-memory cache.
   *  Mirrors a single row of `rehydrate` — a direct `positive.put`, bypassing
   *  the identity-gate `put` (Mongo is already the source of truth here, no
   *  re-folding needed).
   *
   *  …unless this cache has written the film since the stream re-read it (`mark`, see
   *  [[FilmWriteFence]]): that read is older than the resident row, and storing it rolled
   *  the write back until the write's own event re-read the film. That event still comes,
   *  so skipping the older read loses nothing. */
  private[services] def applyUpsert(r: StoredMovieRecord, mark: Long): Unit = {
    val key = r.cacheKey(normalizer)
    val applied = repository.writeFence.ifUndisturbed(r.id.value, mark) {
      // A retitle arriving from another writer: the id's previous key entry is stale.
      corpusIndex.keyOf(r.id).filter(_ != key).foreach(evict)
      val held   = get(key)
      val cached = held.fold(residentShape.of(r.record))(residentShape.keeping(_, r.record))
      // The echo of a write this cache made reads back what it holds already — kept as the very object, not replaced by
      // a copy that every reader then compares slot by slot and that lives, until the film's next write, long enough to
      // be promoted (worker-us).
      if (!(held.exists(_ eq cached) && corpusIndex.idOf(key).contains(r.id))) store(key, cached, r.id)
      // ...and it is not another writer's change.
      if (!held.exists(LeanRecords.equal(_, cached))) changed(r.id)
    }
    if (applied) touch()
    else logger.debug(s"MovieCache: skipped a change-stream read of '${key.cleanTitle}' taken before this cache's own write of it.")
  }

  /** Apply a change confined to some venues' showtimes from those venues alone: their slots
   *  stripped again, the rest of the resident row untouched — exactly what [[applyUpsert]] of the
   *  whole film would store. Declined, having stored nothing, when the film is not resident or a
   *  venue holds other slots than the resident ones (a slot came or went), and the caller hands
   *  over the whole film instead. Fenced like [[applyUpsert]]. */
  private[services] def applyVenueSlots(venues: VenueSlots, mark: Long): VenueVerdict = {
    import ChangeStreamMetrics.VenueDecline as Why
    corpusIndex.keyOf(venues.filmId).flatMap(key => get(key).map(key -> _)) match {
      case None => VenueVerdict.Declined(Why.CacheNotResident)
      case Some((key, resident)) =>
        // The venues' slots replace the resident ones whole, read as a whole-film read reads them, so
        // the row stored is that read's exactly when the venues hold the same slots it does.
        val fits = venues.atCinemas.forall { case (cinema, slots) =>
          resident.data.keysIterator.collect { case s @ models.CinemaShowing(`cinema`, _) => s }.toSet == slots.map(_._1).toSet
        }
        if (!fits) VenueVerdict.Declined(Why.CacheSlotsDiffer)
        else {
          // Only the venues' slots are stripped again, not the whole row as `forCache` would: a
          // one-venue change to a wide film stripped thousands of slots — 4% of the US worker's CPU
          // (JFR 2026-10-01). The slot set is unchanged (checked above).
          val applied = repository.writeFence.ifUndisturbed(venues.filmId.value, mark) {
            val slots   = venues.atCinemas.valuesIterator.flatten.map { case (source, slot) =>
              source -> residentShape.kept(resident.data.get(source), residentShape.slot(slot)) }.toSeq
            // Venues read back as the row holds them (the echo of this cache's own write) leave the row as it is.
            if (!slots.forall { case (source, slot) => resident.data.get(source).exists(_ eq slot) }) {
              store(key, resident.copy(data = resident.data ++ slots), venues.filmId)
              if (!slots.forall { case (source, slot) => resident.data.get(source).exists(LeanRecords.slotsEqual(_, slot)) })
                changed(venues.filmId)
            }
          }
          if (applied) touch()
          VenueVerdict.Applied
        }
    }
  }

  /** Apply an out-of-band DELETE from the change stream: drop the mirrored row whose
   *  source `_id` was removed (a fold/merge loser, an `UnscreenedCleanup` removal, a
   *  re-key's old id). The stream carries only the `_id`, so map it back to the CacheKey
   *  the row was stored under via `idFor`. Applying deletes incrementally is what lets the
   *  backstop rehydrate stop being the ONLY thing that catches them; a rare drifted /
   *  duplicate key that doesn't map is still swept by the backstop. */
  private[services] def applyDelete(id: FilmId): Unit = {
    corpusIndex.keyOf(id)
      .foreach { k =>
        evict(k)
        // An out-of-band Mongo delete arriving via the change stream — the mirror
        // of a fold/UnscreenedCleanup/re-key removal another writer made. The only
        // signal that a cache row vanished for a reason NOT originating on this node.
        RemovalAudit.filmRemoved("cache.applyDelete", id.value, reason = "change-stream-delete")
        touch()
        changed(id)
      }
  }

  /** Re-read while no corpus read has ever completed. A boot read that failed past its attempts
   *  (Mongo slow, or briefly gone) left the cache empty, and only the change stream — rows written
   *  after boot — and the backstop hours later filled it: every quiescent row was invisible to the
   *  fold and the settle meanwhile. Once hydrated this is one field read. */
  private[movies] def coldRetryTick(): Unit =
    if (!wholeCorpusRead) {
      logger.warn("MovieCache cold-retry: no corpus read has completed yet — reading again.")
      rehydrate(); ()
    }

  def start(): Unit = {
    refreshScheduler.scheduleAtFixedRate(
      () => Try(coldRetryTick()).recover { case exception => logger.warn(s"MovieCache cold-retry tick failed: ${exception.getMessage}") },
      CaffeineMovieCache.ColdRetryInterval.toSeconds, CaffeineMovieCache.ColdRetryInterval.toSeconds, TimeUnit.SECONDS)
    watchHandle = repository.watchChangesFencedWithVenues(applyUpsert, applyDelete, applyVenueSlots)
    logger.info(
      s"MovieCache incremental change-stream watch ${if (watchHandle.isDefined) "active" else "unavailable — backstop only"}; " +
      s"backstop rehydrate every ${BackstopIntervalSeconds}s.")
    refreshScheduler.scheduleAtFixedRate(
      () => Try(rehydrate()).recover {
        case exception => logger.warn(s"MovieCache rehydrate tick failed: ${exception.getMessage}")
      },
      BackstopIntervalSeconds, BackstopIntervalSeconds, TimeUnit.SECONDS
    )
  }

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    refreshScheduler.shutdown()
  }
}

object CaffeineMovieCache {
  /** The documents a rehydrate read under one key, and the record the cache keeps for them: the
   *  first document's, until the documents' union replaces it (see `rehydrateFrom`). */
  private final case class HydratedRows(ids: Seq[FilmId], record: MovieRecord) {
    def and(more: Seq[FilmId]): HydratedRows = copy(ids = ids ++ more)
  }
  private object HydratedRows {
    def apply(id: FilmId, record: MovieRecord): HydratedRows = HydratedRows(Seq(id), record)
  }

  /** How often a cache with no completed corpus read reads again — tight, because the state it
   *  recovers from is a cache missing every quiescent row, not drift. */
  val ColdRetryInterval: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.FiniteDuration(30, TimeUnit.SECONDS)
}
