package services.movies

import models.{CacheKey, FilmId, StoredMovieRecord}

import models.{MovieRecord, ShowtimesDigest}
import settings.{BootHydrateMaxAttempts, BootHydrateRetryInterval}
import tools.TickRun

import scala.concurrent.duration.FiniteDuration

/**
 * The [[CorpusMirror]]'s corpus reads: the boot hydrate (retried while the store answers empty, its pages handed to
 * the boot's other readers) and the rehydrates (the cold retry and the backstop), each a whole read of the store put
 * into the resident corpus through its funnels, with the keys gone from the store evicted. `logger` is the cache's,
 * which the hydrate has always logged as.
 */
private[movies] final class CorpusHydrate(
  corpus:                 MirroredCorpus,
  bootHydrateMaxAttempts: BootHydrateMaxAttempts,
  bootHydrateRetry:       BootHydrateRetryInterval,
  cacheMetrics:           CacheSyncMetrics,
  bootReaders:            Seq[BootCorpusReader],
  sleep:                  FiniteDuration => Unit,
  logger:                 play.api.Logger
) {
  import corpus.{evict, held as get, index as corpusIndex, normalizer, repository, shape as residentShape, store, touch}

  // Set once a rehydrate's corpus read has been answered whole — an empty corpus included — and
  // never cleared: from then on a failed read only leaves the cache a little stale. Initialised
  // with the mirror, before the cache's boot hydrate writes it.
  @volatile private var wholeCorpusRead = false
  def hydrated: Boolean = wholeCorpusRead

  // Hydrate from Mongo on construction (the cache calls this, through its mirror, from its constructor).
  // Synchronous: Wiring builds the cache during `start()`, so the first HTTP request only lands after the initial
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
  def bootHydrate(): Unit = {
    // Each read's pages go to the boot readers as they are read; the first complete, non-empty read is
    // the corpus. An empty answer is not the corpus: at boot it is as likely a Mongo that is not ready
    // yet as an empty store, so its pages are forgotten and the hydrate reads again.
    val offer: Seq[StoredMovieRecord] => Unit = page => bootReaders.foreach(_.bootPage(page))
    var attempt = 0
    var read    = rehydrateFrom(offer).getOrElse(0)
    while (read == 0 && attempt < bootHydrateMaxAttempts.value) {
      bootReaders.foreach(_.bootReadEnded(BootReadEnd.Retrying))
      attempt += 1
      sleep(bootHydrateRetry.value) // an interrupt (shutdown mid-boot) ends the construction
      read = rehydrateFrom(offer).getOrElse(0)
    }
    bootReaders.foreach(_.bootReadEnded(if (read > 0) BootReadEnd.Whole else BootReadEnd.GaveUp))
  }

  /** Put every stored row and evict the keys gone from the store; how many rows it put. An incomplete
   *  read (a page the scan could not read is skipped whole) changes nothing — no row put, none
   *  evicted: its missing films are not gone. The next backstop tick reads again. */
  def rehydrate(): Int = rehydrateFrom(_ => ()).getOrElse(0)

  /** [[rehydrate]], as a tick's run: [[TickRun.Incomplete]] when the read could not complete or was refused (logged). */
  def rehydrateTick(): TickRun = TickRun.completedWhen(rehydrateFrom(_ => ()).isDefined)

  /** [[rehydrate]], handing each page of its read to `onPage` as it is read.
   *
   *  Streamed, not collected: each page is stripped to what the cache keeps ([[forCache]]) as it
   *  arrives and then dropped, so the read never holds the whole stitched corpus — every showtime
   *  of every film — at once. Collected, it did, for the whole read and then for the boot readers'
   *  derivation after it: long enough at boot that the corpus's showtimes were promoted into the old
   *  generation only to die there (see [[BootCorpusReader]]). Nothing is put or evicted until the
   *  read is complete. */
  private def rehydrateFrom(onPage: Seq[StoredMovieRecord] => Unit): Option[Int] = {
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
    // None: a read that could not complete, or an empty answer refused — nothing put or evicted.
    readByKey(onPage, findingAll).flatMap { case (byKey, rows) =>
      val tFindAllMs = findingAll.millis
      Option.unless(refusesEmpty(rows, tFindAllMs)) { putAll(byKey, rows, marks, tFindAllMs); rows }
    }
  }

  /** Put the `rows` read, by key, into the resident corpus and evict the keys gone from it — each under the marks
   *  taken before the read — and say what the backstop caught. */
  private def putAll(byKey: java.util.LinkedHashMap[CacheKey, CorpusHydrate.HydratedRows], rows: Int,
                     marks: FilmWriteFence.Marks, tFindAllMs: Long): Unit = {
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
    val cachedSize = corpus.heldCount
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
  private def populate(byKey: java.util.LinkedHashMap[CacheKey, CorpusHydrate.HydratedRows], marks: FilmWriteFence.Marks): Int = {
    var changed = 0
    byKey.forEach { (k, held) =>
      // Several documents under one key are the same film twice; the lowest id is the
      // survivor, deterministically, and the others are reconciled later.
      val survivor = held.ids.minBy(_.value)
      repository.writeFence.ifUndisturbed(survivor.value, marks.of(survivor.value)) {
        if (!get(k).exists(ShowtimesDigest.leanEqual(_, held.record))) changed += 1
        store(k, held.record, survivor)
      }
    }
    changed
  }

  /** Evict every resident key the read no longer holds; how many. Only a key whose film nobody wrote
   *  since the snapshot: one this cache created or retitled meanwhile is absent from the snapshot
   *  because it is NEWER, not gone. */
  private def evictGone(byKey: java.util.LinkedHashMap[CacheKey, CorpusHydrate.HydratedRows], marks: FilmWriteFence.Marks): Int = {
    corpus.heldKeys.filterNot(byKey.containsKey).count { k =>
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
      : Option[(java.util.LinkedHashMap[CacheKey, CorpusHydrate.HydratedRows], Int)] = {
    import scala.jdk.CollectionConverters._
    val byKey = new java.util.LinkedHashMap[CacheKey, CorpusHydrate.HydratedRows]()
    var rows  = 0
    val scan  = repository.foreachPage { page =>
      page.foreach { row =>
        val held = CorpusHydrate.HydratedRows(row.id, residentShape.of(row.record))
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
}

private[movies] object CorpusHydrate {
  /** The documents a rehydrate read under one key, and the record the cache keeps for them: the
   *  first document's, until the documents' union replaces it (see `rehydrateFrom`). */
  private final case class HydratedRows(ids: Seq[FilmId], record: MovieRecord) {
    def and(more: Seq[FilmId]): HydratedRows = copy(ids = ids ++ more)
  }
  private object HydratedRows {
    def apply(id: FilmId, record: MovieRecord): HydratedRows = HydratedRows(Seq(id), record)
  }
}
