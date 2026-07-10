package services.readmodel

import models.{CityScreening, ResolvedMovie}
import play.api.Logging
import services.Stoppable
import services.movies.{MovieRepository, StoredMovieRecord}
import tools.{DaemonExecutors, Env}

import java.util.concurrent.TimeUnit
import scala.util.Try

/**
 * Maintains the denormalised read model (`web_movies` + `web_screenings`) from
 * the source `movies` collection.
 *
 * Two live mechanisms keep it current, mirroring `MovieCache`'s sync design:
 *
 *  1. INCREMENTAL — subscribes to the `movies` change stream
 *     (`MovieRepository.watchUpserts`); each changed row is re-projected and the
 *     resulting documents are diffed against the last projection so only the documents
 *     that actually changed are written. With the persisted resume token this also
 *     replays every upsert missed while the worker was down.
 *  2. ORPHAN PRUNE (backstop) — a cheap, frequent id-only sweep (`pruneOrphans`)
 *     that removes derived documents whose source film has vanished or was re-keyed
 *     (the change stream delivers no deletes; a fold-victim / `UnscreenedCleanup`
 *     removal is reconciled here). It re-projects nothing, so it can't spike CPU.
 *
 * The full re-projection (`reconcile`) is NOT scheduled — it was the periodic
 * ~1-core whole-corpus burst that drained the worker's CPU-credit balance, and the
 * resume-token change stream made it redundant (proven by a sustained did_work=false
 * on the reconcile-sweep metric across restarts). It survives only as an explicit
 * one-shot seed/backfill primitive (fixture/e2e read-model seeding, `BackfillReadModel`).
 *
 * Minimal writes: the last-projected document per film is kept in memory (hydrated
 * from the read model at boot, so a restart rewrites only what changed since
 * the projector last ran). A showtime-only edit re-projects to the same
 * `ResolvedMovie` (skipped) and the same screenings except one (written) — so
 * the web's change-stream delta is that one screening document. The movie document is
 * always written before its screenings, so a consumer joining screening→movie
 * sees the metadata first; the web join also tolerates the reverse order, so
 * neither side depends on it.
 *
 * The change-stream callback and the prune sweep are serialised by one lock,
 * so the in-memory last-projection state needs no further synchronisation.
 */
class ReadModelProjector(
  movieRepository: MovieRepository,
  writer:    ReadModelWriter,
  reader:    ReadModelReader,
  metrics:   ReadModelProjectionMetrics = ReadModelProjectionMetrics.noop,
  scheduler: java.util.concurrent.ScheduledExecutorService = DaemonExecutors.scheduler("read-model-projector")
) extends Stoppable with Logging {
  import ReadModelProjectionMetrics.{Op, ReconcileKind, Target}

  // Diff state for minimal writes: the CONTENT HASH of the last-projected document per
  // film (and per screening), NOT the full document. The projection is deterministic, so
  // an unchanged row hashes to the same value and its write is skipped; keeping only the
  // hash resident holds the whole read model for diffing at a few bytes per doc instead of
  // the full ResolvedMovie / CityScreening object graph (which also lives in Mongo — this
  // is a pure duplicate we don't need to keep inflated). A 32-bit hash collision
  // (astronomically rare) would skip one genuine write, leaving a stale doc until the
  // row's next real change re-projects it — self-healing, never permanently wrong.
  private val lastMovie      = scala.collection.mutable.Map.empty[String, Int]
  private val lastScreenings = scala.collection.mutable.Map.empty[String, Map[String, Int]]
  // Metadata-reuse cache (optimisation #1): per SOURCE ROW (keyed by the anchor
  // `ReadModelProjection.filmId`, stable across the display-title split), the
  // `metadataHash` of the last projection plus the projected `ResolvedMovie` variant(s)
  // it produced, in `projectAll` order. The projected metadata is a pure function of the
  // row's cinema STRUCTURE, not its SHOWTIMES (see `ReadModelProjection.metadataHash`), so
  // a showtime-only change re-uses these and recomputes only the cheap screenings half —
  // skipping resolve/synopsisByCity/ratings, the expensive projection work the reproject/
  // enrich churn spends its CPU on. Unlike `lastMovie`/`lastScreenings` (which hold a few
  // bytes per doc) this holds the ResolvedMovie object graph resident — but only for rows
  // actually projected, ~corpus-sized (few MB on the 320m heap), and evicted on prune.
  private val lastMetadata   = scala.collection.mutable.Map.empty[String, (Int, Seq[ResolvedMovie])]
  private val lock           = new AnyRef

  // The cheap id-only orphan prune runs FREQUENTLY (deletes/re-keys the change stream
  // drops must clear within a tick). The expensive full re-projection is no longer
  // scheduled at all (the resume-token change stream made it redundant — see the class
  // doc); it survives only as the explicit `reconcile()` seed/backfill primitive.
  private val PruneSeconds = Env.positiveLong("KINOWO_READMODEL_PRUNE_SECONDS", 1800L)      // 30 min
  // Deferred off the boot path — running a full scan synchronously at `start()` stacked a
  // second scan onto the cache hydrate + first scrape on a cold JVM (the boot CPU drain).
  private val PruneBootDelaySeconds = Env.positiveLong("KINOWO_READMODEL_PRUNE_BOOT_DELAY_SECONDS", 300L)
  @volatile private var watchHandle: Option[AutoCloseable] = None

  def enabled: Boolean = writer.enabled && movieRepository.enabled

  /** Apply one source-row change from the change stream. */
  def onMovieUpsert(stored: StoredMovieRecord): Unit = lock.synchronized { project(stored); () }

  // Caller holds `lock`. Project the row and write only what changed, movie
  // document before screenings. A row whose enrichment hasn't concluded
  // (`readyToProject` false) is held back — publishing the pre-enrichment,
  // yearless row is exactly what creates the duplicate `foo|` + `foo|2025`
  // cards, so it must never reach the read model until it has settled.
  /** Returns the number of derived documents actually (re)written for this row —
   *  0 when the projection was byte-identical to what's already stored. The
   *  reconcile sweep sums this to know whether a full re-projection caught
   *  anything the change stream missed. */
  private def project(stored: StoredMovieRecord): Int = {
    if (!stored.record.readyToProject) return 0
    // A row fans out into one card per display-title variant (Cyrillic / English
    // / banner-prefixed listings of one film); the common single-title row yields
    // exactly one. Each variant card is diffed and written independently. Time the
    // pure projection (CPU-bound, no I/O) — its rate() in centi-cores attributes how
    // much worker CPU the reproject/enrich churn spends here (the credit-floor driver).
    val projectStart = System.nanoTime()
    val variants     = projectReusingMetadata(stored)
    metrics.recordProject((System.nanoTime() - projectStart) / 1e9)
    var written = 0
    variants.foreach { case (movie, screenings) =>
      if (!lastMovie.get(movie._id).contains(movie.##)) {
        writer.upsertMovie(movie)
        metrics.recordWrite(Target.Movie, Op.Upsert, 1)
        lastMovie.update(movie._id, movie.##)
        written += 1
      }
      written += diffScreenings(movie._id, screenings)
    }
    written
  }

  // Caller holds `lock`. Project the row, REUSING the cached `ResolvedMovie` metadata when
  // the row's metadata inputs are unchanged (a showtime-only change at an already-present
  // cinema — the overwhelming common case under reproject/enrich showtime churn). Correctness:
  // an unchanged `metadataHash` guarantees an unchanged metadata output AND an unchanged
  // display-title variant partition (the hash covers the whole record bar showtimes, and no
  // metadata accessor reads showtimes), so `screeningsAll` — which re-runs the SAME `variants`
  // split in the SAME order — lines up 1:1 with the cached movie(s). The size guard is a
  // belt-and-suspenders fallback to a full re-projection: it can never pair a movie with the
  // wrong screenings. `screeningsAll` is byte-identical to `projectAll(...).map(_._2)` but skips
  // the resolve/synopsisByCity/ratings work — that skip is the whole optimisation.
  private def projectReusingMetadata(stored: StoredMovieRecord): Seq[(ResolvedMovie, Seq[CityScreening])] = {
    val rowKey = ReadModelProjection.filmId(stored)
    val hash   = ReadModelProjection.metadataHash(stored)
    lastMetadata.get(rowKey) match {
      case Some((cachedHash, movies)) if cachedHash == hash =>
        val screenings = ReadModelProjection.screeningsAll(stored)
        if (screenings.sizeIs == movies.size) {
          metrics.recordMetadataProjection(reused = true)
          movies.zip(screenings)
        } else recomputeMetadata(rowKey, hash, stored)
      case _ => recomputeMetadata(rowKey, hash, stored)
    }
  }

  private def recomputeMetadata(rowKey: String, hash: Int, stored: StoredMovieRecord): Seq[(ResolvedMovie, Seq[CityScreening])] = {
    val variants = ReadModelProjection.projectAll(stored)
    lastMetadata.update(rowKey, hash -> variants.map(_._1))
    metrics.recordMetadataProjection(reused = false)
    variants
  }

  /** Returns the number of screening documents written (upserts + deletes). */
  private def diffScreenings(filmId: String, next: Seq[CityScreening]): Int = {
    val nextById = next.map(s => s._id -> s).toMap
    val previous = lastScreenings.getOrElse(filmId, Map.empty)
    var upserted = 0
    nextById.foreach { case (id, s) => if (!previous.get(id).contains(s.##)) { writer.upsertScreening(s); upserted += 1 } }
    val deletes = previous.keysIterator.filterNot(nextById.contains).toSeq
    deletes.foreach(writer.deleteScreening)
    if (upserted > 0)        metrics.recordWrite(Target.Screening, Op.Upsert, upserted)
    if (deletes.nonEmpty)    metrics.recordWrite(Target.Screening, Op.Delete, deletes.size)
    if (nextById.isEmpty) lastScreenings.remove(filmId) else lastScreenings.update(filmId, nextById.view.mapValues(_.##).toMap)
    upserted + deletes.size
  }

  // Only `reconcile` calls this — a film whose source row vanished or was re-keyed
  // (its filmId changed) is dropped wholesale. `recordFilmPruned` is the link-break
  // signal; the document deletes are also counted as reprojection writes.
  private def deleteFilm(filmId: String): Unit = {
    writer.deleteMovie(filmId)
    val screeningIds = lastScreenings.getOrElse(filmId, Map.empty).keys.toSeq
    screeningIds.foreach(writer.deleteScreening)
    metrics.recordWrite(Target.Movie, Op.Delete, 1)
    if (screeningIds.nonEmpty) metrics.recordWrite(Target.Screening, Op.Delete, screeningIds.size)
    metrics.recordFilmPruned(1)
    lastMovie.remove(filmId)
    lastScreenings.remove(filmId)
    // Evict the metadata-reuse entry. Keyed by the row's ANCHOR filmId, which equals the
    // pruned `filmId` for a single-variant row (the common case); a split row's anchor may
    // differ, so this is best-effort — a lingering entry is harmless (a future row reusing the
    // same key with a matching hash would reuse identical metadata) and bounded by corpus size.
    lastMetadata.remove(filmId)
  }

  /** One reconciliation pass over the whole corpus.
   *
   *  `reproject = true` — the FULL sweep: re-project every ready row (the diff keeps
   *  writes minimal — only genuinely changed documents are written) AND prune. This
   *  is the expensive path (projecting ~1400 rows is a ~1-core burst that, on the old
   *  30-min cadence, filled the 320m heap → GC thrash → CPU-credit starvation). It is
   *  NO LONGER SCHEDULED — the resume-token change stream now catches the upserts it
   *  used to (its only unique job); it runs only via the explicit `reconcile()`
   *  seed/backfill primitive (test/fixture seeding).
   *
   *  `reproject = false` — the CHEAP sweep (`pruneOrphans`): build the live-id set from
   *  `ReadModelProjection.filmIds` (no projection) and prune only. This is the frequent
   *  backstop for DELETES / re-keys, which the change stream drops (`watchUpserts` nulls
   *  onDelete) — the film-id is `sanitize(title)|resolvedYear`, many source rows to one
   *  card, so a single source delete can't be mapped to a card without a reverse index;
   *  the set-difference prune is the correct, cheap way to reconcile them.
   *
   *  Self-healing: the prune diffs the ACTUAL read-model ids
   *  (`reader.findAllMovieIds`/`findAllScreeningRefs`) against the live source, NOT this
   *  process's in-memory `lastMovie` — a film a PRIOR worker process wrote and re-keyed
   *  is invisible to a memory-based prune, which is how a re-key across a restart leaked a
   *  duplicate card permanently. Streaming `foreachRecord` (not a whole-corpus `findAll`)
   *  and id-only read-model reads keep the whole corpus off the heap.
   *
   *  A row that fails to project must not abort the prune (the prune is what removes the
   *  duplicates), so each projection is guarded individually. */
  private def sweep(reproject: Boolean): Unit = lock.synchronized {
    // Log-only label. The reconcile-sweep metric now tracks ONLY the prune (the live,
    // scheduled backstop); the reproject path survives as a test/backfill seed and is
    // no longer metered — the reproject retirement gate it fed has been removed.
    val kind = if (reproject) "reproject" else ReconcileKind.Prune
    val liveIds = scala.collection.mutable.Set.empty[String]
    var reprojected = 0
    val scanComplete = movieRepository.foreachRecord { row =>
      if (row.record.readyToProject) {
        liveIds ++= ReadModelProjection.filmIds(row)
        if (reproject)
          try reprojected += project(row)
          catch { case exception: Throwable =>
            logger.warn(s"read-model $kind: a row failed to project, continuing: ${exception.getMessage}") }
      }
    }
    var prunedFilms      = 0
    var prunedScreenings = 0
    if (!scanComplete) {
      // The source scan failed mid-way (a Mongo batch read threw after its retries —
      // typically a server-selection / socket timeout while the worker is CPU-throttled,
      // the 2026-06-29 01:34–02:19 served-films flap). `liveIds` is therefore a TRUNCATED
      // view, so pruning on it would delete every read-model card whose source row this
      // scan never reached. Keep the (idempotent) projections we did make and SKIP the
      // destructive prune; the next tick reconciles cleanly once reads recover.
      logger.warn(s"read-model $kind: source scan was incomplete (Mongo read failed mid-scan) — " +
        "skipping the prune this tick to avoid deleting live read-model rows; will retry next tick.")
    } else {
      // Prune off id-only projections — the prune reads ids/filmIds, never payloads.
      reader.findAllMovieIds().iterator.filterNot(liveIds).foreach { id => deleteFilm(id); prunedFilms += 1 }
      reader.findAllScreeningRefs().iterator.filterNot(ref => liveIds(ref.filmId)).foreach { ref =>
        writer.deleteScreening(ref._id)
        metrics.recordWrite(Target.Screening, Op.Delete, 1)
        lastScreenings.updateWith(ref.filmId)(_.map(_ - ref._id).filter(_.nonEmpty))
        prunedScreenings += 1
      }
    }
    // Measurement (prune only): a `prune` sweep with didWork=true is the deletes/re-keys
    // the change stream can't deliver. Surfaced as kinowo_worker_readmodel_reconcile_sweeps.
    val didWork = reprojected > 0 || prunedFilms > 0 || prunedScreenings > 0
    if (!reproject) metrics.recordReconcileSweep(ReconcileKind.Prune, didWork)
    logger.info(s"read-model $kind sweep: reprojected $reprojected doc(s), pruned $prunedFilms film(s) + " +
      s"$prunedScreenings orphan screening(s)${if (scanComplete) "" else " [scan INCOMPLETE — prune skipped]"}.")
  }

  /** Full re-projection + prune. NOT scheduled — the resume-token change stream made the
   *  periodic reproject redundant. Kept as an explicit one-shot seed/backfill primitive:
   *  fixture/e2e read-model seeding calls it to project a settled corpus synchronously
   *  (it stitches split films via `foreachRecord`, which a per-row `onMovieUpsert` seed
   *  would not). Mirrors `scripts.BackfillReadModel`. */
  def reconcile(): Unit = sweep(reproject = true)

  /** Cheap id-only orphan prune — the frequent backstop for deleted/re-keyed rows. */
  def pruneOrphans(): Unit = sweep(reproject = false)

  def start(): Unit = if (enabled) {
    // Seed the last-projection state from the derived collections, so a restart
    // doesn't rewrite documents that are already correct.
    lock.synchronized {
      reader.findAllMovies().foreach(m => lastMovie.update(m._id, m.##))
      reader.findAllScreenings().groupBy(_.filmId).foreach { case (fid, ss) =>
        lastScreenings.update(fid, ss.map(s => s._id -> s.##).toMap)
      }
    }
    // The change-stream watch covers live changes from now on (and, via the persisted
    // resume token, replays every upsert missed while the worker was down); the seeded
    // state above means incremental writes are no-ops for already-correct documents. Only
    // the cheap orphan prune is scheduled — the full reproject was retired (see class doc).
    watchHandle = movieRepository.watchUpserts(onMovieUpsert)
    // Cheap orphan prune: frequent, no per-row re-projection (can't spike CPU). Deferred
    // off the boot path so it doesn't compete with boot hydrate + the first scrape.
    scheduler.scheduleAtFixedRate(
      () => Try(pruneOrphans()).recover { case exception => logger.warn(s"read-model prune tick failed: ${exception.getMessage}") },
      PruneBootDelaySeconds, PruneSeconds, TimeUnit.SECONDS)
    logger.info(s"ReadModelProjector started; orphan-prune every ${PruneSeconds}s (first in ${PruneBootDelaySeconds}s); " +
      s"no periodic reproject (retired); change-stream watch " +
      s"${if (watchHandle.isDefined) "active" else "unavailable — orphan-prune only"}.")
  } else logger.info("ReadModelProjector disabled (read model or movies repository not enabled).")

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}
