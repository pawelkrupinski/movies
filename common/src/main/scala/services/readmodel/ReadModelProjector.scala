package services.readmodel

import models.{CityScreening, ResolvedMovie}
import play.api.Logging
import services.Stoppable
import services.movies.{ChangeStreamLiveness, MovieRepository, StoredMovieRecord}
import settings.{ReadModelPruneBootDelay, ReadModelPruneInterval, ShareCardFirstHold}
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.util.Try

/**
 * Maintains the denormalised read model (`web_movies` + `web_screenings`) from
 * the source `movies` collection.
 *
 * Two live mechanisms keep it current, mirroring `MovieCache`'s sync design:
 *
 *  1. INCREMENTAL — subscribes to the `movies` change stream
 *     (`MovieRepository.watchChanges`); each changed row is re-projected and the
 *     resulting documents are diffed against the last projection so only the documents
 *     that actually changed are written. With the persisted resume token this also
 *     replays every upsert missed while the worker was down.
 *  2. ORPHAN PRUNE (backstop) — a cheap, frequent id-only sweep (`pruneOrphans`)
 *     that removes derived documents whose source film has vanished or was re-keyed
 *     and whose delete event this process never applied (it was down past the resume
 *     window, or the card was written by an earlier process — see `onMovieDelete`). It re-projects nothing, so it can't spike CPU.
 *
 * A deploy that changes what the projection DERIVES from an unchanged row reaches the stored
 * cards through neither: the DERIVATION PASS (`advanceDerivationPass`) re-projects the whole
 * corpus once, paced, when the store records another derivation ([[ReadModelDerivation]]) — only
 * the cards, off the slots-only read, when all that moved since was what a card shows.
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
  scheduler: java.util.concurrent.ScheduledExecutorService = DaemonExecutors.scheduler("read-model-projector"),
  cpuClock:  tools.ThreadCpuClock = tools.ThreadCpuClock.threadMxBean,
  // Where each card's `shareCard` comes from, and the first-publish gate — see [[ShareCardLedger]].
  shareCards: ShareCardLedger = ShareCardLedger.none,
  // How long a brand-new card may be held back waiting for its share card before it is published
  // anyway, with the fallback image and `shareCardPending` set. Never indefinitely.
  firstCardHold: ShareCardFirstHold = ShareCardFirstHold(ReadModelProjector.DefaultFirstCardHold),
  // No default: the rolling content check numbers its slice by this clock, so a spec left on the
  // wall clock re-projected its row in the half-hours that row's slice came up (Main red, 2026-09-25).
  clock:     java.time.Clock,
  // How often the id-only orphan prune runs, and how long after boot the first one waits
  // (`KINOWO_READMODEL_PRUNE_SECONDS` / `…_BOOT_DELAY_SECONDS`, resolved by the worker's root).
  pruneInterval:  ReadModelPruneInterval  = ReadModelProjector.DefaultPruneInterval,
  pruneBootDelay: ReadModelPruneBootDelay = ReadModelProjector.DefaultPruneBootDelay,
  // Blocks until the change stream has applied what it had in flight when a sweep let go of the
  // lock — what tells a heal the stream would have made anyway from a real miss (see `verdictOnHeals`).
  awaitStreamApplied: ChangeStreamLiveness => Unit = ReadModelProjector.awaitStreamApplied(_),
  // Which derivation the stored read model was last re-projected whole under — see
  // [[ReadModelDerivationMarker]] and `advanceDerivationPass`. `none` owes no pass.
  derivationMarker: ReadModelDerivationMarker = ReadModelDerivationMarker.none,
  // Every derivation this code knows, the last its own: what a store behind it is owed (see
  // [[ReadModelDerivation.owedSince]]). A parameter so a spec can stage a history.
  derivationHistory: Seq[Derivation] = ReadModelDerivation.History,
  // Confidence-gated ratings (identity phase 3): what each card's ratings are served as. `off`
  // — the card as projected — unless the worker's composition root switches it on.
  ratingGate: services.identity.RatingGate = services.identity.RatingGate.off
) extends Stoppable with Logging {
  // The projection keys rows by the repository's own `_id` formula, so it must
  // fold titles with the same rules the repository writes under — take them from
  // it rather than accepting a second, separately-wired copy that could disagree.
  private val normalizer: services.movies.TitleNormalizer = movieRepository.normalizer

  import ReadModelProjectionMetrics.{HealTrigger, Op, ProjectTrigger, PruneReason, ReconcileKind, RetireReason, Target}
  import ReadModelProjector.{ContentSlices, DerivationPass, DerivationPassTickSeconds}

  // Diff state for minimal writes: the CONTENT HASH of the last-projected document per
  // film (and per screening), NOT the full document. The projection is deterministic, so
  // an unchanged row hashes to the same value and its write is skipped; keeping only the
  // hash resident holds the whole read model for diffing at a few bytes per doc instead of
  // the full ResolvedMovie / CityScreening object graph (which also lives in Mongo — this
  // is a pure duplicate we don't need to keep inflated). A 32-bit hash collision
  // (astronomically rare) would skip one genuine write, leaving a stale doc until the
  // row's next real change re-projects it — self-healing, never permanently wrong.
  // Per card: a hash per PART of what was last written, so a rewrite can name what moved.
  private val lastMovie      = scala.collection.mutable.Map.empty[String, CardHash]
  // Per SOURCE ROW: the metadata hash at which a heal looked and found nothing to write, and
  // the venues it found to be phantoms.
  // The venue check reads SLOTS ONLY (the sweep must not pull the whole screenings
  // collection), so it cannot tell a slot with no showtimes from a venue whose row is
  // missing, and it asks about both. A slot that is simply spent therefore reads as absent
  // on EVERY sweep: PL re-projected the same ~333 rows every 30 minutes for as long as this
  // ran (2026-09-08). Once a heal has re-projected a row and written nothing, the absence
  // was a phantom of that slots-only view — remember it, and stop asking until the row's
  // metadata moves or a projection WRITES that venue, which is exactly when the answer can
  // change. Not on every projection of the row: a showtime moving at its other venue, or the
  // catch-up re-projecting a row the cursor was late with, leaves a spent slot spent, and
  // wiping the note there re-asked 13 Polish rows on a sweep over an unchanged corpus.
  private val healedClean    = scala.collection.mutable.Map.empty[String, (Int, Set[String])]
  // Per SOURCE ROW: the card ids its last projection produced. What lets a re-projection
  // retire the variant card a vanished listing no longer earns, and a delete event retire
  // every card of a row that was merged away — so the prune finds nothing (the rule).
  private val lastCardsByRow = scala.collection.mutable.Map.empty[String, Set[String]]
  // Per card: what was last written for each of its screenings rows — see [[WrittenScreening]].
  private val lastScreenings = scala.collection.mutable.Map.empty[String, Map[String, WrittenScreening]]
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
  // THE FIRST-PUBLISH GATE. Per card not yet served (see `gate`): its source row and the instant its hold
  // ends. A link-preview scraper caches `og:image` for about a month on its first fetch, so a card
  // must not go public before its share card exists — but no film waits longer than
  // `firstCardHold` for one (see [[releaseExpiredHolds]]).
  private val held           = scala.collection.mutable.Map.empty[String, HeldCard]
  // Cards published by an expired hold that still have no share card: `shareCardPending` on their
  // documents, and seeded from them at boot, so a later projection keeps the mark until the card lands.
  private val pendingCards   = scala.collection.mutable.Set.empty[String]
  private val lock           = new AnyRef

  // The cheap id-only orphan prune runs FREQUENTLY (deletes/re-keys the change stream
  // drops must clear within a tick). The expensive full re-projection is no longer
  // scheduled at all (the resume-token change stream made it redundant — see the class
  // doc); it survives only as the explicit `reconcile()` seed/backfill primitive.
  private val PruneSeconds = pruneInterval.value.toSeconds
  // Deferred off the boot path — running a full scan synchronously at `start()` stacked a
  // second scan onto the cache hydrate + first scrape on a cold JVM (the boot CPU drain).
  private val PruneBootDelaySeconds = pruneBootDelay.value.toSeconds

  // Numbered by the clock, not from zero: a counter each process starts afresh checks
  // slice 0 again after every deploy, and on a day of hourly deploys the rest of the corpus
  // was never reached. Counting on from the clock keeps the next process on the next slice.
  private var sweepCount    = clock.instant().getEpochSecond / PruneSeconds
  @volatile private var watchHandle: Option[AutoCloseable] = None
  // The rows the change stream applied or deleted since the running prune sweep started, `None`
  // outside one: what tells a heal the stream would have made anyway from a miss (`verdictOnHeals`).
  private var appliedSinceSweep: Option[scala.collection.mutable.Set[String]] = None
  // Where the whole-corpus re-projection a derivation change owes stands — see `advanceDerivationPass`.
  private var derivationPass: DerivationPass = DerivationPass.Unchecked

  def enabled: Boolean = writer.enabled && movieRepository.enabled

  /** Apply one source-row change from the change stream. */
  def onMovieUpsert(stored: StoredMovieRecord): Unit =
    lock.synchronized {
      appliedSinceSweep.foreach(_ += stored.id.value)
      projectRow(stored, ProjectTrigger.Stream)
      // A second way out of a first-publish hold besides the task scheduled for its end: that
      // task may be claimed by a replica other than the one holding the card.
      // A row it could not read keeps its hold for the next sweep — see `releaseExpired`.
      if (held.nonEmpty) releaseExpired()
      ()
    }

  /** A row deleted or merged away: every card it produced goes with it, now — not at the
   *  next prune. The cards are what this process remembers producing for the row, plus
   *  anything in the read model under the row's id (a card a previous process wrote). */
  def onMovieDelete(id: services.movies.FilmId): Unit = lock.synchronized {
    appliedSinceSweep.foreach(_ += id.value)
    val remembered = lastCardsByRow.getOrElse(id.value, Set.empty)
    val underId    = lastMovie.keysIterator.filter(card => card == id.value || card.startsWith(id.value + "~")).toSet
    (remembered ++ underId).foreach(retireCard(_, RetireReason.RowDeleted))
    lastCardsByRow.remove(id.value)
    ()
  }

  /** Caller holds `lock`. [[project]] a row read whole. */
  private def projectRow(row: StoredMovieRecord, trigger: ProjectTrigger): Int =
    project(ReadModelProjection.partition(row, normalizer), trigger)

  // Caller holds `lock`. Project the row and write only what changed, movie
  // document before screenings. A row whose enrichment hasn't concluded
  // (`readyToProject` false) is held back — publishing the pre-enrichment,
  // yearless row is exactly what creates the duplicate `foo|` + `foo|2025`
  // cards, so it must never reach the read model until it has settled.
  /** Returns the number of derived documents actually (re)written for this row —
   *  0 when the projection was byte-identical to what's already stored. The
   *  reconcile sweep sums this to know whether a full re-projection caught
   *  anything the change stream missed. */
  private def project(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Int = {
    val rowId = partition.stored.id.value
    if (!partition.stored.record.readyToProject) {
      healedClean.remove(rowId)
      // A row that lost its readiness takes its cards with it (the prune would, later).
      lastCardsByRow.remove(rowId).foreach(_.foreach(retireCard(_, RetireReason.RowUnready)))
      dropHoldsNotProducedBy(rowId, Set.empty)
      return 0
    }
    // A row fans out into one card per display-title variant (Cyrillic / English
    // / banner-prefixed listings of one film); the common single-title row yields
    // exactly one. Each variant card is diffed and written independently. Measure the
    // pure projection BOTH ways: wall-clock answers "how long did it take" (the latency
    // histogram), thread CPU answers "how much CPU did it burn" (the credit-floor
    // attribution). They diverge under concurrency and under steal, so the one that may
    // be compared against process CPU is the CPU one — see `recordProject`.
    val wallStart = System.nanoTime()
    val cpuStart  = cpuClock.nanos()
    val variants  = projectReusingMetadata(partition).map { (movie, venues) => (movie, planScreenings(movie._id, venues)) }
    dropHoldsNotProducedBy(rowId, variants.map(_._1._id).toSet)
    metrics.recordProject(
      trigger,
      wallSeconds = (System.nanoTime() - wallStart) / 1e9,
      cpuSeconds  = (cpuClock.nanos() - cpuStart) / 1e9
    )
    // The WRITE half, timed separately from the computation above (`recordProject`) — see
    // `recordWriteBurst`. A wide release writes one document per (card, city, cinema)
    // through `writer.upsertMovie`/`diffScreenings`, sequentially; this is the phase that
    // scales with city count, not with resolve/synopsisByCity/ratings cost.
    val writeStart = System.nanoTime()
    var written = 0
    val now     = clock.millis()
    val publish = variants.flatMap { case (projected, screenings) =>
      gate(rowId, projected, screened = screenings.nonEmpty, now).map(_ -> screenings)
    }
    publish.foreach { case (movie, screenings) =>
      val hash    = CardHash.of(movie)
      val before  = lastMovie.get(movie._id)
      val changed = !before.contains(hash)
      if (changed) {
        writer.upsertMovie(movie)
        metrics.recordWrite(Target.Movie, Op.Upsert, 1)
        metrics.recordCardWrite(before.fold(Set.empty[String])(_.partsDifferingFrom(hash)))
        written += 1
      }
      written += diffScreenings(movie._id, screenings)
      // Remembered only once the screenings are written too: a throw in the screenings
      // write used to leave the card's hash "current", so the screenings were never
      // retried until the row changed again.
      if (changed) {
        lastMovie.update(movie._id, hash)
        shareCards.onProjected(movie, screened = screenings.nonEmpty)
      }
    }
    // A variant card this row produced last time and no longer does — its decorated
    // listing vanished — is retired here, by the path that knows, not by the prune. A card
    // still held by the first-publish gate was never produced.
    // A card held back while its unserved document stands (see `gate`) is not gone either.
    val produced = publish.map(_._1._id).toSet
    val before   = lastCardsByRow.getOrElse(rowId, Set.empty)
    val kept     = produced ++ before.filter(held.contains)
    (before -- kept).foreach(retireCard(_, RetireReason.VariantGone))
    lastCardsByRow.update(rowId, kept)
    // A venue this projection wrote is no phantom any more: if its row goes missing now, that
    // is a real loss the heal must repair.
    val served = kept.flatMap(card => lastScreenings.get(card).fold(Set.empty[String])(_.keySet))
    healedClean.updateWith(rowId)(_.map { case (hash, phantoms) => (hash, phantoms -- served) }.filter(_._2.nonEmpty))
    metrics.recordWriteBurst((System.nanoTime() - writeStart) / 1e9)
    written
  }

  /** Caller holds `lock`. The document to write for `projected` — its share card filled in — or
   *  None while the first-publish gate holds it back.
   *
   *  Only a card that is about to be SERVED for the first time — it has screenings, and this process
   *  has never written it with any (the read model does not hold it served: `lastMovie` and
   *  `lastScreenings` are seeded from it) — is gated: a card whose inputs merely CHANGE keeps its
   *  current share card until the new one exists, which [[ShareCardLedger.current]] already
   *  answers. That covers a film coming BACK on screen: its document outlived its screenings (and
   *  the daily prune its card), and nothing about the document changes when they return. A held
   *  card is asked for its share card on every projection, and published with `shareCardPending`
   *  once its hold has run out. */
  private def gate(rowId: String, projected: ResolvedMovie, screened: Boolean, now: Long): Option[ResolvedMovie] = {
    val id       = projected._id
    val served   = lastMovie.contains(id) && lastScreenings.get(id).exists(_.nonEmpty)
    val firstOne = screened && !served && !shareCards.readyToPublish(projected)
    val hold     = Option.when(firstOne)(held.getOrElseUpdate(id, HeldCard(rowId, now + firstCardHold.value.toMillis)))
    if (!firstOne) held.remove(id)
    val expired  = hold.exists(_.until <= now)
    if (firstOne && !expired) {
      shareCards.requestFirstCard(projected, java.time.Instant.ofEpochMilli(hold.get.until))
      None
    } else {
      if (expired) {
        held.remove(id)
        pendingCards += id
        logger.warn(s"share card: published $id without its card — none was ready within ${firstCardHold.value.toSeconds}s.")
      }
      Some(withShareCard(projected))
    }
  }

  /** Caller holds `lock`. `projected` as the read model serves it: its current share card filled
   *  in. A card published pending (see `gate`) whose share card has landed since stops being
   *  pending, and the ledger hears of it. */
  private def withShareCard(projected: ResolvedMovie): ResolvedMovie = {
    val id        = projected._id
    val shareCard = shareCards.current(projected)
    if (shareCard.nonEmpty && pendingCards.remove(id)) shareCards.onPendingCardLanded(id)
    projected.copy(shareCard = shareCard, shareCardPending = pendingCards.contains(id))
  }

  /** Caller holds `lock`. Forget the first-publish holds on `rowId`'s cards that its projection no
   *  longer produces — it lost its readiness, or its display title moved to another card. Only
   *  `gate` releases a hold on a card still produced; a hold nothing produces was otherwise never
   *  released, and once expired it cost a read and a full re-projection of its row on every change
   *  event (see `releaseExpired`). */
  private def dropHoldsNotProducedBy(rowId: String, produced: Set[String]): Unit =
    held.filterInPlace((card, hold) => hold.row != rowId || produced(card))

  /** Re-project the row behind card `filmId` so its document picks up the share-card store's
   *  current state — what the renderer calls once a card is written, which is also what
   *  publishes a card the first-publish gate was holding. */
  def refreshShareCard(filmId: String): Unit = lock.synchronized {
    val row = held.get(filmId).map(_.row).getOrElse(filmId.takeWhile(_ != '~'))
    // A card held for its share card and published by the render landing is in flight like a
    // stream event: a sweep between the two found it absent (see `verdictOnHeals`).
    appliedSinceSweep.foreach(_ += row)
    movieRepository.findById(services.movies.FilmId(row)).foreach(projectRow(_, ProjectTrigger.ShareCard))
  }

  /** End the hold on card `filmId` now — its card can't be made (every poster failed for good) —
   *  and publish it with the fallback. */
  def releaseShareCardHold(filmId: String): Unit = lock.synchronized {
    held.get(filmId).foreach { card =>
      held.update(filmId, card.copy(until = clock.millis()))
      movieRepository.findById(services.movies.FilmId(card.row)).foreach(projectRow(_, ProjectTrigger.HoldRelease))
    }
  }

  /** Publish every held card whose hold has run out — asked by the task the ledger schedules for
   *  the end of each hold, so a card whose render never finishes (or whose row never changes
   *  again) still goes public after `firstCardHold`. */
  def releaseExpiredHolds(): Unit = {
    val unread = releaseExpired()
    // Throw so the task that asked retries: the holds are still there to release.
    if (unread.nonEmpty)
      throw new IllegalStateException(s"could not read ${unread.size} held row(s) to publish (${unread.take(5).mkString(", ")}); holds kept")
  }

  /** Publish every expired hold whose row reads, drop the ones whose row is GONE, and keep —
   *  returning — the ones whose row could not be READ. That last case used to count as gone:
   *  the hold was dropped, and the card was never published. */
  private def releaseExpired(): Set[String] = lock.synchronized {
    val now  = clock.millis()
    val rows = held.valuesIterator.filter(_.until <= now).map(_.row).toSet
    rows.filter { row =>
      movieRepository.findByIdChecked(services.movies.FilmId(row)) match {
        case (Some(whole), _) => projectRow(whole, ProjectTrigger.HoldRelease); false
        case (None, true)     => dropHoldsNotProducedBy(row, Set.empty); false             // gone: nothing to publish
        case (None, false)    => true                                                       // unreadable: keep the hold
      }
    }
  }

  /** Test seam: the cards the first-publish gate is holding. */
  private[readmodel] def heldCards: Set[String] = lock.synchronized(held.keySet.toSet)

  /** Caller holds `lock`. Remove one card and its screenings on the change-stream path. */
  private def retireCard(cardId: String, reason: String): Unit = {
    removeCard(cardId, audit = s"stream-$reason")
    metrics.recordCardRetired(reason)
  }

  // Caller holds `lock`. Project the row, REUSING the cached `ResolvedMovie` metadata when
  // the row's metadata inputs are unchanged (a showtime-only change at an already-present
  // cinema — the overwhelming common case under reproject/enrich showtime churn). Correctness:
  // an unchanged `metadataHash` guarantees an unchanged metadata output AND an unchanged
  // display-title variant partition (the hash covers the whole record bar showtimes, and no
  // metadata accessor reads showtimes), so `venuesAll` — the SAME partition's variants in
  // the SAME order — lines up 1:1 with the cached movie(s). The size guard is a
  // belt-and-suspenders fallback to recomputing: it can never pair a movie with the wrong
  // screenings. Reusing skips the resolve/synopsisByCity/ratings work; the venues' rows are
  // built (or not) afterwards, by `planScreenings`.
  private def projectReusingMetadata(partition: ReadModelProjection.Partition): Seq[(ResolvedMovie, Seq[ReadModelProjection.VenueScreening])] = {
    val stored = partition.stored
    // Keyed by the SOURCE ROW (`persistedId`, unique per `movies` document), NOT by the
    // projected `ReadModelProjection.filmId`: that id keys on `resolvedYear` and so
    // deliberately COLLAPSES several source rows onto one card (`kumotry|2025` and
    // `kumotry|2026` both project to `kumotry|2026` — see its doc). The cached VALUE is one
    // row's hash + `ResolvedMovie`s, so keying on the shared card id let two rows overwrite
    // each other's entry on every alternating change: neither ever read its own hash back,
    // every projection recomputed, and the miss rate never decayed. Keying per row makes each
    // row's entry its own.
    val rowKey = stored.id.value
    // The gate's version is part of the key: a new gate must not reuse cards gated by the old one.
    val hash   = (ReadModelProjection.metadataHash(stored), ratingGate.version).##
    val venues = partition.venuesAll
    val movies = lastMetadata.get(rowKey) match {
      case Some((cachedHash, cached)) if cachedHash == hash && venues.sizeIs == cached.size =>
        metrics.recordMetadataProjection(reused = true)
        cached
      case _ =>
        val recomputed = partition.moviesAll.map(ratingGate(stored, _))
        lastMetadata.update(rowKey, hash -> recomputed)
        metrics.recordMetadataProjection(reused = false)
        recomputed
    }
    movies.zip(venues)
  }

  /** Caller holds `lock`. Build the screenings rows of one card — but only for the venues
   *  whose inputs moved since their row was written. A venue whose [[WrittenScreening]]
   *  carries the same input hash already has exactly the row it would build, so it is
   *  carried as `None`: still a live id (not deleted), not rebuilt, not rewritten. This is
   *  what turns a showtime change at one venue of a wide release from thousands of rows
   *  built into one. A memo that cannot vouch — seeded from the read model at boot, or
   *  dropped by a heal — has no input hash, so every such venue is rebuilt. */
  private def planScreenings(filmId: String, venues: Seq[ReadModelProjection.VenueScreening]): Seq[PlannedScreening] = {
    val previous = lastScreenings.getOrElse(filmId, Map.empty)
    val planned  = venues.map { venue =>
      val input = venue.inputHash
      val built = if (previous.get(venue._id).exists(_.input.contains(input))) None else Some(venue.screening)
      PlannedScreening(venue._id, input, built)
    }
    val rebuilt = planned.count(_.built.isDefined)
    metrics.recordVenueProjection(rebuilt = rebuilt, reused = planned.size - rebuilt)
    planned
  }

  /** Returns the number of screening documents written (upserts + deletes). */
  private def diffScreenings(filmId: String, next: Seq[PlannedScreening]): Int = {
    val previous = lastScreenings.getOrElse(filmId, Map.empty)
    var upserted = 0
    val nextById = next.map { planned =>
      val output = planned.built match {
        case Some(s) =>
          val hash = s.##
          if (!previous.get(planned._id).exists(_.output == hash)) { writer.upsertScreening(s); upserted += 1 }
          hash
        // Not rebuilt: `planScreenings` only skips a venue whose entry vouches for its row.
        case None => previous(planned._id).output
      }
      planned._id -> WrittenScreening(output, Some(planned.input))
    }.toMap
    val deletes = previous.keysIterator.filterNot(nextById.contains).toSeq
    deletes.foreach(writer.deleteScreening)
    if (upserted > 0)        metrics.recordWrite(Target.Screening, Op.Upsert, upserted)
    if (deletes.nonEmpty) {
      metrics.recordWrite(Target.Screening, Op.Delete, deletes.size)
      // A card kept but some of its screening slots trimmed — routine reprojection
      // churn (DEBUG), but it's how a film can drop out of "served tomorrow" without
      // its whole card being deleted, so the audit covers it too.
      services.movies.RemovalAudit.screeningsCleared("read-model.diff", filmId, deletes.size,
        whole = nextById.isEmpty, reason = "reproject-trim")
    }
    if (nextById.isEmpty) lastScreenings.remove(filmId) else lastScreenings.update(filmId, nextById)
    upserted + deletes.size
  }

  // Only `reconcile` calls this — a film whose source row vanished or was re-keyed
  // (its filmId changed) is dropped wholesale. `recordFilmPruned` is the link-break
  // signal; the document deletes are also counted as reprojection writes.
  private def deleteFilm(filmId: String, reason: String): Unit = {
    removeCard(filmId, audit = "reconcile-prune")
    metrics.recordFilmPruned(reason, 1)
  }

  /** Caller holds `lock`. Remove a card and the screenings this process remembers for
   *  it; `audit` names the path on the removal-audit line. */
  private def removeCard(filmId: String, audit: String): Unit = {
    val screeningIds = lastScreenings.getOrElse(filmId, Map.empty).keys.toSeq
    writer.deleteMovie(filmId)
    // The card is gone from here on, so its share-card files go and the memo forgets it even if a
    // screenings delete then throws — remembered, the row coming back unchanged would skip writing
    // the card, and nothing would retire its files again. Screenings left behind belong to no live
    // card; the prune removes them while it stays retired.
    shareCards.onRetired(filmId)
    try screeningIds.foreach(writer.deleteScreening)
    catch { case exception: Throwable => forgetCard(filmId); throw exception }
    // The point a film actually leaves the served site (web_movies + its
    // web_screenings). During a "cards vanish" episode this names every dropped
    // filmId, one INFO line each — the read-model half of the removal audit.
    services.movies.RemovalAudit.cardRemoved(filmId, screeningIds.size, reason = audit)
    metrics.recordWrite(Target.Movie, Op.Delete, 1)
    if (screeningIds.nonEmpty) metrics.recordWrite(Target.Screening, Op.Delete, screeningIds.size)
    forgetCard(filmId)
    // NOT evicted here: `lastMetadata` is keyed by SOURCE ROW id, and `filmId` is a projected
    // CARD id — several rows can collapse onto one card, so there is no card→row mapping to
    // remove by. The prune sweep retains exactly the live row keys instead (see `sweep`),
    // which is a tighter eviction than this best-effort remove ever was.
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
   *  backstop for DELETES / re-keys the change stream's `onMovieDelete` did not retire
   *  (missed while down, or cards a previous process wrote that `lastCardsByRow` never
   *  saw); the set-difference prune is the correct, cheap way to reconcile them.
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
  private def sweep(reproject: Boolean): Seq[String] = lock.synchronized {
    // Log-only label. The reconcile-sweep metric now tracks ONLY the prune (the live,
    // scheduled backstop); the reproject path survives as a test/backfill seed and is
    // no longer metered — the reproject retirement gate it fed has been removed.
    val kind = if (reproject) "reproject" else ReconcileKind.Prune
    val liveIds = scala.collection.mutable.Set.empty[String]
    // The source-row keys behind those cards — what `lastMetadata` is keyed by.
    val liveRowKeys = scala.collection.mutable.Set.empty[String]
    // The ids of the ready rows this scan saw — what the content check walks its slice of.
    val liveRowIds  = scala.collection.mutable.ArrayBuffer.empty[services.movies.FilmId]
    var reprojected = 0
    // The cards as they are BEFORE this sweep. A ready row ANY of whose ids has no card
    // is healed here, in the same pass, before anything is pruned: on 2026-09-07 the
    // card id scheme changed under a live read model, the prune removed 509 Polish
    // cards as orphans, and the scrapes took an hour to write the new ones — the
    // ReadModelFilmPruneBurst alert. A prune must never leave a live film without a
    // card, whatever id its old card carried. "Any", not "none": most of those 509 were
    // VARIANT cards (a "35 lat po premierze …" banner, a "przedpremiera" listing) whose
    // plain sibling survived, and a heal that asked only for rows with no card at all
    // left Warszawa 45 films short four hours later.
    // …and only when that read was COMPLETE: an incomplete keyset scan answers "no cards",
    // and healing on that answer would re-project every row (the boot burst this design
    // avoids). A prune that cannot see the cards heals nothing this tick.
    val (cardsBeforeSeq, cardsRead) = if (reproject) (Seq.empty[String], true) else reader.findAllMovieIdsChecked()
    val cardsBefore = cardsBeforeSeq.toSet
    if (!reproject && !cardsRead) logger.warn(s"read-model $kind: the card ids could not be read — no row is healed this tick.")
    // The screenings rows as they are before this sweep, read once for the venue heal and
    // the prune alike. A venue the source lists (a cinema slot) with no row here is healed
    // like a missing card: a slot written after the row's last projection stayed invisible
    // while nothing else touched the row. A read that fails — or stops short, which the keyset
    // scan reports as EMPTY — heals no venue this tick: "no rows" is not "could not read them".
    val screeningRefsBefore: Option[Seq[ScreeningRef]] =
      if (reproject) None else Try(reader.findAllScreeningRefsChecked()).toOption.collect { case (refs, true) => refs }
    val screeningsBefore = screeningRefsBefore.map(_.map(_._id).toSet)
    val healed = scala.collection.mutable.ArrayBuffer.empty[String]
    // The REPROJECT needs showtimes — it writes them. The PRUNE never looks at one: it
    // computes ids, and `filmIds` derives those from the cinema SLOTS, which the slots-only
    // scan still stitches. So the frequent, scheduled sweep no longer pulls the whole
    // `screenings` collection through WiredTiger twice an hour to throw it away — 177,676
    // rows and 129 MB across the five countries (2026-09-05) against a 1.07 GB cache.
    val scan: (StoredMovieRecord => Unit) => Boolean =
      if (reproject) movieRepository.foreachRecord else movieRepository.foreachRecordWithSlots
    val scanComplete = scan { row =>
      if (row.record.readyToProject) {
        // Card ids, venue ids and the projection all come off ONE partition of the row.
        val partition = ReadModelProjection.partition(row, normalizer)
        val ids = partition.filmIds
        liveIds ++= ids
        liveRowKeys += row.id.value
        liveRowIds  += row.id
        if (!lastCardsByRow.contains(row.id.value)) lastCardsByRow.update(row.id.value, ids.toSet)
        if (reproject)
          continuing(s"read-model $kind: a row failed to project")(reprojected += project(partition, ProjectTrigger.Reproject))
        else if (cardsRead) {
          val metadataHash = ReadModelProjection.metadataHash(row)
          val absentCards  = ids.filterNot(cardsBefore)
          val absentVenues =
            screeningsBefore.fold(Seq.empty[String])(has => partition.screeningIds.filterNot(has))
              .filterNot(phantomsOf(row.id.value, metadataHash))   // looked already; nothing to write
          if (absentCards.nonEmpty || absentVenues.nonEmpty)
            continuing(s"read-model $kind: a row missing a projection failed to project") {
              heal(row.id, metadataHash, absentCards, absentVenues).foreach { repaired =>
                if (repaired) healed += row.id.value
              }
            }
        }
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
      // Each delete guarded: a read-model write THROWS on failure, and one refused delete must
      // not skip every other orphan, the content slice, the catch-up and the sweep's metrics.
      reader.findAllMovieIds().iterator.filterNot(liveIds).foreach { id =>
        val rowOfCard = id.takeWhile(_ != '~')
        continuing(s"read-model $kind: pruning card $id failed") {
          deleteFilm(id, if (liveRowKeys(rowOfCard)) PruneReason.VariantGone else PruneReason.RowGone)
          prunedFilms += 1
        }
      }
      // Drop metadata cached for source rows that no longer exist. Only reachable on a
      // COMPLETE scan — on a truncated one `liveRowKeys` is partial and this would evict
      // live rows' metadata, costing a needless recompute each.
      lastMetadata.filterInPlace((rowKey, _) => liveRowKeys(rowKey))
      lastCardsByRow.filterInPlace((rowKey, _) => liveRowKeys(rowKey))
      healedClean.filterInPlace((rowKey, _) => liveRowKeys(rowKey))
      screeningRefsBefore.getOrElse(reader.findAllScreeningRefs()).iterator.filterNot(ref => liveIds(ref.filmId)).foreach { ref =>
        continuing(s"read-model $kind: pruning screening ${ref._id} failed") {
          writer.deleteScreening(ref._id)
          metrics.recordWrite(Target.Screening, Op.Delete, 1)
          lastScreenings.updateWith(ref.filmId)(_.map(_ - ref._id).filter(_.nonEmpty))
          prunedScreenings += 1
        }
      }
    }
    // THE SELF-HEAL FOR A SILENT CHANGE STREAM (prune only). A terminal cursor error reopens
    // itself; a cursor that is open and delivering nothing — a server-side stall, a stale
    // resume position after a migration — reopens nothing, and this sweep healed a MISSING card
    // or venue but never re-projected a CHANGED row, so the site served stale ratings and
    // showtimes until someone restarted the worker. Every row written after the `movies`
    // cursor's last delivered event is what that cursor has failed to deliver; re-project
    // exactly those. Bounded to the rows that moved: `updatedAt` is indexed, and in steady
    // state — a live cursor delivers a write within seconds — the read returns nothing.
    // Whole rows (showtimes included) from a full stitch, so the projection is the one the
    // stream would have made. Independent of `scanComplete`: it is its own bounded read, and a
    // stale card is a stale card whether or not the prune could run.
    // A catch-up that re-projected every row it read raises the floor to the instant its read
    // started, so while the cursor STAYS dead each sweep reads only what was written since the
    // last one — not the same rows again, which on a cursor subscribed after the corpus was
    // written was the whole corpus every sweep. A row whose projection threw holds the floor
    // where it was, so the next sweep reads it again. The alert on the cursor's age (the
    // DELIVERY floor, which a catch-up never moves) is what ends the state. Only while a movies cursor is
    // SUBSCRIBED: a repository that never opened one (a test wiring, a Mongo-less boot) has
    // promised no deliveries, and the prune must stay the id-only sweep it is there.
    // THE ROLLING CONTENT CHECK: one slice of the corpus per sweep, read whole and
    // re-projected, so a row whose stored projection has drifted is corrected within a day
    // even though nothing about it changes again. Deterministic by row id, so the slices
    // partition the corpus rather than sampling it, and every row is reached.
    if (!reproject && scanComplete) {
      val slice   = math.floorMod(sweepCount, ContentSlices.toLong).toInt
      val drifted = reprojectSlice(liveRowIds, slice, s"$kind sweep", movieRepository.findByIdChecked, projectRow(_, ProjectTrigger.Content))._1
      if (drifted > 0)
        logger.warn(s"read-model $kind sweep: content check slice $slice of $ContentSlices rewrote $drifted document(s) — " +
          "a stored projection had drifted from what the source projects to, which the id-only sweeps cannot see.")
      metrics.recordDriftWrites(drifted)
      if (derivationPass == DerivationPass.Unchecked) armDerivationPass(liveRowIds.toVector)
    }
    sweepCount += 1

    var caughtUp = 0
    val liveness = movieRepository.changeStreamLiveness
    if (!reproject && liveness.isWatching(ChangeStreamLiveness.Movies)) {
      val readFrom = liveness.now()
      val since    = liveness.catchUpFloor(ChangeStreamLiveness.Movies)
      var failed   = false
      val complete = movieRepository.foreachRecordUpdatedSince(since) { row =>
        if (row.record.readyToProject)
          continuing(s"read-model $kind: a row written since the change stream last delivered failed to project") {
            projectRow(row, ProjectTrigger.CatchUp); caughtUp += 1
          }.getOrElse { failed = true }
      }
      if (complete && !failed) liveness.caughtUp(ChangeStreamLiveness.Movies, readFrom)
      metrics.recordCatchUp(caughtUp)
      if (caughtUp > 0)
        logger.warn(s"read-model $kind sweep: re-projected $caughtUp row(s) written since the movies change stream last " +
          s"delivered ($since) — the cursor is open but not delivering them.")
    }
    // Measurement (prune only): a `prune` sweep with didWork=true is the deletes/re-keys
    // the change stream can't deliver. Surfaced as kinowo_worker_readmodel_reconcile_sweeps.
    val didWork = reprojected > 0 || prunedFilms > 0 || prunedScreenings > 0
    if (!reproject) metrics.recordReconcileSweep(ReconcileKind.Prune, didWork)
    logger.info(s"read-model $kind sweep: reprojected $reprojected doc(s), pruned $prunedFilms film(s) + " +
      s"$prunedScreenings orphan screening(s)${if (scanComplete) "" else " [scan INCOMPLETE — prune skipped]"}.")
    healed.toSeq
  }

  /** Which of the rows a prune sweep healed were MISSES — the rest the change stream had in flight.
   *
   *  The sweep holds the projection lock from its first read of the read model to its last
   *  write, and every stream apply waits for that lock. So a scrape landing a new venue or a new
   *  row seconds before the sweep, or during it, leaves exactly the id the sweep finds absent in
   *  an event queued behind it: the heal writes it, and the stream would have written it a moment
   *  later. Counted, those paged `ReadModelHealsRecurring` for a stream that was fine — every
   *  sweep heal from 2026-09-24 21:08Z to 09-25 02:37Z (US and UK) came with a scrape finishing
   *  within five seconds of the sweep starting, and with no retirement, failed re-read or restart
   *  behind it. So the verdict waits, outside the lock, for the stream to apply what it had in
   *  flight: a healed row the stream then applies was not missed; one it leaves alone was. Only
   *  misses are metered and named in the WARN line. */
  private def verdictOnHeals(healed: Seq[String]): Unit = {
    if (healed.nonEmpty) awaitStreamApplied(movieRepository.changeStreamLiveness)
    val applied = lock.synchronized { val rows = appliedSinceSweep.fold(Set.empty[String])(_.toSet); appliedSinceSweep = None; rows }
    val (inFlight, missed) = healed.partition(applied)
    if (inFlight.nonEmpty)
      logger.info(s"read-model prune sweep: wrote ${inFlight.size} row(s) the change stream had in flight and then " +
        s"applied itself — not heals: ${ReadModelProjector.idsForLog(inFlight)}.")
    if (missed.nonEmpty) {
      metrics.recordHeal(HealTrigger.Sweep, missed.size)
      logger.warn(s"read-model prune sweep: projected ${missed.size} ready row(s) missing a card or a venue " +
        s"before the prune: ${ReadModelProjector.idsForLog(missed)}.")
    }
  }

  /** Full re-projection + prune. NOT scheduled — the resume-token change stream made the
   *  periodic reproject redundant. Kept as an explicit one-shot seed/backfill primitive:
   *  fixture/e2e read-model seeding calls it to project a settled corpus synchronously
   *  (it stitches split films via `foreachRecord`, which a per-row `onMovieUpsert` seed
   *  would not). Mirrors `scripts.BackfillReadModel`. */
  def reconcile(): Unit = { sweep(reproject = true); () }

  /** Cheap id-only orphan prune — the frequent backstop for deleted / merged-away rows, and
   *  for the rows a silent change stream failed to deliver (see `sweep`). */
  def pruneOrphans(): Unit = {
    lock.synchronized { appliedSinceSweep = Some(scala.collection.mutable.Set.empty) }
    val healed =
      try sweep(reproject = false)
      catch { case exception: Throwable => lock.synchronized { appliedSinceSweep = None }; throw exception }
    verdictOnHeals(healed)
  }

  /** Caller holds `lock`. Re-project every row of `rowIds` in content slice `slice`, each `read` by
   *  id and handed to `project`: the documents written, and whether every row was read and
   *  projected (a row gone since `rowIds` was taken counts as done — its cards are the prune's). */
  private def reprojectSlice(rowIds: Iterable[services.movies.FilmId], slice: Int, what: String,
                             read: services.movies.FilmId => (Option[StoredMovieRecord], Boolean),
                             project: StoredMovieRecord => Int): (Int, Boolean) = {
    var written  = 0
    var complete = true
    rowIds.iterator.filter(ReadModelProjector.contentSliceOf(_) == slice).foreach { id =>
      val projected = continuing(s"read-model $what: a row in content slice $slice failed to project") {
        read(id) match {
          case (Some(row), _) => written += project(row)
          case (None, true)   => ()
          case (None, false)  => complete = false
        }
      }
      if (projected.isEmpty) complete = false
    }
    (written, complete)
  }

  /** Caller holds `lock`. Re-project the CARDS of a row read without its showtimes, and rewrite
   *  each one the read model already serves that came out different. A card never reads a
   *  showtime (`ReadModelProjection.metadataHash`), so these are the cards [[project]] would write
   *  from the whole row. Everything that does depend on the showtimes is left to the paths that
   *  read them: a card's screenings, a card first published (the share-card gate asks whether it
   *  screens), a variant card that appears or goes (a derivation that moves those is not
   *  cards-only), and a row that lost its readiness. */
  private def projectCards(partition: ReadModelProjection.Partition): Int = {
    if (!partition.stored.record.readyToProject) return 0
    val wallStart = System.nanoTime()
    val cpuStart  = cpuClock.nanos()
    val cards     = projectReusingMetadata(partition).map(_._1)
    metrics.recordProject(ProjectTrigger.Derivation,
      wallSeconds = (System.nanoTime() - wallStart) / 1e9, cpuSeconds = (cpuClock.nanos() - cpuStart) / 1e9)
    var written = 0
    cards.filter(card => lastMovie.contains(card._id) && !held.contains(card._id)).foreach { projected =>
      val id     = projected._id
      val movie  = withShareCard(projected)
      val hash   = CardHash.of(movie)
      val before = lastMovie.get(id)
      if (!before.contains(hash)) {
        writer.upsertMovie(movie)
        metrics.recordWrite(Target.Movie, Op.Upsert, 1)
        metrics.recordCardWrite(before.fold(Set.empty[String])(_.partsDifferingFrom(hash)))
        lastMovie.update(id, hash)
        shareCards.onProjected(movie, screened = lastScreenings.get(id).exists(_.nonEmpty))
        written += 1
      }
    }
    written
  }

  /** Caller holds `lock`. Read which derivation the stored read model was last re-projected whole
   *  under and, when this code derives another, start the pass it owes over `rowIds` — the cards
   *  alone when every derivation since moved only cards ([[ReadModelDerivation.owedSince]]), and
   *  from the slice a previous process's pass towards the same version had reached. A read that
   *  fails decides nothing: the next sweep reads again. */
  private def armDerivationPass(rowIds: Vector[services.movies.FilmId]): Unit =
    (for (recorded <- derivationMarker.recorded(); progress <- derivationMarker.progress()) yield (recorded, progress)) match {
      case scala.util.Failure(exception) =>
        logger.warn(s"read-model derivation pass: the recorded derivation could not be read (${exception.getMessage}) — " +
          "asking again next sweep.")
      case scala.util.Success((recorded, progress)) =>
        ReadModelDerivation.owedSince(recorded, derivationHistory) match {
          case None => derivationPass = DerivationPass.Current
          case Some(scope) =>
            val current = derivationHistory.last.version
            val from    = progress.collect { case DerivationProgress(`current`, next) => next }.getOrElse(0)
            derivationPass = DerivationPass.Running(rowIds, scope, nextSlice = from, written = 0, complete = true)
            logger.warn(s"read-model derivation pass: the stored read model was derived under " +
              s"${recorded.getOrElse("no recorded version")}, this worker derives $current — re-projecting " +
              s"${if (scope == DerivationScope.Cards) "the cards of" else "all"} ${rowIds.size} row(s), one content slice of " +
              s"$ContentSlices every ${DerivationPassTickSeconds}s" +
              (if (from > 0) s", resuming at slice $from where an earlier pass stopped." else "."))
        }
    }

  /** THE DERIVATION PASS: one content slice of the whole-corpus re-projection a derivation change
   *  owes, per tick.
   *
   *  A deploy that changes what the projection DERIVES from an unchanged row moves no row, so the
   *  change stream re-projects nothing and each stored card keeps the old derivation until the
   *  rolling content check reaches its slice — up to a day. On 2026-09-24 the poster selection
   *  changed that way and `ReadModelContentMismatch` held ~2.5 h, until a human forced a
   *  whole-corpus content check by hand. So the first complete prune sweep compares this code's
   *  derivation ([[ReadModelDerivation.current]]) with the one the store recorded and, when they
   *  differ, walks the same 48 content slices over every row it saw — one slice per tick, each
   *  under the lock only for its own rows, so the change stream keeps flowing and Mongo takes a
   *  slice's reads at a time: the whole corpus in 8 minutes, inside the content audit's
   *  15-minute re-check. Diff-based like every projection, so only cards that moved are written.
   *  When only card fields moved it reads each row slots-only and re-projects its cards alone.
   *  Each complete slice records the pass's progress, so a restart resumes rather than repeats.
   *  Only a pass that read and projected every row records the new version; one that did not
   *  leaves the old one, and the next sweep starts it again from the first slice that missed a
   *  row. Nothing happens when the store's version is this process's. */
  def advanceDerivationPass(): Unit = lock.synchronized {
    derivationPass match {
      case running: DerivationPass.Running =>
        // A cards-only derivation reads each row slots-only — no `screenings` read — and
        // re-projects its cards alone (`projectCards`).
        val (written, complete) =
          if (running.scope == DerivationScope.Cards)
            reprojectSlice(running.rows, running.nextSlice, "derivation pass (cards)",
                           movieRepository.findByIdWithSlotsChecked, row => projectCards(ReadModelProjection.partition(row, normalizer)))
          else
            reprojectSlice(running.rows, running.nextSlice, "derivation pass",
                           movieRepository.findByIdChecked, projectRow(_, ProjectTrigger.Derivation))
        val next = running.copy(nextSlice = running.nextSlice + 1, written = running.written + written,
                                complete = running.complete && complete)
        // Only while every slice so far was complete: a restart then resumes after the last one,
        // and a pass that missed a row starts over from the first slice it missed one in.
        if (next.complete && next.nextSlice < ContentSlices)
          continuing("read-model derivation pass: recording its progress failed; a restart repeats this slice") {
            derivationMarker.recordProgress(DerivationProgress(derivationHistory.last.version, next.nextSlice))
          }
        derivationPass =
          if (next.nextSlice < ContentSlices) next
          else if (!next.complete) {
            logger.warn(s"read-model derivation pass: re-projected ${next.rows.size} row(s), rewrote ${next.written} " +
              "document(s), but some rows could not be read or projected — the derivation stays unrecorded and the " +
              "next sweep runs the pass again.")
            DerivationPass.Unchecked
          } else
            continuing("read-model derivation pass: recording the derivation failed; the next sweep runs the pass again") {
              derivationMarker.record(derivationHistory.last.version)
              logger.info(s"read-model derivation pass: re-projected ${if (next.scope == DerivationScope.Cards) "the cards of " else ""}" +
                s"all ${next.rows.size} row(s) under ${derivationHistory.last.version}, rewriting ${next.written} document(s); recorded.")
              DerivationPass.Current
            }.getOrElse(DerivationPass.Unchecked)
      case _ => ()
    }
  }

  /** Seed the last-projection state from the derived collections, so a restart doesn't
   *  rewrite documents that are already correct — and so a document this process did not
   *  write (one left under a renamed city's former slug, say) is one its next projection of
   *  that film can retire. What a worker does as it boots; public so a harness can model the
   *  boot of the next worker over a read model it did not write. */
  def seedFromReadModel(): Unit =
    lock.synchronized {
      reader.findAllMovies().foreach { m =>
        lastMovie.update(m._id, CardHash.of(m))
        if (m.shareCardPending) pendingCards += m._id
      }
      reader.findAllScreenings().groupBy(_.filmId).foreach { case (fid, ss) =>
        // The content is known, the slots it was built from are not: no input hash, so the
        // first projection of each venue rebuilds it rather than trusting it.
        lastScreenings.update(fid, ss.map(s => s._id -> WrittenScreening(s.##, input = None)).toMap)
      }
    }

  def start(): Unit = if (enabled) {
    seedFromReadModel()
    healMissingCards()
    // The change-stream watch covers live changes from now on (and, via the persisted
    // resume token, replays every upsert missed while the worker was down); the seeded
    // state above means incremental writes are no-ops for already-correct documents. Only
    // the cheap orphan prune is scheduled — the full reproject was retired (see class doc).
    watchHandle = movieRepository.watchChanges(onMovieUpsert, onMovieDelete)
    // Cheap orphan prune: frequent, no per-row re-projection (can't spike CPU). Deferred
    // off the boot path so it doesn't compete with boot hydrate + the first scrape.
    scheduler.scheduleAtFixedRate(
      () => Try(pruneOrphans()).recover { case exception => logger.warn(s"read-model prune tick failed: ${exception.getMessage}") },
      PruneBootDelaySeconds, PruneSeconds, TimeUnit.SECONDS)
    // The derivation pass's slices, paced: a no-op unless the first sweep found one owed.
    scheduler.scheduleAtFixedRate(
      () => Try(advanceDerivationPass()).recover { case exception => logger.warn(s"read-model derivation pass tick failed: ${exception.getMessage}") },
      PruneBootDelaySeconds, DerivationPassTickSeconds, TimeUnit.SECONDS)
    logger.info(s"ReadModelProjector started; orphan-prune every ${PruneSeconds}s (first in ${PruneBootDelaySeconds}s); " +
      s"no periodic reproject (retired); change-stream watch " +
      s"${if (watchHandle.isDefined) "active" else "unavailable — orphan-prune only"}.")
  } else logger.info("ReadModelProjector disabled (read model or movies repository not enabled).")

  /** Project, at boot, every ready row one of whose cards is missing — before the
   *  first prune can delete the card it has under an old id.
   *
   *  A row's card id can change under a running read model: the id scheme moved
   *  (2026-09, from `sanitize(title)|resolvedYear` to the row's permanent `FilmId`), or
   *  a database was restored from before that. The scheduled prune then deletes the
   *  old-id cards as orphans, while the change stream re-creates only the rows a
   *  scrape happens to rewrite — so the served site runs short until every venue has
   *  been scraped again. The first version of this check gated a whole-corpus
   *  reproject on a MAJORITY of cards being stale, and the 2026-09-07 rollout came in
   *  under it (509 of 1,156 Polish cards): Poznań served ~10% fewer films for the
   *  better part of an hour. The exact question is cheaper and has no threshold:
   *  which ready rows lack a card under one of their ids? Those are projected whole;
   *  everything else is left to the diffing change-stream path. A row is asked for
   *  EVERY id it projects to — a split row whose plain card survived but whose variant
   *  card did not is still short, and the change stream only revisits it on its next
   *  scrape change. */
  private def healMissingCards(): Unit = Try {
    val (cardsSeq, cardsRead) = reader.findAllMovieIdsChecked()
    val cards = cardsSeq.toSet
    val venues = Try(reader.findAllScreeningRefsChecked()).toOption.collect { case (refs, true) => refs.map(_._id).toSet }
    // The check reads slots only (ids derive from the slot titles, never from a
    // showtime); the few rows it names are then read whole, showtimes included. An
    // incomplete card read heals nothing: "no cards" and "could not read" differ.
    val missing = scala.collection.mutable.ListBuffer.empty[(services.movies.FilmId, Int, Seq[String], Seq[String])]
    val complete = cardsRead && movieRepository.foreachRecordWithSlots { row =>
      if (row.record.readyToProject) {
        val partition    = ReadModelProjection.partition(row, normalizer)
        lastCardsByRow.update(row.id.value, partition.filmIds.toSet)
        val absentCards  = partition.filmIds.filterNot(cards)
        val absentVenues = venues.fold(Seq.empty[String])(has => partition.screeningIds.filterNot(has))
        if (absentCards.nonEmpty || absentVenues.nonEmpty)
          missing += ((row.id, ReadModelProjection.metadataHash(row), absentCards, absentVenues))
      }
    }
    if (!cardsRead) logger.warn("read model: the card ids could not be read at boot — nothing healed; the prune sweep retries.")
    // Each row guarded, as in the sweep: one failed write must not leave every later row unhealed.
    val projected = missing.flatMap { case (id, metadataHash, absentCards, absentVenues) =>
      continuing(s"read model: healing $id at boot failed")(lock.synchronized(heal(id, metadataHash, absentCards, absentVenues)))
        .flatten.map(id.value -> _) }
    val healed = projected.collect { case (id, true) => id }
    if (healed.nonEmpty) {
      metrics.recordHeal(HealTrigger.Boot, healed.size)
      logger.warn(s"read model: projected ${healed.size} ready row(s) missing a card or a venue at boot" +
        (if (complete) "" else " (source scan incomplete — the rest heal on their next change)") +
        s": ${ReadModelProjector.idsForLog(healed)}.")
    }
  }.recover { case exception => logger.warn(s"read-model missing-card check failed, skipped: ${exception.getMessage}") }

  /** Caller holds `lock`. Re-project a row whose cards under `absentCards` and whose
   *  screenings rows under `absentVenues` are known to be missing from the read model —
   *  read whole, since the slots-only scans that find them carry no showtimes. What this
   *  process remembers about them is dropped first: the memo says "already written", the
   *  read model says otherwise, and the read model is the truth — trusting the memo would
   *  skip the very write the heal exists for.
   *
   *  True only when the projection wrote one of the ids that were MISSING — the repair a
   *  heal is. Any other write is a change the change stream has not applied yet: the heal
   *  reads the row whole and writes whatever differs, and while the sweep holds `lock` every
   *  event is queued behind it. Counting those called the 2026-09-23 US sweep's pending
   *  showtime changes heals, and paged `ReadModelHealsRecurring` for a stream that was fine.
   *
   *  A heal that repaired nothing found the absence to be the slots-only view's phantom (a
   *  spent slot projects no venue row). That answer is remembered against the row's
   *  `metadataHash`, for the boot check and the sweep alike, so no later pass asks again
   *  until the row moves. The boot check once kept no note, and the first sweep five
   *  minutes later re-projected every spent-slot row a second time — ~260 of them per PL
   *  worker start.
   *
   *  `None` when nothing was projected -- the row is gone by the time it is read whole, or no
   *  longer ready; otherwise whether the absence was repaired. */
  private def heal(id: services.movies.FilmId, metadataHash: Int, absentCards: Seq[String], absentVenues: Seq[String]): Option[Boolean] = {
    absentCards.foreach(forgetCard)
    absentVenues.foreach(forgetScreening)
    movieRepository.findById(id).flatMap { whole =>
      projectRow(whole, ProjectTrigger.Heal)
      // Forgotten above, so remembered now only if this projection produced and wrote it.
      val written  = (venue: String) => lastScreenings.valuesIterator.exists(_.contains(venue))
      val repaired = absentCards.exists(lastMovie.contains) || absentVenues.exists(written)
      val phantoms = absentVenues.filterNot(written).toSet
      if (phantoms.nonEmpty) healedClean.update(id.value, (metadataHash, phantomsOf(id.value, metadataHash) ++ phantoms))
      Option.when(whole.record.readyToProject)(repaired)
    }
  }

  /** The venues a heal found to be phantoms of this row, while its metadata is what it was. */
  private def phantomsOf(rowId: String, metadataHash: Int): Set[String] =
    healedClean.get(rowId).collect { case (hash, phantoms) if hash == metadataHash => phantoms }.getOrElse(Set.empty)

  /** Run one per-row step of a pass over many rows; a throw is logged and costs that row alone. */
  private def continuing[A](failure: => String)(step: => A): Option[A] =
    try Some(step)
    catch { case scala.util.control.NonFatal(exception) =>
      logger.warn(s"$failure, continuing: ${exception.getMessage}")
      None
    }

  private def forgetCard(filmId: String): Unit = {
    lastMovie.remove(filmId)
    held.remove(filmId)
    pendingCards.remove(filmId)
    lastScreenings.remove(filmId)
  }

  /** Drop one screenings row from the memo, by its id — its film is whichever memo
   *  entry holds the id, so the card id need not be parsed out of it. */
  private def forgetScreening(screeningId: String): Unit =
    lastScreenings.mapValuesInPlace((_, byId) => byId - screeningId).filterInPlace((_, byId) => byId.nonEmpty)

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}

object ReadModelProjector {
  /** How many prune sweeps it takes to re-project the whole corpus once — the ROLLING
   *  CONTENT CHECK. Every backstop before it compared IDS: the prune removes a card whose
   *  row is gone, the heal writes a card or a venue that is missing. None of them can see a
   *  row that EXISTS and is WRONG, and on 2026-09-08 three UK films had held the wrong
   *  showtimes since 2026-08-29 — Troy and 2046 at the Prince Charles, Glastonbury at the
   *  Southsea — served to real users, invisible to every sweep, and unrepairable by the
   *  change stream because the source had long since stopped changing. A projection is
   *  diff-based, so re-projecting a slice costs a read per row and writes only what actually
   *  drifted; at 48 slices on a 30-minute sweep the whole corpus is verified once a day.
   *
   *  A constant, not a knob: its one override was 1, to push a derivation change out at once
   *  (2026-09-24), and the derivation pass now does that on its own. */
  private[readmodel] val ContentSlices = 48

  /** The content slice a row falls in: by its id, so the slices partition the corpus. */
  private[readmodel] def contentSliceOf(id: services.movies.FilmId): Int =
    math.floorMod(id.value.##.toLong, ContentSlices.toLong).toInt

  /** Seconds between two slices of a derivation pass: 48 slices, the whole corpus in 8 minutes. */
  private val DerivationPassTickSeconds = 10L

  /** Where the whole-corpus re-projection a derivation change owes stands. */
  private sealed trait DerivationPass
  private object DerivationPass {
    /** The store's recorded derivation has not been read (yet, or again after a failed pass). */
    case object Unchecked extends DerivationPass
    /** The store was derived under this process's version: nothing owed. */
    case object Current extends DerivationPass
    /** Re-projecting `rows` (their cards alone, for a `Cards` scope), slice `nextSlice` next;
     *  `complete` while every row so far was read and projected. */
    final case class Running(rows: Vector[services.movies.FilmId], scope: DerivationScope, nextSlice: Int, written: Int,
                             complete: Boolean) extends DerivationPass
  }

  /** Two minutes: a render is a poster fetch (up to ~35s against a slow cinema origin) plus a
   *  composite, so this covers a cold one with room to spare while keeping a new film's first
   *  appearance close to its scrape. */
  val DefaultFirstCardHold: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.Duration(120L, TimeUnit.SECONDS)

  /** The id-only orphan prune's compiled-in cadence: every 30 minutes, first 5 minutes after boot. */
  val DefaultPruneInterval: ReadModelPruneInterval =
    ReadModelPruneInterval(scala.concurrent.duration.Duration(1800L, TimeUnit.SECONDS))
  val DefaultPruneBootDelay: ReadModelPruneBootDelay =
    ReadModelPruneBootDelay(scala.concurrent.duration.Duration(300L, TimeUnit.SECONDS))

  /** How long a write that landed while a sweep held the lock may take to be DELIVERED by its
   *  cursor, before the wait below can see it queued. */
  private val InFlightDeliveryGrace = scala.concurrent.duration.Duration(2000L, TimeUnit.MILLISECONDS)
  /** The longest a sweep's heal verdict waits for the stream's apply thread to catch up. */
  private val InFlightApplyTimeout = scala.concurrent.duration.Duration(30L, TimeUnit.SECONDS)

  /** Let the change stream apply what it had in flight when a sweep let go of the lock: wait out
   *  the delivery of a write that landed during the sweep, then until every event handed to the
   *  apply thread by then has been applied, or the timeout — an apply thread that far behind is
   *  `ChangeStreamApplyLagging`'s business, and a heal it leaves unapplied counts. No cursor
   *  subscribed (a Mongo-less boot, a test wiring) means nothing is in flight. */
  def awaitStreamApplied(
    liveness: ChangeStreamLiveness,
    grace:    scala.concurrent.duration.FiniteDuration = InFlightDeliveryGrace,
    timeout:  scala.concurrent.duration.FiniteDuration = InFlightApplyTimeout
  ): Unit =
    if (ChangeStreamLiveness.Collections.exists(liveness.isWatching)) {
      Thread.sleep(grace.toMillis)
      val handedOff = liveness.lastTicket
      val deadline  = System.nanoTime() + timeout.toNanos
      while (!liveness.appliedThrough(handedOff) && System.nanoTime() < deadline) Thread.sleep(50)
    }

  /** How many row ids one heal line names before it summarises the rest. */
  private[readmodel] val LoggedIdsPerLine = 20

  /** The healed row ids for a log line — every heal is otherwise a count nobody can trace
   *  back to the retirement that caused it — capped so a mass heal stays one line. */
  private[readmodel] def idsForLog(ids: collection.Seq[String]): String =
    ids.take(LoggedIdsPerLine).mkString(", ") +
      (if (ids.sizeIs > LoggedIdsPerLine) s" (+${ids.size - LoggedIdsPerLine} more)" else "")
}

/** What the projector remembers about a written screenings row: the hash of the row
 *  itself (`output`, the minimal-write diff) and of the inputs it was built from
 *  (`input`, [[ReadModelProjection.VenueScreening.inputHash]]) — `None` when this process
 *  did not build it, so nothing vouches that the current inputs would build it again. */
private[readmodel] final case class WrittenScreening(output: Int, input: Option[Int])

/** One venue's screenings row as a projection plans it: rebuilt (`built`), or carried
 *  unbuilt because the row written from the same `input` is still current. */
private[readmodel] final case class PlannedScreening(_id: String, input: Int, built: Option[CityScreening])

/** A card held back by the first-publish gate: the source row that projects it, and when its
 *  hold ends (epoch millis). */
private[readmodel] final case class HeldCard(row: String, until: Long)

/** What the projector remembers about a written card: one hash per part, so the next
 *  write can name the parts that moved ([[ReadModelProjectionMetrics.CardPart]]). Equal
 *  when every part is equal, which is exactly "the card did not change". */
private[readmodel] final case class CardHash(parts: Map[String, Int]) {
  def partsDifferingFrom(other: CardHash): Set[String] =
    parts.collect { case (part, h) if !other.parts.get(part).contains(h) => part }.toSet
}

private[readmodel] object CardHash {
  import ReadModelProjectionMetrics.CardPart
  def of(m: ResolvedMovie): CardHash = CardHash(Map(
    CardPart.Title          -> (m.title, m.originalTitle).##,
    CardPart.Poster         -> (m.posterUrl, m.fallbackPosterUrls).##,
    CardPart.Facts          -> (m.runtimeMinutes, m.releaseYear, m.genres, m.countries, m.directors, m.cast).##,
    CardPart.Synopsis       -> m.synopsis.##,
    CardPart.SynopsisByCity -> m.synopsisByCity.##,
    CardPart.Ratings        -> (m.ratings, m.weightedRating).##,
    CardPart.Trailers       -> m.trailerUrls.##,
    CardPart.AgeRating      -> m.ageRating.##,
    CardPart.ShareCard      -> (m.shareCard, m.shareCardPending).##))
}
