package services.readmodel

import services.movies.LatestTitleYear

import models.ResolvedMovie
import play.api.Logging
import services.movies.{MovieChangeFeed, MovieRepositoryReader, StoredMovieRecord}

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
 *  2. ORPHAN PRUNE (backstop) — a cheap, frequent id-only sweep ([[ReadModelSweeper]]'s `pruneOrphans`)
 *     that removes derived documents whose source film has vanished or was re-keyed
 *     and whose delete event this process never applied (it was down past the resume
 *     window, or the card was written by an earlier process — see `onMovieDelete`). It re-projects nothing, so it can't spike CPU.
 *
 * A deploy that changes what the projection DERIVES from an unchanged row reaches the stored
 * cards through neither: the DERIVATION PASS ([[ReadModelDerivationPass]]) re-projects the whole
 * corpus once, paced, when the store records another derivation ([[ReadModelDerivation]]) — only
 * the cards, off the slots-only read, when all that moved since was what a card shows.
 *
 * The full re-projection (the sweeper's `reconcile`) is NOT scheduled — it was the periodic
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
 * The change-stream callback and the sweeper's passes are serialised by one lock (its [[ProjectionMemo]]'s),
 * so the in-memory last-projection state needs no further synchronisation. This class owns
 * the per-row projection and its memo; [[ScreeningRowWrites]] the screenings rows' diff; its [[ShareCardGate]] the
 * share-card hold; [[ReadModelBoot]] the boot reads and the change-stream subscription that drive it; [[ReadModelSweeper]]
 * the passes over the whole corpus, which drive it through its [[projection]]. The projector builds no sweeper and no
 * boot: the composition root builds both over it.
 */
class ReadModelProjector(
  movieRepository: MovieRepositoryReader & MovieChangeFeed,
  writer:    ReadModelWriter,
  reader:    ReadModelReader,
  metrics:   ReadModelProjectionMetrics = ReadModelProjectionMetrics.noop,
  cpuClock:  tools.ThreadCpuClock = tools.ThreadCpuClock.threadMxBean,
  // Where each card's `shareCard` comes from, and the first-publish gate that holds a brand-new card
  // back (bounded) waiting for its share card — see [[ShareCardGate]].
  shareCardGate: ShareCardGate = ShareCardGate.ungated,
  // No default: the rolling content check numbers its slice by this clock, so a spec left on the
  // wall clock re-projected its row in the half-hours that row's slice came up (Main red, 2026-09-25).
  clock:     java.time.Clock,
  // Confidence-gated ratings (identity phase 3): what each card's ratings are served as. `off`
  // — the card as projected — unless the worker's composition root switches it on.
  ratingGate: RatingGate = RatingGate.off
) extends Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)
  // The projection keys rows by the repository's own `_id` formula, so it must
  // fold titles with the same rules the repository writes under — take them from
  // it rather than accepting a second, separately-wired copy that could disagree.
  private val normalizer: services.titles.TitleNormalizer = movieRepository.normalizer

  import ReadModelProjectionMetrics.{Op, ProjectTrigger, RetireReason, Target}

  // What this process knows it has written: the diff state of every projection, and the lock serialising them.
  private[readmodel] val memo = new ProjectionMemo
  import memo.{healedClean, lastCardsByRow, lastGroups, lastMetadata, lastMovie, lastScreenings, lock}
  private val screeningRows = new ScreeningRowWrites(writer, metrics, lastScreenings)

  // The sweeper over [[projection]], when one is: what this projector tells it of each change it applies,
  // read and replaced under `lock` only.
  private var sweep: ReadModelSweeper.Listener = ReadModelSweeper.Listener.none

  /** What a [[ReadModelSweeper]] drives — this projector's stores, its lock, its per-row projection
   *  and its memo. The composition root builds the sweeper over it; a projector constructs none. */
  val projection: ReadModelSweeper.Projection = new ReadModelSweeper.Projection {
    def movieRepository: MovieRepositoryReader & MovieChangeFeed = ReadModelProjector.this.movieRepository
    def writer: ReadModelWriter = ReadModelProjector.this.writer
    def reader: ReadModelReader = ReadModelProjector.this.reader
    def metrics: ReadModelProjectionMetrics = ReadModelProjector.this.metrics
    def clock: java.time.Clock = ReadModelProjector.this.clock
    private[readmodel] def reportTo(listener: ReadModelSweeper.Listener): Unit = sweep = listener
    def exclusively[A](body: => A): A = lock.synchronized(body)
    def partition(row: StoredMovieRecord): ReadModelProjection.Partition = ReadModelProjection.partition(row, normalizer)
    def project(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Int = ReadModelProjector.this.project(partition, trigger)
    def projectRow(row: StoredMovieRecord, trigger: ProjectTrigger): Int = ReadModelProjector.this.projectRow(row, trigger)
    def projectCards(partition: ReadModelProjection.Partition): Int = ReadModelProjector.this.projectCards(partition)
    def heal(id: services.movies.FilmId, metadataHash: Int, absentCards: Seq[String], absentVenues: Seq[String]): Option[Boolean] =
      ReadModelProjector.this.heal(id, metadataHash, absentCards, absentVenues)
    def phantomsOf(rowId: String, metadataHash: Int): Set[String] = memo.phantomsOf(rowId, metadataHash)
    def noteRowCards(rowId: String, cards: Set[String]): Unit =
      if (!lastCardsByRow.contains(rowId)) lastCardsByRow.update(rowId, cards)
    def recordRowCards(rowId: String, cards: Set[String]): Unit = lastCardsByRow.update(rowId, cards)
    def retainRows(live: String => Boolean): Unit = memo.retainRows(live)
    def removeCard(filmId: String, audit: String): Unit = ReadModelProjector.this.removeCard(filmId, audit)
    def forgetScreening(ref: ScreeningRef): Unit = lastScreenings.forget(ref.filmId, ref._id)
  }

  def enabled: Boolean = writer.enabled && movieRepository.enabled

  /** Apply one source-row change from the change stream. */
  def onMovieUpsert(stored: StoredMovieRecord): Unit =
    lock.synchronized {
      sweep.noteApplied(stored.id.value)
      projectRow(stored, ProjectTrigger.Stream)
      // A second way out of a first-publish hold besides the task scheduled for its end: that
      // task may be claimed by a replica other than the one holding the card.
      // A row it could not read keeps its hold for the next sweep — see `releaseExpired`.
      if (shareCardGate.holdsAny) releaseExpired()
      ()
    }

  /** Apply a change confined to some venues' showtimes from those venues alone — the whole film
   *  unread — when that is exactly what projecting the whole film again would write: the venues' rows
   *  rebuilt, nothing else touched. Declined, having written nothing, whenever it cannot vouch for that,
   *  and the caller projects the whole film instead:
   *    - the row was not projected by this process, or a venue's slot reports a title outside the
   *      groups it was (a retitle would move venues between cards);
   *    - a venue holds two slots of the film (its row unions them in the record's own slot order);
   *    - a venue would appear or vanish (a card's first screening gates its publication, and a card's
   *      screenings decide whether it is served at all);
   *    - a card is held or unpublished.
   *  Card metadata never reads showtimes ([[ReadModelProjection.metadataHash]]), so a showtime-only
   *  change leaves every card as it is. */
  def onVenueSlots(venues: services.movies.VenueSlots): services.movies.VenueVerdict = lock.synchronized {
    val rowId = venues.filmId.value
    planVenues(rowId, venues) match {
      case Left(reason) => services.movies.VenueVerdict.Declined(reason)
      case Right(byCard) =>
        sweep.noteApplied(rowId)
        byCard.foreach(screeningRows.applyVenues)
        if (shareCardGate.holdsAny) releaseExpired()
        services.movies.VenueVerdict.Applied
    }
  }

  /** Caller holds `lock`. The venues [[onVenueSlots]] rebuilds, by card — or the first reason it
   *  cannot vouch that they are all a whole-film projection would change (see there). */
  private def planVenues(rowId: String, venues: services.movies.VenueSlots): VenuePlans.Plan = {
    import services.movies.ChangeStreamMetrics.VenueDecline as Why
    import VenuePlans.refuse
    scala.util.boundary {
      val groups  = lastGroups.getOrElse(rowId, refuse(Why.ProjectorRowUnprojected))
      val cards   = groups.cardByGroup.values.toSeq.distinct
      val planned = venues.atCinemas.toSeq.flatMap { case (cinema, slots) =>
        VenuePlans.ofCinema(groups, cards, cinema, slots, normalizer.sanitize, lastScreenings.contains) }
      val byCard  = planned.groupMap(_._1)(_._2)
      byCard.keys.foreach { card =>
        if (shareCardGate.holds(card)) refuse(Why.ProjectorCardHeld)
        if (!lastMovie.contains(card)) refuse(Why.ProjectorCardUnpublished)
      }
      Right(byCard)
    }
  }

  /** A row deleted or merged away: every card it produced goes with it, now — not at the
   *  next prune. The cards are what this process remembers producing for the row, plus
   *  anything in the read model under the row's id (a card a previous process wrote). */
  def onMovieDelete(id: services.movies.FilmId): Unit = lock.synchronized {
    sweep.noteApplied(id.value)
    val remembered = lastCardsByRow.getOrElse(id.value, Set.empty)
    val underId    = lastMovie.keysIterator.filter(card => card == id.value || card.startsWith(id.value + "~")).toSet
    (remembered ++ underId).foreach(retireCard(_, RetireReason.RowDeleted))
    lastCardsByRow.remove(id.value)
    lastGroups.remove(id.value)
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
    if (!partition.stored.record.readyToProject) { retireUnready(rowId); return 0 }
    // A row fans out into one card per display-title variant (Cyrillic / English
    // / banner-prefixed listings of one film); the common single-title row yields
    // exactly one. Each variant card is diffed and written independently.
    val variants = timedProjection(trigger)(planRow(partition, trigger))
    // The WRITE half, timed separately from the computation above (`recordProject`) — see
    // `recordWriteBurst`. A wide release writes one document per (card, city, cinema)
    // through `writer.upsertMovie`/`screeningRows.diff`, sequentially; this is the phase that
    // scales with city count, not with resolve/synopsisByCity/ratings cost.
    val writing = tools.Stopwatch.start()
    // A content check's writes are DRIFT CANDIDATES — the stored projection disagreed with the
    // source — held by what moved for the sweep's verdict, which counts and names only the rows
    // no event then repaired, the film ids being what a drift investigation has to start from.
    val drift   = Option.when(trigger == ProjectTrigger.Content)(new RowDriftLog)
    val now     = clock.millis()
    val publish = variants.flatMap { case (projected, screenings) =>
      shareCardGate.admit(rowId, projected, screened = screenings.nonEmpty, served = memo.served(projected._id), now).map(_ -> screenings)
    }
    val written = publish.map { case (movie, screenings) => writeVariant(movie, screenings, drift) }.sum
    retireVariantsGone(rowId, publish.map(_._1._id).toSet)
    metrics.recordWriteBurst(trigger, writing.seconds)
    drift.flatMap(_.of(rowId)).foreach(sweep.noteDrift)
    written
  }

  /** Caller holds `lock`. A row that lost its readiness takes its cards with it (the prune would, later). */
  private def retireUnready(rowId: String): Unit = {
    lastGroups.remove(rowId)
    healedClean.remove(rowId)
    lastCardsByRow.remove(rowId).foreach(_.foreach(retireCard(_, RetireReason.RowUnready)))
    shareCardGate.dropHoldsNotProducedBy(rowId, Set.empty)
  }

  /** Run a pure projection and measure it BOTH ways: wall-clock answers "how long did it take" (the
   *  latency histogram), thread CPU answers "how much CPU did it burn" (the credit-floor attribution).
   *  They diverge under concurrency and under steal, so the one that may be compared against process
   *  CPU is the CPU one — see `recordProject`. */
  private def timedProjection[A](trigger: ProjectTrigger)(projection: => A): A = {
    val wall     = tools.Stopwatch.start()
    val cpuStart = cpuClock.nanos()
    val result   = projection
    metrics.recordProject(trigger, wallSeconds = wall.seconds, cpuSeconds = (cpuClock.nanos() - cpuStart) / 1e9)
    result
  }

  /** Caller holds `lock`. Project a ready row's cards and plan their screenings rows, remembering its
   *  title groups and dropping the holds on cards it no longer produces. */
  private def planRow(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Seq[(ResolvedMovie, Seq[PlannedScreening])] = {
    val rowId     = partition.stored.id.value
    val projected = projectReusingMetadata(partition, trigger)
    val variants  = projected.map { (movie, venues) => (movie, screeningRows.plan(movie._id, venues)) }
    lastGroups.update(rowId, RowGroups(partition.anchorKey, partition.cardByGroup,
      projected.iterator.flatMap(_._2).filter(_.slotCount > 1).map(_._id).toSet))
    shareCardGate.dropHoldsNotProducedBy(rowId, variants.map(_._1._id).toSet)
    variants
  }

  /** Caller holds `lock`. Write one published card and its screenings rows where they changed: the
   *  documents written. */
  private def writeVariant(movie: ResolvedMovie, screenings: Seq[PlannedScreening], drift: Option[RowDriftLog]): Int = {
    val hash    = CardHash.of(movie)
    val changed = !lastMovie.get(movie._id).contains(hash)
    if (changed) {
      val parts = upsertCard(movie, hash)
      drift.foreach(_.card(movie._id, parts))
    }
    val screeningDiff = screeningRows.diff(movie._id, screenings)
    // A card kept but some of its screening slots trimmed — routine reprojection
    // churn (DEBUG), but it's how a film can drop out of "served tomorrow" without
    // its whole card being deleted, so the audit covers it too.
    if (screeningDiff.deleted.nonEmpty)
      services.movies.RemovalAudit.screeningsCleared("read-model.diff", movie._id, screeningDiff.deleted.size,
        whole = screenings.isEmpty, reason = "reproject-trim")
    drift.foreach(_.screenings(screeningDiff))
    // Remembered only once the screenings are written too: a throw in the screenings
    // write used to leave the card's hash "current", so the screenings were never
    // retried until the row changed again.
    if (changed) rememberCard(movie, hash, screened = screenings.nonEmpty)
    (if (changed) 1 else 0) + screeningDiff.documents
  }

  /** Caller holds `lock`. Write a card whose hash moved to `hash`: the parts that moved. */
  private def upsertCard(movie: ResolvedMovie, hash: CardHash): Set[String] = {
    writer.upsertMovie(movie)
    metrics.recordWrite(Target.Movie, Op.Upsert, 1)
    val parts = lastMovie.get(movie._id).fold(Set.empty[String])(_.partsDifferingFrom(hash))
    metrics.recordCardWrite(parts)
    parts
  }

  /** Caller holds `lock`. Remember a card as written at `hash`, and tell the share-card gate. */
  private def rememberCard(movie: ResolvedMovie, hash: CardHash, screened: Boolean): Unit = {
    lastMovie.update(movie._id, hash)
    shareCardGate.cardWritten(movie, screened = screened)
  }

  /** Caller holds `lock`. A variant card this row produced last time and no longer does — its
   *  decorated listing vanished — is retired here, by the path that knows, not by the prune. A card
   *  still held by the first-publish gate was never produced. A card held back while its unserved
   *  document stands (see [[ShareCardGate.admit]]) is not gone either. */
  private def retireVariantsGone(rowId: String, produced: Set[String]): Unit = {
    val before   = lastCardsByRow.getOrElse(rowId, Set.empty)
    val kept     = produced ++ before.filter(shareCardGate.holds)
    (before -- kept).foreach(retireCard(_, RetireReason.VariantGone))
    lastCardsByRow.update(rowId, kept)
    // A venue this projection wrote is no phantom any more: if its row goes missing now, that
    // is a real loss the heal must repair.
    val served = kept.flatMap(card => lastScreenings.ids(card).toSet)
    healedClean.updateWith(rowId)(_.map { case (hash, phantoms) => (hash, phantoms -- served) }.filter(_._2.nonEmpty))
  }

  /** Re-project the row behind card `filmId` so its document picks up the share-card store's
   *  current state — what the renderer calls once a card is written, which is also what
   *  publishes a card the first-publish gate was holding. */
  def refreshShareCard(filmId: String): Unit = lock.synchronized {
    val row = shareCardGate.heldRow(filmId).getOrElse(filmId.takeWhile(_ != '~'))
    // A card held for its share card and published by the render landing is in flight like a
    // stream event: a sweep between the two found it absent (see [[SweepVerdict]]).
    sweep.noteApplied(row)
    movieRepository.findById(services.movies.FilmId(row)).foreach(projectRow(_, ProjectTrigger.ShareCard))
  }

  /** End the hold on card `filmId` now — its card can't be made (every poster failed for good) —
   *  and publish it with the fallback. */
  def releaseShareCardHold(filmId: String): Unit = lock.synchronized {
    shareCardGate.expireNow(filmId, clock.millis()).foreach { row =>
      movieRepository.findById(services.movies.FilmId(row)).foreach(projectRow(_, ProjectTrigger.HoldRelease))
    }
  }

  /** Publish every held card whose hold has run out — asked by the task the ledger schedules for
   *  the end of each hold, so a card whose render never finishes (or whose row never changes
   *  again) still goes public after its first-publish hold. */
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
    val rows = shareCardGate.expiredRows(now)
    rows.filter { row =>
      movieRepository.findByIdChecked(services.movies.FilmId(row)) match {
        case tools.ReadOutcome.Answered(whole)     => projectRow(whole, ProjectTrigger.HoldRelease); false
        case tools.ReadOutcome.Absent(_)  => shareCardGate.dropHoldsNotProducedBy(row, Set.empty); false             // gone: nothing to publish
        case tools.ReadOutcome.Failed(_)  => true                                                       // unreadable: keep the hold
      }
    }
  }

  /** Test seam: the cards the first-publish gate is holding. */
  private[readmodel] def heldCards: Set[String] = lock.synchronized(shareCardGate.heldCards)

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
  // built (or not) afterwards, by `screeningRows.plan`.
  private def projectReusingMetadata(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Seq[(ResolvedMovie, Seq[ReadModelProjection.VenueScreening])] = {
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
        metrics.recordMetadataProjection(trigger, reused = true)
        cached
      case _ =>
        val recomputed = partition.moviesAll.map(ratingGate(stored, _))
        lastMetadata.update(rowKey, hash -> recomputed)
        metrics.recordMetadataProjection(trigger, reused = false)
        recomputed
    }
    movies.zip(venues)
  }

  /** Caller holds `lock`. Remove a card and the screenings this process remembers for
   *  it; `audit` names the path on the removal-audit line. */
  private def removeCard(filmId: String, audit: String): Unit = {
    val screeningIds = lastScreenings.ids(filmId)
    writer.deleteMovie(filmId)
    // The card is gone from here on, so its share-card files go and the memo forgets it even if a
    // screenings delete then throws — remembered, the row coming back unchanged would skip writing
    // the card, and nothing would retire its files again. Screenings left behind belong to no live
    // card; the prune removes them while it stays retired.
    shareCardGate.cardRetired(filmId)
    tools.AnyThrowable.rethrownAfter(screeningIds.foreach(writer.deleteScreening))(_ => forgetCard(filmId))
    // The point a film actually leaves the served site (web_movies + its
    // web_screenings). During a "cards vanish" episode this names every dropped
    // filmId, one INFO line each — the read-model half of the removal audit.
    services.movies.RemovalAudit.cardRemoved(filmId, screeningIds.size, reason = audit)
    metrics.recordWrite(Target.Movie, Op.Delete, 1)
    if (screeningIds.nonEmpty) metrics.recordWrite(Target.Screening, Op.Delete, screeningIds.size)
    forgetCard(filmId)
    // NOT evicted here: `lastMetadata` is keyed by SOURCE ROW id, and `filmId` is a projected
    // CARD id — several rows can collapse onto one card, so there is no card→row mapping to
    // remove by. The prune sweep retains exactly the live row keys instead (see `retainRows`),
    // which is a tighter eviction than this best-effort remove ever was.
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
    val cards = timedProjection(ProjectTrigger.Derivation)(projectReusingMetadata(partition, ProjectTrigger.Derivation).map(_._1))
    cards.filter(card => lastMovie.contains(card._id) && !shareCardGate.holds(card._id)).count { projected =>
      val movie   = shareCardGate.withShareCard(projected)
      val hash    = CardHash.of(movie)
      val changed = !lastMovie.get(movie._id).contains(hash)
      if (changed) {
        upsertCard(movie, hash)
        rememberCard(movie, hash, screened = lastScreenings.holdsAny(movie._id))
      }
      changed
    }
  }

  /** A card the read model serves, as the boot seed reads it ([[ReadModelBoot]]): remembered as written, and its
   *  share card's pending state told to the gate. Caller holds `lock`. */
  private[readmodel] def seedCard(movie: ResolvedMovie): Unit = {
    lastMovie.update(movie._id, CardHash.of(movie))
    if (movie.shareCardPending) shareCardGate.seedPending(movie._id)
  }

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
      val written  = (venue: String) => lastScreenings.holds(venue)
      val repaired = absentCards.exists(lastMovie.contains) || absentVenues.exists(written)
      val phantoms = absentVenues.filterNot(written).toSet
      if (phantoms.nonEmpty) healedClean.update(id.value, (metadataHash, memo.phantomsOf(id.value, metadataHash) ++ phantoms))
      Option.when(whole.record.readyToProject)(repaired)
    }
  }

  private def forgetCard(filmId: String): Unit = {
    lastMovie.remove(filmId)
    shareCardGate.forget(filmId)
    lastScreenings.forgetCard(filmId)
  }

  /** Drop one screenings row from the memo, by its id — its film is whichever memo
   *  entry holds the id, so the card id need not be parsed out of it. */
  private def forgetScreening(screeningId: String): Unit =
    lastScreenings.forget(screeningId)
}
