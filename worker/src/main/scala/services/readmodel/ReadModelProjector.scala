package services.readmodel

import models.StoredMovieRecord

import services.movies.LatestTitleYear

import models.ResolvedMovie
import play.api.Logging
import services.movies.{MovieChangeFeed, MovieRepositoryReader}

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
 * so the in-memory last-projection state needs no further synchronisation. A change passes three stages: this class
 * decides which of a row's cards stand and which go; its [[CardProjection]] projects the row's cards and plans their
 * screenings rows; its [[ShareCardGate]] holds a brand-new card back for its share card; its [[CardStore]] writes what
 * stands and removes what goes. At boot it seeds its memo from the read model ([[seedFromReadModel]]) and learns the
 * corpus's rows ([[learnAtBoot]], [[learnFromCorpus]]); [[ReadModelBoot]] decides when, and holds the change-stream
 * subscription that drives it until then;
 * [[ReadModelSweeper]] the passes over the whole corpus, which drive it through its [[projection]]. The projector builds
 * none of them: [[ReadModelProjector.apply]] assembles its stages, and the composition root builds the sweeper and the
 * boot over it.
 */
class ReadModelProjector private[readmodel] (
  movieRepository: MovieRepositoryReader & MovieChangeFeed,
  writer:          ReadModelWriter,
  reader:          ReadModelReader,
  metrics:         ReadModelProjectionMetrics,
  // Where each card's `shareCard` comes from, and the first-publish gate that holds a brand-new card
  // back (bounded) waiting for its share card — see [[ShareCardGate]].
  shareCardGate:   ShareCardGate,
  clock:           java.time.Clock,
  stopwatch:       tools.Stopwatch,
  // What this process knows it has written: the diff state of every projection, and the lock serialising them.
  private[readmodel] val memo: ProjectionMemo,
  // The stages a row's change passes through: its cards projected, then written.
  cardProjection:  CardProjection,
  cardStore:       CardStore
)(using LatestTitleYear) extends Logging {
  // The projection keys rows by the repository's own `_id` formula, so it must
  // fold titles with the same rules the repository writes under — take them from
  // it rather than accepting a second, separately-wired copy that could disagree.
  private val normalizer: services.titles.TitleNormalizer = movieRepository.normalizer

  import ReadModelProjectionMetrics.{ProjectTrigger, RetireReason}

  import memo.{healedClean, lastCardsByRow, lastGroups, lastMovie, lastScreenings, lock}

  // The sweeper over [[projection]], when one is: what this projector tells it of each change it applies,
  // read and replaced under `lock` only.
  private var sweep: SweptProjection.Listener = SweptProjection.Listener.none

  /** What a [[ReadModelSweeper]] drives — this projector's stores, its lock, its per-row projection
   *  and its memo. The composition root builds the sweeper over it; a projector constructs none. */
  val projection: SweptProjection = new SweptProjection {
    def movieRepository: MovieRepositoryReader & MovieChangeFeed = ReadModelProjector.this.movieRepository
    def scan(withShowtimes: Boolean)(row: StoredMovieRecord => Unit): tools.ScanOutcome =
      if (withShowtimes) movieRepository.foreachRecord(row) else movieRepository.foreachRecordWithSlots(row)
    def read(id: models.FilmId, withShowtimes: Boolean): tools.ReadOutcome[StoredMovieRecord] =
      if (withShowtimes) movieRepository.findByIdChecked(id) else movieRepository.findByIdWithSlotsChecked(id)
    def reader: ReadModelReader = ReadModelProjector.this.reader
    def metrics: ReadModelProjectionMetrics = ReadModelProjector.this.metrics
    def clock: java.time.Clock = ReadModelProjector.this.clock
    private[readmodel] def reportTo(listener: SweptProjection.Listener): Unit = sweep = listener
    def exclusively[A](body: => A): A = lock.synchronized(body)
    def partition(row: StoredMovieRecord): ReadModelProjection.Partition = ReadModelProjection.partition(row, normalizer)
    def project(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Int = ReadModelProjector.this.project(partition, trigger)
    def projectRow(row: StoredMovieRecord, trigger: ProjectTrigger): Int = ReadModelProjector.this.projectRow(row, trigger)
    def projectCards(partition: ReadModelProjection.Partition): Int = ReadModelProjector.this.projectCards(partition)
    def heal(id: models.FilmId, metadataHash: Int, absentCards: Seq[String], absentVenues: Seq[String]): Option[Boolean] =
      ReadModelProjector.this.heal(id, metadataHash, absentCards, absentVenues)
    def phantomsOf(rowId: String, metadataHash: Int): Set[String] = memo.phantomsOf(rowId, metadataHash)
    def noteRowCards(rowId: String, cards: Set[String]): Unit =
      if (!lastCardsByRow.contains(rowId)) lastCardsByRow.update(rowId, cards)
    def recordRowCards(rowId: String, cards: Set[String]): Unit = lastCardsByRow.update(rowId, cards)
    def retainRows(live: String => Boolean): Unit = memo.retainRows(live)
    def removeCard(filmId: String, audit: String): Unit = cardStore.remove(filmId, audit)
    def removeScreening(ref: ScreeningRef): Unit = cardStore.removeScreening(ref)
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
    VenuePlans.plan(lastGroups.get(rowId), venues, normalizer.sanitize, lastScreenings.contains,
                    held = shareCardGate.holds, published = lastMovie.contains) match {
      case Left(reason) => services.movies.VenueVerdict.Declined(reason)
      case Right(byCard) =>
        sweep.noteApplied(rowId)
        byCard.foreach(cardStore.writeVenues)
        if (shareCardGate.holdsAny) releaseExpired()
        services.movies.VenueVerdict.Applied
    }
  }

  /** A row deleted or merged away: every card it produced goes with it, now — not at the
   *  next prune. The cards are what this process remembers producing for the row, plus
   *  anything in the read model under the row's id (a card a previous process wrote). */
  def onMovieDelete(id: models.FilmId): Unit = lock.synchronized {
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
    if (!partition.stored.readyToProject) { retireUnready(rowId); return 0 }
    // A row fans out into one card per display-title variant (Cyrillic / English
    // / banner-prefixed listings of one film); the common single-title row yields
    // exactly one. Each variant card is diffed and written independently.
    val variants = planRow(partition, trigger)
    // The WRITE half, timed separately from the computation above (`recordProject`) — see
    // `recordWriteBurst`. A wide release writes one document per (card, city, cinema)
    // through the card store, sequentially; this is the phase that
    // scales with city count, not with resolve/synopsisByCity/ratings cost.
    val writing = stopwatch.start()
    // A content check's writes are DRIFT CANDIDATES — the stored projection disagreed with the
    // source — held by what moved for the sweep's verdict, which counts and names only the rows
    // no event then repaired, the film ids being what a drift investigation has to start from.
    val drift   = Option.when(trigger == ProjectTrigger.Content)(new RowDriftLog)
    val now     = clock.millis()
    val publish = variants.flatMap { case (projected, screenings) =>
      shareCardGate.admit(rowId, projected, screened = screenings.nonEmpty, served = memo.served(projected._id), now).map(_ -> screenings)
    }
    val written = publish.map { case (movie, screenings) => cardStore.write(movie, screenings, drift) }.sum
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

  /** Caller holds `lock`. Project a ready row's cards and plan their screenings rows, remembering its
   *  title groups and dropping the holds on cards it no longer produces. */
  private def planRow(partition: ReadModelProjection.Partition, trigger: ProjectTrigger): Seq[(ResolvedMovie, Seq[PlannedScreening])] = {
    val rowId     = partition.stored.id.value
    val cards     = cardProjection.plan(partition, trigger)
    lastGroups.update(rowId, RowGroups(partition.anchorKey, partition.cardByGroup,
      cards.iterator.flatMap(_.venues).filter(_.slotCount > 1).map(_._id).toSet))
    val variants  = cards.map(card => card.movie -> card.screenings)
    shareCardGate.dropHoldsNotProducedBy(rowId, variants.map(_._1._id).toSet)
    variants
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
    movieRepository.findById(models.FilmId(row)).foreach(projectRow(_, ProjectTrigger.ShareCard))
  }

  /** End the hold on card `filmId` now — its card can't be made (every poster failed for good) —
   *  and publish it with the fallback. */
  def releaseShareCardHold(filmId: String): Unit = lock.synchronized {
    shareCardGate.expireNow(filmId, clock.millis()).foreach { row =>
      movieRepository.findById(models.FilmId(row)).foreach(projectRow(_, ProjectTrigger.HoldRelease))
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
      movieRepository.findByIdChecked(models.FilmId(row)).fold(
        answered = whole => { projectRow(whole, ProjectTrigger.HoldRelease); false },
        absent   = { shareCardGate.dropHoldsNotProducedBy(row, Set.empty); false },   // gone: nothing to publish
        failed   = true)                                                              // unreadable: keep the hold
    }
  }

  /** Test seam: the cards the first-publish gate is holding. */
  private[readmodel] def heldCards: Set[String] = lock.synchronized(shareCardGate.heldCards)

  /** Caller holds `lock`. Remove one card and its screenings on the change-stream path. */
  private def retireCard(cardId: String, reason: String): Unit = {
    cardStore.remove(cardId, audit = s"stream-$reason")
    metrics.recordCardRetired(reason)
  }

  /** Caller holds `lock`. Re-project the CARDS of a row read without its showtimes, and rewrite
   *  each one the read model already serves that came out different. A card never reads a
   *  showtime (`ReadModelProjection.metadataHash`), so these are the cards [[project]] would write
   *  from the whole row. Everything that does depend on the showtimes is left to the paths that
   *  read them: a card's screenings, a card first published (the share-card gate asks whether it
   *  screens), a variant card that appears or goes (a derivation that moves those is not
   *  cards-only), and a row that lost its readiness. */
  private def projectCards(partition: ReadModelProjection.Partition): Int = {
    if (!partition.stored.readyToProject) return 0
    val cards = cardProjection.cards(partition, ProjectTrigger.Derivation).map(_._1)
    cards.filter(card => lastMovie.contains(card._id) && !shareCardGate.holds(card._id))
      .count(projected => cardStore.rewrite(shareCardGate.withShareCard(projected)))
  }

  /** Seed the last-projection state from the derived collections, so a restart doesn't
   *  rewrite documents that are already correct — and so a document this process did not
   *  write (one left under a renamed city's former slug, say) is one its next projection of
   *  that film can retire. What a worker does as it boots ([[ReadModelBoot]]); public so a harness can model the
   *  boot of the next worker over a read model it did not write. */
  def seedFromReadModel(): Unit = lock.synchronized {
    // A read that falls short seeds nothing: every card is then rewritten on its first projection,
    // the costly-but-safe direction.
    reader.findAllMoviesChecked().answered.getOrElse {
      logger.warn("read model: the cards could not be read whole at boot — seeding none; each is rewritten on its first projection")
      Seq.empty
    }.foreach(cardStore.seed)
    // Page by page, keeping each row's id and hash only: read whole, US's screenings were
    // ~400 MB live beside the identity take-up. The content is known, the slots it was
    // built from are not: no input hash, so the first projection of each venue rebuilds it
    // rather than trusting it. A read that falls short seeds nothing, as it always has.
    val byFilm = scala.collection.mutable.HashMap.empty[String, scala.collection.mutable.Builder[(String, WrittenScreening), Map[String, WrittenScreening]]]
    val complete = reader.foreachScreening(s => byFilm.getOrElseUpdate(s.filmId, Map.newBuilder) += s._id -> WrittenScreening(s.##, input = None))
    if (complete.isComplete) byFilm.foreach { case (fid, rows) => lastScreenings.update(fid, rows.result()) }
    memo.seeded = true
  }

  /** Learn every row of the boot hydrate's complete read. Before the seed the memo has no row to vouch:
   *  learning then would mark each row known with every venue unvouched. */
  private[readmodel] def learnAtBoot(rows: Seq[BootRow]): Unit = lock.synchronized {
    if (memo.seeded) rows.foreach(row => memo.learnFrom(row.id.value, row.lesson))
  }

  /** Learn every ready row from a read of the corpus with its showtimes — what [[learnAtBoot]] takes from the
   *  hydrate's read, when the boot had none to hand over (it failed, or its derivation ran past its wait). One row at
   *  a time under the lock, so the change stream's applies interleave; a change at a row not learned yet meanwhile is
   *  re-read whole, as it would be anyway. */
  private[readmodel] def learnFromCorpus(): Unit = {
    val read = movieRepository.foreachRecord { row =>
      if (row.readyToProject)
        scala.util.Try(Lesson.of(ReadModelProjection.partition(row, normalizer))).foreach(lesson =>
          lock.synchronized(if (memo.seeded) memo.learnFrom(row.id.value, lesson)))
    }
    logger.info(s"read model: learned the corpus's rows from a read of its own (${read.explain}).")
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
  private def heal(id: models.FilmId, metadataHash: Int, absentCards: Seq[String], absentVenues: Seq[String]): Option[Boolean] = {
    absentCards.foreach(cardStore.forget)
    absentVenues.foreach(cardStore.forgetScreening)
    movieRepository.findById(id).flatMap { whole =>
      projectRow(whole, ProjectTrigger.Heal)
      // Forgotten above, so remembered now only if this projection produced and wrote it.
      val written  = (venue: String) => lastScreenings.holds(venue)
      val repaired = absentCards.exists(lastMovie.contains) || absentVenues.exists(written)
      val phantoms = absentVenues.filterNot(written).toSet
      if (phantoms.nonEmpty) healedClean.update(id.value, (metadataHash, memo.phantomsOf(id.value, metadataHash) ++ phantoms))
      Option.when(whole.readyToProject)(repaired)
    }
  }
}

object ReadModelProjector {
  /** A projector and its stages over one memo — the [[CardProjection]] that projects a row's cards and the
   *  [[CardStore]] that writes them — as the worker's composition root and the specs build it. */
  def apply(
    movieRepository: MovieRepositoryReader & MovieChangeFeed,
    writer:          ReadModelWriter,
    reader:          ReadModelReader,
    metrics:         ReadModelProjectionMetrics = ReadModelProjectionMetrics.noop,
    cpuClock:        tools.ThreadCpuClock = tools.ThreadCpuClock.threadMxBean,
    shareCardGate:   ShareCardGate = ShareCardGate.ungated,
    // No default: the rolling content check numbers its slice by this clock, so a spec left on the
    // wall clock re-projected its row in the half-hours that row's slice came up (Main red, 2026-09-25).
    clock:           java.time.Clock,
    // Confidence-gated ratings (identity phase 3): what each card's ratings are served as. `off`
    // — the card as projected — unless the worker's composition root switches it on.
    ratingGate:      RatingGate = RatingGate.off,
    // What the projection's and the writes' durations are measured on: the root's.
    stopwatch:       tools.Stopwatch
  ): ReadModelProjector = {
    // The latest year a title may name, read off the projector's clock at each ask (`LatestTitleYear`).
    given LatestTitleYear = LatestTitleYear(clock)
    val memo          = new ProjectionMemo
    val screeningRows = new ScreeningRowWrites(writer, metrics, memo.lastScreenings)
    new ReadModelProjector(movieRepository, writer, reader, metrics, shareCardGate, clock, stopwatch, memo,
      cardProjection = new CardProjection(memo, screeningRows, ratingGate, metrics, cpuClock, stopwatch),
      cardStore      = new CardStore(writer, metrics, memo, screeningRows, shareCardGate))
  }
}
