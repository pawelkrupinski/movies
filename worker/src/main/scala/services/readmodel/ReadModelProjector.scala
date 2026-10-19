package services.readmodel

import services.movies.LatestTitleYear

import models.{CityScreening, ResolvedMovie}
import play.api.Logging
import services.Stoppable
import services.movies.{MovieRepository, StoredMovieRecord}
import settings.ShareCardFirstHold
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
 *  2. ORPHAN PRUNE (backstop) — a cheap, frequent id-only sweep ([[ReadModelSweeper]]'s `pruneOrphans`)
 *     that removes derived documents whose source film has vanished or was re-keyed
 *     and whose delete event this process never applied (it was down past the resume
 *     window, or the card was written by an earlier process — see `onMovieDelete`). It re-projects nothing, so it can't spike CPU.
 *
 * A deploy that changes what the projection DERIVES from an unchanged row reaches the stored
 * cards through neither: the sweeper's DERIVATION PASS (`advanceDerivationPass`) re-projects the whole
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
 * The change-stream callback and the sweeper's passes are serialised by one lock,
 * so the in-memory last-projection state needs no further synchronisation. This class owns
 * the per-row projection, its memo and the share-card hold; [[ReadModelSweeper]] the passes
 * over the whole corpus, which drive it through its [[projection]]. The projector builds no
 * sweeper: the composition root builds one over [[projection]] and schedules its passes.
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
  // Confidence-gated ratings (identity phase 3): what each card's ratings are served as. `off`
  // — the card as projected — unless the worker's composition root switches it on.
  ratingGate: RatingGate = RatingGate.off,
  // What the boot reads take from the cache's boot hydrate in place of a corpus read of their own
  // (see [[BootCorpusStudy]]). Without one, they read the corpus themselves.
  bootStudy: Option[BootCorpusStudy] = None
) extends Stoppable with ContinuingSteps with Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)
  // The projection keys rows by the repository's own `_id` formula, so it must
  // fold titles with the same rules the repository writes under — take them from
  // it rather than accepting a second, separately-wired copy that could disagree.
  private val normalizer: services.movies.TitleNormalizer = movieRepository.normalizer

  import ReadModelProjectionMetrics.{Op, ProjectTrigger, RetireReason, Target}

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
  private val lastScreenings = new ScreeningMemo
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
  // Each projected row's display-title groups and the card each files its venues under, from its last
  // projection: what lets [[onVenueSlots]] place a venue without the rest of the film. A few entries a row.
  private val lastGroups     = scala.collection.mutable.Map.empty[String, RowGroups]
  // Whether the boot seed has run: [[learn]] vouches venue rows against it.
  @volatile private var seeded  = false
  // Cards published by an expired hold that still have no share card: `shareCardPending` on their
  // documents, and seeded from them at boot, so a later projection keeps the mark until the card lands.
  private val pendingCards   = scala.collection.mutable.Set.empty[String]
  private val lock           = new AnyRef

  // The sweeper over [[projection]], when one is: what this projector tells it of each change it applies,
  // read and replaced under `lock` only.
  private var sweep: ReadModelSweeper.Listener = ReadModelSweeper.Listener.none

  /** What a [[ReadModelSweeper]] drives — this projector's stores, its lock, its per-row projection
   *  and its memo. The composition root builds the sweeper over it; a projector constructs none. */
  val projection: ReadModelSweeper.Projection = new ReadModelSweeper.Projection {
    def movieRepository: MovieRepository = ReadModelProjector.this.movieRepository
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
    def phantomsOf(rowId: String, metadataHash: Int): Set[String] = ReadModelProjector.this.phantomsOf(rowId, metadataHash)
    def noteRowCards(rowId: String, cards: Set[String]): Unit =
      if (!lastCardsByRow.contains(rowId)) lastCardsByRow.update(rowId, cards)
    def recordRowCards(rowId: String, cards: Set[String]): Unit = lastCardsByRow.update(rowId, cards)
    def retainRows(live: String => Boolean): Unit = {
      lastMetadata.filterInPlace((rowKey, _) => live(rowKey))
      lastCardsByRow.filterInPlace((rowKey, _) => live(rowKey))
      healedClean.filterInPlace((rowKey, _) => live(rowKey))
    }
    def removeCard(filmId: String, audit: String): Unit = ReadModelProjector.this.removeCard(filmId, audit)
    def forgetScreening(ref: ScreeningRef): Unit = lastScreenings.forget(ref.filmId, ref._id)
  }

  @volatile private var watchHandle: Option[AutoCloseable] = None

  def enabled: Boolean = writer.enabled && movieRepository.enabled

  /** Apply one source-row change from the change stream. */
  def onMovieUpsert(stored: StoredMovieRecord): Unit =
    lock.synchronized {
      sweep.noteApplied(stored.id.value)
      projectRow(stored, ProjectTrigger.Stream)
      // A second way out of a first-publish hold besides the task scheduled for its end: that
      // task may be claimed by a replica other than the one holding the card.
      // A row it could not read keeps its hold for the next sweep — see `releaseExpired`.
      if (held.nonEmpty) releaseExpired()
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
        byCard.foreach(applyVenues)
        if (held.nonEmpty) releaseExpired()
        services.movies.VenueVerdict.Applied
    }
  }

  /** Caller holds `lock`. Rebuild only the changed venues' rows of `card`: every other row of the
   *  card is carried as written — the whole-film projection would plan it unbuilt from the same
   *  inputs (and a row whose stored content drifted is the content check's, as it is under any
   *  venue apply). */
  private def applyVenues(card: String, changed: Seq[ReadModelProjection.VenueScreening]): Unit = {
    var upserted = 0
    val updated  = changed.map { venue =>
      val input = venue.inputHash
      val prior = lastScreenings.get(card, venue._id).get // present: `planVenues` declines a venue appearing
      if (prior.input.contains(input)) venue._id -> prior
      else {
        val screening = venue.screening
        val output    = screening.##
        if (output != prior.output) { writer.upsertScreening(screening); upserted += 1 }
        venue._id -> WrittenScreening(output, Some(input))
      }
    }
    if (upserted > 0) metrics.recordWrite(Target.Screening, Op.Upsert, upserted)
    metrics.recordVenueProjection(rebuilt = changed.size, reused = lastScreenings.size(card) - changed.size)
    lastScreenings.updateRows(card, updated)
  }

  /** Caller holds `lock`. The venues [[onVenueSlots]] rebuilds, by card — or the first reason it
   *  cannot vouch that they are all a whole-film projection would change (see there). */
  private def planVenues(rowId: String, venues: services.movies.VenueSlots): Either[String, Map[String, Seq[ReadModelProjection.VenueScreening]]] = {
    import services.movies.ChangeStreamMetrics.VenueDecline as Why
    scala.util.boundary {
      val RowGroups(anchor, cardByGroup, multiSlot) = lastGroups.getOrElse(rowId, scala.util.boundary.break(Left(Why.ProjectorRowUnprojected)))
      def refuse(reason: String) = scala.util.boundary.break(Left(reason))
      val cards   = cardByGroup.values.toSeq.distinct
      val planned = scala.collection.mutable.ArrayBuffer.empty[(String, ReadModelProjection.VenueScreening)]
      venues.atCinemas.foreach { case (cinema, slots) =>
        val byCard = slots.map { case (_, slot) =>
          cardByGroup.getOrElse(slot.title.map(normalizer.sanitize).getOrElse(anchor), refuse(Why.ProjectorTitleGroup)) -> slot
        }.groupMap(_._1)(_._2)
        if (byCard.exists(_._2.sizeIs > 1)) refuse(Why.ProjectorTwoSlots)
        cards.foreach { card =>
          val before = models.City.forCinema(cinema).map(ReadModelProjection.screeningId(card, _, cinema))
            .filter(lastScreenings.contains(card, _))
          val after  = byCard.get(card).flatMap(slots => ReadModelProjection.venueOf(cinema, slots.head, card))
          (before, after) match {
            case (None, None)                                  => ()
            case (None, Some(_))                               => refuse(Why.ProjectorVenueAppears)
            case (Some(_), None)                               => refuse(Why.ProjectorVenueVanishes)
            case (Some(id), Some(venue)) if venue._id != id || multiSlot(id) => refuse(Why.ProjectorUnionedVenue)
            case (Some(_), Some(venue))                        => planned += card -> venue
          }
        }
      }
      val byCard = planned.toSeq.groupMap(_._1)(_._2)
      byCard.keys.foreach { card =>
        if (held.contains(card)) refuse(Why.ProjectorCardHeld)
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
    // through `writer.upsertMovie`/`diffScreenings`, sequentially; this is the phase that
    // scales with city count, not with resolve/synopsisByCity/ratings cost.
    val writing = tools.Stopwatch.start()
    // A content check's writes are DRIFT CANDIDATES — the stored projection disagreed with the
    // source — held by what moved for the sweep's verdict, which counts and names only the rows
    // no event then repaired, the film ids being what a drift investigation has to start from.
    val drift   = Option.when(trigger == ProjectTrigger.Content)(new RowDriftLog)
    val now     = clock.millis()
    val publish = variants.flatMap { case (projected, screenings) =>
      gate(rowId, projected, screened = screenings.nonEmpty, now).map(_ -> screenings)
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
    dropHoldsNotProducedBy(rowId, Set.empty)
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
    val variants  = projected.map { (movie, venues) => (movie, planScreenings(movie._id, venues)) }
    lastGroups.update(rowId, RowGroups(partition.anchorKey, partition.cardByGroup,
      projected.iterator.flatMap(_._2).filter(_.slotCount > 1).map(_._id).toSet))
    dropHoldsNotProducedBy(rowId, variants.map(_._1._id).toSet)
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
    val screeningDiff = diffScreenings(movie._id, screenings)
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

  /** Caller holds `lock`. Remember a card as written at `hash`, and tell the share-card ledger. */
  private def rememberCard(movie: ResolvedMovie, hash: CardHash, screened: Boolean): Unit = {
    lastMovie.update(movie._id, hash)
    shareCards.onProjected(movie, screened = screened)
  }

  /** Caller holds `lock`. A variant card this row produced last time and no longer does — its
   *  decorated listing vanished — is retired here, by the path that knows, not by the prune. A card
   *  still held by the first-publish gate was never produced. A card held back while its unserved
   *  document stands (see `gate`) is not gone either. */
  private def retireVariantsGone(rowId: String, produced: Set[String]): Unit = {
    val before   = lastCardsByRow.getOrElse(rowId, Set.empty)
    val kept     = produced ++ before.filter(held.contains)
    (before -- kept).foreach(retireCard(_, RetireReason.VariantGone))
    lastCardsByRow.update(rowId, kept)
    // A venue this projection wrote is no phantom any more: if its row goes missing now, that
    // is a real loss the heal must repair.
    val served = kept.flatMap(card => lastScreenings.ids(card).toSet)
    healedClean.updateWith(rowId)(_.map { case (hash, phantoms) => (hash, phantoms -- served) }.filter(_._2.nonEmpty))
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
    val served   = lastMovie.contains(id) && lastScreenings.holdsAny(id)
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
    // stream event: a sweep between the two found it absent (see [[ReadModelSweeper]]'s `verdictOnSweep`).
    sweep.noteApplied(row)
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
        case tools.ReadOutcome.Answered(whole)     => projectRow(whole, ProjectTrigger.HoldRelease); false
        case tools.ReadOutcome.Absent(_)  => dropHoldsNotProducedBy(row, Set.empty); false             // gone: nothing to publish
        case tools.ReadOutcome.Failed(_)  => true                                                       // unreadable: keep the hold
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

  /** Caller holds `lock`. Build the screenings rows of one card — but only for the venues
   *  whose inputs moved since their row was written. A venue whose [[WrittenScreening]]
   *  carries the same input hash already has exactly the row it would build, so it is
   *  carried as `None`: still a live id (not deleted), not rebuilt, not rewritten. This is
   *  what turns a showtime change at one venue of a wide release from thousands of rows
   *  built into one. A memo that cannot vouch — seeded from the read model at boot, or
   *  dropped by a heal — has no input hash, so every such venue is rebuilt. */
  private def planScreenings(filmId: String, venues: Seq[ReadModelProjection.VenueScreening]): Seq[PlannedScreening] = {
    val planned  = venues.map { venue =>
      val input = venue.inputHash
      val built = if (lastScreenings.get(filmId, venue._id).exists(_.input.contains(input))) None else Some(venue.screening)
      PlannedScreening(venue._id, input, built)
    }
    val rebuilt = planned.count(_.built.isDefined)
    metrics.recordVenueProjection(rebuilt = rebuilt, reused = planned.size - rebuilt)
    planned
  }

  /** Returns the number of screening documents written (upserts + deletes). */
  private def diffScreenings(filmId: String, next: Seq[PlannedScreening]): ScreeningDiff = {
    val previous = lastScreenings.of(filmId)
    val upserts  = Seq.newBuilder[CityScreening]
    val nextById = next.map { planned =>
      val output = planned.built match {
        case Some(s) =>
          val hash = s.##
          if (!previous.get(planned._id).exists(_.output == hash)) upserts += s
          hash
        // Not rebuilt: `planScreenings` only skips a venue whose entry vouches for its row.
        case None => previous(planned._id).output
      }
      planned._id -> WrittenScreening(output, Some(planned.input))
    }.toMap
    val written  = upserts.result()
    writer.upsertScreenings(written)
    val upserted = written.size
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
    lastScreenings.update(filmId, nextById)
    ScreeningDiff(written.map(_._id), deletes)
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
    cards.filter(card => lastMovie.contains(card._id) && !held.contains(card._id)).count { projected =>
      val movie   = withShareCard(projected)
      val hash    = CardHash.of(movie)
      val changed = !lastMovie.get(movie._id).contains(hash)
      if (changed) {
        upsertCard(movie, hash)
        rememberCard(movie, hash, screened = lastScreenings.holdsAny(movie._id))
      }
      changed
    }
  }

  /** Seed the last-projection state from the derived collections, so a restart doesn't
   *  rewrite documents that are already correct — and so a document this process did not
   *  write (one left under a renamed city's former slug, say) is one its next projection of
   *  that film can retire. What a worker does as it boots; public so a harness can model the
   *  boot of the next worker over a read model it did not write. */
  def seedFromReadModel(): Unit =
    lock.synchronized {
      // A read that falls short seeds nothing: every card is then rewritten on its first projection,
      // the costly-but-safe direction.
      reader.findAllMoviesChecked().answered.getOrElse {
        logger.warn("read model: the cards could not be read whole at boot — seeding none; each is rewritten on its first projection")
        Seq.empty
      }.foreach { m =>
        lastMovie.update(m._id, CardHash.of(m))
        if (m.shareCardPending) pendingCards += m._id
      }
      // Page by page, keeping each row's id and hash only: read whole, US's screenings were
      // ~400 MB live beside the identity take-up. The content is known, the slots it was
      // built from are not: no input hash, so the first projection of each venue rebuilds it
      // rather than trusting it. A read that falls short seeds nothing, as it always has.
      val byFilm = scala.collection.mutable.HashMap.empty[String, scala.collection.mutable.Builder[(String, WrittenScreening), Map[String, WrittenScreening]]]
      val complete = reader.foreachScreening(s => byFilm.getOrElseUpdate(s.filmId, Map.newBuilder) += s._id -> WrittenScreening(s.##, input = None))
      if (complete.isComplete) byFilm.foreach { case (fid, rows) => lastScreenings.update(fid, rows.result()) }
      seeded = true
    }

  /** The whole boot at once — [[prepare]] with `sweeper`'s heal, then [[watch]]; scheduling the
   *  sweeper's passes is its owner's. */
  def start(sweeper: ReadModelSweeper): Unit = { prepare(sweeper); watch() }

  /** The boot's reads — the state seed and `sweeper`'s missing-card heal — which read the read model and
   *  the `movies` store directly, never the in-memory cache, so a worker runs them from the first
   *  moment of its boot, beside the cache hydrate (28–57 s of a US boot, run before anything else
   *  started until 2026-09-30). Attached to the stream first ([[attach]]); what it delivers
   *  meanwhile is projected once they are done. */
  def prepare(sweeper: ReadModelSweeper): Unit = if (enabled) {
    attach()
    // A read that THROWS ends the boot reads, never the boot: [[watch]] runs after this on the same
    // boot thread, and a throw out of here skipped it — no stream projection and no sweeps, for the
    // life of the process. Without a seed each card is rewritten on its first projection, and the
    // first sweep heals what the missing-card heal would have: costly, but the projector runs.
    try {
      seedFromReadModel()
      val study = bootStudy.flatMap(_.take())
      sweeper.healMissingCards(study)
      // Without the hydrate's rows, read them for the lessons, off the boot thread: without them every film's first
      // venue change after the boot is re-read whole (78 of the US's 78 venue applies in half an hour, 2026-10-01).
      study.fold { scheduler.execute(() => continuing("read model: learning the corpus failed")(learnFromCorpus())); () }(learnAtBoot)
    } catch {
      case scala.util.control.NonFatal(exception) =>
        logger.error(s"read model: the boot reads failed (${exception.getClass.getSimpleName}: ${exception.getMessage}) — " +
          "projecting without them; the sweeps heal what they would have", exception)
    } finally releaseBootHold()
  }

  /** Attach to the change stream without opening it, holding every change it delivers until the
   *  boot reads are done ([[prepare]]). [[prepare]] calls it; a worker calls it first, before the
   *  cache opens the stream, since its [[prepare]] runs in the background beside the cache hydrate.
   *  Attached any later, the projector missed every change the stream applied to the cache before
   *  it, and the resume position moved past them for good: a venue a scrape added stayed missing
   *  until the first prune sweep healed it — all four of 2026-09-30's first-sweep heals, each write
   *  landing 1-4 s before the watch — and one a scrape removed stayed served until the content check
   *  reached its row: us/chicago a card over its corpus for 22 hours from 2026-09-30 08:55.
   *  Idempotent. */
  def attach(): Unit = if (enabled) bootHold.synchronized {
    if (watchHandle.isEmpty)
      watchHandle = movieRepository.attachChangesWithVenues(
        stored => if (!heldForBoot(stored.id.value)) onMovieUpsert(stored),
        id     => if (!heldForBoot(id.value)) onMovieDelete(id),
        venues => if (heldForBoot(venues.filmId.value)) services.movies.VenueVerdict.Applied else onVenueSlots(venues))
  }

  // The rows the stream changed while the boot reads ran — read again and projected once they are
  // done, never applied mid-seed or mid-heal.
  private val bootHold = new BootHold

  /** Whether a change to `rowId` is held for the end of the boot reads (and, if so, recorded). */
  private def heldForBoot(rowId: String): Boolean = bootHold.holds(rowId)

  /** End the boot hold: project every row the stream changed meanwhile, as it stands now, until
   *  none is left — only then does the stream apply its changes directly, so none lands between the
   *  last read and the switch. A row gone is retired; one that cannot be read is left to the next
   *  prune sweep's heal. Idempotent. */
  private def releaseBootHold(): Unit = {
    var rows = bootHold.take()
    while (rows.nonEmpty) {
      rows.foreach { rowId =>
        val id = services.movies.FilmId(rowId)
        continuing(s"read model: applying the change to $rowId held through the boot reads failed") {
          movieRepository.findByIdChecked(id) match {
            case tools.ReadOutcome.Answered(row)      => onMovieUpsert(row)
            case tools.ReadOutcome.Absent(_) => onMovieDelete(id)
            case tools.ReadOutcome.Failed(_) => logger.warn(s"read model: $rowId changed during the boot reads and could not be read — the prune sweep heals it.")
          }
        }
      }
      rows = bootHold.take()
    }
  }

  /** The live half: the change-stream watch. After [[prepare]]; the sweeper's paced passes are
   *  scheduled beside it by whoever built the sweeper ([[ReadModelSweeper.schedule]]). */
  def watch(): Unit = if (enabled) {
    // The change-stream watch covers live changes from now on (and, via the persisted
    // resume token, replays every upsert missed while the worker was down); the seeded
    // state above means incremental writes are no-ops for already-correct documents. The full
    // reproject was retired (see class doc).
    // Attached since `prepare`; opened here in case nothing else has opened the stream.
    attach()
    releaseBootHold()
    movieRepository.openChanges()
    logger.info(s"ReadModelProjector started; no periodic reproject (retired); change-stream watch " +
      s"${if (watchHandle.isDefined) "active" else "unavailable — orphan-prune only"}.")
  } else logger.info("ReadModelProjector disabled (read model or movies repository not enabled).")

  /** Learn a row the change stream has not projected since this worker booted, from the boot hydrate's
   *  read of it (every film with its showtimes, see [[BootCorpusStudy]]) and without writing: its title
   *  groups, and the inputs of each venue row whose stored content is exactly what the row projects to.
   *  That is what [[onVenueSlots]] needs to apply a change at a few of its venues from those venues
   *  alone; the boot seed knows only the rows' contents. Without it each film's first change after
   *  every deploy re-read the whole film (`projector_row_unprojected`: 78 of the US's 78 declines in
   *  half an hour, 2026-10-01). A venue row whose content differs is drift and stays unvouched — a
   *  change at its card is re-read whole, and the content check rewrites it. A row the stream has
   *  projected meanwhile is left alone: it knows better. Caller holds `lock`. */
  private def learnFrom(rowId: String, lesson: Lesson): Unit = if (!lastGroups.contains(rowId)) {
    lesson.cards.foreach { case (card, venues) =>
      val vouched = venues.flatMap { venue =>
        lastScreenings.get(card, venue.id).filter(row => row.input.isEmpty && row.output == venue.output)
          .map(row => venue.id -> WrittenScreening(row.output, Some(venue.input)))
      }
      lastScreenings.updateRows(card, vouched)
    }
    lastGroups.update(rowId, RowGroups(lesson.anchorKey, lesson.cardByGroup, lesson.multiSlot))
  }

  /** Learn every row of the boot hydrate's complete read. Before the seed the memo has no row to vouch:
   *  learning then would mark each row known with every venue unvouched. */
  private def learnAtBoot(rows: Seq[BootRow]): Unit = lock.synchronized {
    if (seeded) rows.foreach(row => learnFrom(row.id.value, row.lesson))
  }

  /** Learn every ready row from a read of the corpus with its showtimes — what [[learnAtBoot]] takes from the
   *  hydrate's read, when the boot had none to hand over (it failed, or its derivation ran past its wait). One row at
   *  a time under the lock, so the change stream's applies interleave; a change at a row not learned yet meanwhile is
   *  re-read whole, as it would be anyway. */
  private[readmodel] def learnFromCorpus(): Unit = {
    val read = movieRepository.foreachRecord { row =>
      if (row.record.readyToProject)
        Try(Lesson.of(ReadModelProjection.partition(row, normalizer))).foreach(lesson =>
          lock.synchronized(if (seeded) learnFrom(row.id.value, lesson)))
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
  private def heal(id: services.movies.FilmId, metadataHash: Int, absentCards: Seq[String], absentVenues: Seq[String]): Option[Boolean] = {
    absentCards.foreach(forgetCard)
    absentVenues.foreach(forgetScreening)
    movieRepository.findById(id).flatMap { whole =>
      projectRow(whole, ProjectTrigger.Heal)
      // Forgotten above, so remembered now only if this projection produced and wrote it.
      val written  = (venue: String) => lastScreenings.holds(venue)
      val repaired = absentCards.exists(lastMovie.contains) || absentVenues.exists(written)
      val phantoms = absentVenues.filterNot(written).toSet
      if (phantoms.nonEmpty) healedClean.update(id.value, (metadataHash, phantomsOf(id.value, metadataHash) ++ phantoms))
      Option.when(whole.record.readyToProject)(repaired)
    }
  }

  /** The venues a heal found to be phantoms of this row, while its metadata is what it was. */
  private def phantomsOf(rowId: String, metadataHash: Int): Set[String] =
    healedClean.get(rowId).collect { case (hash, phantoms) if hash == metadataHash => phantoms }.getOrElse(Set.empty)
  private def forgetCard(filmId: String): Unit = {
    lastMovie.remove(filmId)
    held.remove(filmId)
    pendingCards.remove(filmId)
    lastScreenings.forgetCard(filmId)
  }

  /** Drop one screenings row from the memo, by its id — its film is whichever memo
   *  entry holds the id, so the card id need not be parsed out of it. */
  private def forgetScreening(screeningId: String): Unit =
    lastScreenings.forget(screeningId)

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}

object ReadModelProjector {
  /** Two minutes: a render is a poster fetch (up to ~35s against a slow cinema origin) plus a
   *  composite, so this covers a cold one with room to spare while keeping a new film's first
   *  appearance close to its scrape. */
  val DefaultFirstCardHold: scala.concurrent.duration.FiniteDuration =
    scala.concurrent.duration.Duration(120L, TimeUnit.SECONDS)
}
