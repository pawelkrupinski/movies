package services.readmodel

import play.api.Logging
import tools.Stoppable

import scala.util.Try

/**
 * The [[ReadModelProjector]]'s boot and its change-stream subscription: the state seed from the read model, the
 * sweeper's missing-card heal and the corpus lessons ([[prepare]]); the attach that holds every change the stream
 * delivers until those boot reads are done, and the release projecting them after ([[attach]]); the watch that opens
 * the stream ([[watch]]). The projector applies each change; this decides when it may.
 *
 * `scheduler` is the projector's thread — its learning off the boot thread; stopped by [[stop]]. `bootStudy` is what
 * the boot reads take from the cache's boot hydrate in place of a corpus read of their own (see [[BootCorpusStudy]]);
 * without one, they read the corpus themselves.
 */
final class ReadModelBoot(
  projector: ReadModelProjector,
  scheduler: java.util.concurrent.ScheduledExecutorService,
  bootStudy: Option[BootCorpusStudy] = None
) extends Stoppable with ContinuingSteps with Logging {
  private val projection = projector.projection
  private val memo       = projector.memo

  @volatile private var watchHandle: Option[AutoCloseable] = None

  /** Seed the last-projection state from the derived collections, so a restart doesn't
   *  rewrite documents that are already correct — and so a document this process did not
   *  write (one left under a renamed city's former slug, say) is one its next projection of
   *  that film can retire. What a worker does as it boots; public so a harness can model the
   *  boot of the next worker over a read model it did not write. */
  def seedFromReadModel(): Unit =
    memo.lock.synchronized {
      // A read that falls short seeds nothing: every card is then rewritten on its first projection,
      // the costly-but-safe direction.
      projection.reader.findAllMoviesChecked().answered.getOrElse {
        logger.warn("read model: the cards could not be read whole at boot — seeding none; each is rewritten on its first projection")
        Seq.empty
      }.foreach(projector.seedCard)
      // Page by page, keeping each row's id and hash only: read whole, US's screenings were
      // ~400 MB live beside the identity take-up. The content is known, the slots it was
      // built from are not: no input hash, so the first projection of each venue rebuilds it
      // rather than trusting it. A read that falls short seeds nothing, as it always has.
      val byFilm = scala.collection.mutable.HashMap.empty[String, scala.collection.mutable.Builder[(String, WrittenScreening), Map[String, WrittenScreening]]]
      val complete = projection.reader.foreachScreening(s => byFilm.getOrElseUpdate(s.filmId, Map.newBuilder) += s._id -> WrittenScreening(s.##, input = None))
      if (complete.isComplete) byFilm.foreach { case (fid, rows) => memo.lastScreenings.update(fid, rows.result()) }
      memo.seeded = true
    }

  /** The whole boot at once — [[prepare]] with `sweeper`'s heal, then [[watch]]; scheduling the
   *  sweeper's passes is its owner's. */
  def start(sweeper: ReadModelSweeper): Unit = { prepare(sweeper); watch() }

  /** The boot's reads — the state seed and `sweeper`'s missing-card heal — which read the read model and
   *  the `movies` store directly, never the in-memory cache, so a worker runs them from the first
   *  moment of its boot, beside the cache hydrate (28–57 s of a US boot, run before anything else
   *  started until 2026-09-30). Attached to the stream first ([[attach]]); what it delivers
   *  meanwhile is projected once they are done. */
  def prepare(sweeper: ReadModelSweeper): Unit = if (projector.enabled) {
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
  def attach(): Unit = if (projector.enabled) bootHold.synchronized {
    if (watchHandle.isEmpty)
      watchHandle = projection.movieRepository.attachChangesWithVenues(streamedUpsert, streamedDelete, streamedVenueSlots)
  }

  // The stream's changes, each applied unless the boot hold takes it.
  private def streamedUpsert(stored: services.movies.StoredMovieRecord): Unit = if (!heldForBoot(stored.id.value)) projector.onMovieUpsert(stored)
  private def streamedDelete(id: services.movies.FilmId): Unit = if (!heldForBoot(id.value)) projector.onMovieDelete(id)
  private def streamedVenueSlots(venues: services.movies.VenueSlots): services.movies.VenueVerdict =
    if (heldForBoot(venues.filmId.value)) services.movies.VenueVerdict.Applied else projector.onVenueSlots(venues)

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
          projection.movieRepository.findByIdChecked(id) match {
            case tools.ReadOutcome.Answered(row) => projector.onMovieUpsert(row)
            case tools.ReadOutcome.Absent(_)     => projector.onMovieDelete(id)
            case tools.ReadOutcome.Failed(_)     => logger.warn(s"read model: $rowId changed during the boot reads and could not be read — the prune sweep heals it.")
          }
        }
      }
      rows = bootHold.take()
    }
  }

  /** The live half: the change-stream watch. After [[prepare]]; the sweeper's paced passes are
   *  scheduled beside it by whoever built the sweeper ([[ReadModelSweeper.schedule]]). */
  def watch(): Unit = if (projector.enabled) {
    // The change-stream watch covers live changes from now on (and, via the persisted
    // resume token, replays every upsert missed while the worker was down); the seeded
    // state above means incremental writes are no-ops for already-correct documents. The full
    // reproject was retired (see the projector's doc).
    // Attached since `prepare`; opened here in case nothing else has opened the stream.
    attach()
    releaseBootHold()
    projection.movieRepository.openChanges()
    logger.info(s"ReadModelProjector started; no periodic reproject (retired); change-stream watch " +
      s"${if (watchHandle.isDefined) "active" else "unavailable — orphan-prune only"}.")
  } else logger.info("ReadModelProjector disabled (read model or movies repository not enabled).")

  /** Learn every row of the boot hydrate's complete read. Before the seed the memo has no row to vouch:
   *  learning then would mark each row known with every venue unvouched. */
  private def learnAtBoot(rows: Seq[BootRow]): Unit = memo.lock.synchronized {
    if (memo.seeded) rows.foreach(row => memo.learnFrom(row.id.value, row.lesson))
  }

  /** Learn every ready row from a read of the corpus with its showtimes — what [[learnAtBoot]] takes from the
   *  hydrate's read, when the boot had none to hand over (it failed, or its derivation ran past its wait). One row at
   *  a time under the lock, so the change stream's applies interleave; a change at a row not learned yet meanwhile is
   *  re-read whole, as it would be anyway. */
  private[readmodel] def learnFromCorpus(): Unit = {
    val read = projection.movieRepository.foreachRecord { row =>
      if (row.record.readyToProject)
        Try(Lesson.of(projection.partition(row))).foreach(lesson =>
          memo.lock.synchronized(if (memo.seeded) memo.learnFrom(row.id.value, lesson)))
    }
    logger.info(s"read model: learned the corpus's rows from a read of its own (${read.explain}).")
  }

  def stop(): Unit = {
    watchHandle.foreach(h => Try(h.close()))
    scheduler.shutdown()
  }
}
