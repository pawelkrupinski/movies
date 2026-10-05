package services.readmodel

import play.api.Logging
import services.movies.{BootCorpusReader, BootReadEnd, StoredMovieRecord, TitleNormalizer}

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** What the read-model projector's boot reads take from the cache's boot hydrate, in place of a
 *  corpus read of their own.
 *
 *  The hydrate reads every film whole. From those rows this derives, off the hydrate's thread, the two
 *  things the projector's boot reads need:
 *   - what the missing-card check asks of each ready row;
 *   - the lesson the projector learns each row's venues from (`ReadModelProjector.learnFrom`).
 *
 *  Before this, the check read the corpus slots-only (4.0 s on a worker-us boot). The census's
 *  first pass then read it whole (12.5 s), two minutes after the hydrate had read the same rows
 *  (2026-10-04). Only ids, hashes and lessons are kept: each page is derived as it arrives, in
 *  order, and released once derived, so the study never holds more of the read than the pages its
 *  derivation has yet to reach.
 *
 *  It is separate from the projector so the hydrate can hand it the rows while the worker's
 *  composition root is still being built, before the projector has been built. */
final class BootCorpusStudy(normalizer: TitleNormalizer, wait: FiniteDuration = BootCorpusStudy.Wait)
  extends BootCorpusReader with Logging {
  import BootCorpusStudy.{NoRows, State}

  private val state = new AtomicReference[State](State.Open)
  private val study = Promise[Option[Seq[BootRow]]]()
  // The current read's pages, derived in the order they came. Touched only from the hydrate's thread.
  private var derived: Future[Vector[BootRow]] = NoRows

  def bootPage(rows: Seq[StoredMovieRecord]): Unit =
    // Only while the boot reads have not yet looked for it. If they have, they read for themselves,
    // and deriving it now would only cost CPU.
    if (state.compareAndSet(State.Open, State.Studying(study.future)) || state.get.isInstanceOf[State.Studying])
      derived = derived.map(_ ++ rows.collect { case row if row.record.readyToProject =>
        val partition = ReadModelProjection.partition(row, normalizer)
        BootRow(row.id, partition.filmIds, partition.screeningIds, ReadModelProjection.metadataHash(row), Lesson.of(partition))
      })(using ExecutionContext.global)

  def bootReadEnded(end: BootReadEnd): Unit = end match {
    case BootReadEnd.Whole    => study.completeWith(derived.map(Some(_))(using ExecutionContext.parasitic)); ()
    case BootReadEnd.Retrying => derived = NoRows
    case BootReadEnd.GaveUp   => derived = NoRows; study.trySuccess(None); ()
  }

  /** The hydrate's ready rows, derived, once the derivation is done. Asked once. It is `None` in
   *  three cases: nothing was offered, the derivation failed, or it ran past `wait`. The boot reads
   *  then read the corpus themselves. */
  private[readmodel] def take(): Option[Seq[BootRow]] = state.getAndSet(State.Closed) match {
    case State.Studying(rows) =>
      Try(Await.result(rows, wait)).fold(
        exception => { logger.warn(s"read model: the boot hydrate's rows could not be derived (${exception.getMessage}) — reading the corpus instead"); None },
        identity)
    case _ => None
  }
}

object BootCorpusStudy {
  /** How long the boot reads wait for the derivation before reading the corpus themselves. The
   *  derivation takes a few seconds, and the seed it waits behind takes longer still. */
  val Wait: FiniteDuration = FiniteDuration(60, "seconds")

  private val NoRows: Future[Vector[BootRow]] = Future.successful(Vector.empty)

  private enum State {
    case Open
    case Studying(rows: Future[Option[Seq[BootRow]]])
    case Closed
  }
}
