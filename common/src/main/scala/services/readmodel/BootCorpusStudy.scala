package services.readmodel

import play.api.Logging
import services.movies.{BootCorpusReader, StoredMovieRecord, TitleNormalizer}

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** What the read-model projector's boot reads take from the cache's boot hydrate, in place of a
 *  corpus read of their own.
 *
 *  The hydrate reads every film whole. From those rows this derives, on a thread of its own, the two
 *  things the projector's boot reads need:
 *   - what the missing-card check asks of each ready row;
 *   - the lesson the projector learns each row's venues from (`ReadModelProjector.learnFrom`).
 *
 *  Before this, the check read the corpus slots-only (4.0 s on a worker-us boot). The census's
 *  first pass then read it whole (12.5 s), two minutes after the hydrate had read the same rows
 *  (2026-10-04). Only ids, hashes and lessons are kept, and the rows are released when the
 *  derivation ends.
 *
 *  It is separate from the projector so the hydrate can hand it the rows while the worker's
 *  composition root is still being built, before the projector has been built. */
final class BootCorpusStudy(normalizer: TitleNormalizer, wait: FiniteDuration = BootCorpusStudy.Wait)
  extends BootCorpusReader with Logging {
  import BootCorpusStudy.State

  private val state = new AtomicReference[State](State.Open)

  def bootCorpus(read: Option[Seq[StoredMovieRecord]]): Unit = read.foreach { rows =>
    val study = Promise[Seq[BootRow]]()
    // Only while the boot reads have not yet looked for it. If they have, they read for themselves,
    // and deriving it now would only cost CPU.
    if (state.compareAndSet(State.Open, State.Studying(study.future)))
      study.completeWith(Future(rows.collect { case row if row.record.readyToProject =>
        val partition = ReadModelProjection.partition(row, normalizer)
        BootRow(row.id, partition.filmIds, partition.screeningIds, ReadModelProjection.metadataHash(row), Lesson.of(partition))
      })(using ExecutionContext.global))
    ()
  }

  /** The hydrate's ready rows, derived, once the derivation is done. Asked once. It is `None` in
   *  three cases: nothing was offered, the derivation failed, or it ran past `wait`. The boot reads
   *  then read the corpus themselves. */
  private[readmodel] def take(): Option[Seq[BootRow]] = state.getAndSet(State.Closed) match {
    case State.Studying(study) =>
      Try(Await.result(study, wait)).fold(
        exception => { logger.warn(s"read model: the boot hydrate's rows could not be derived (${exception.getMessage}) — reading the corpus instead"); None },
        Some(_))
    case _ => None
  }
}

object BootCorpusStudy {
  /** How long the boot reads wait for the derivation before reading the corpus themselves. The
   *  derivation takes a few seconds, and the seed it waits behind takes longer still. */
  val Wait: FiniteDuration = FiniteDuration(60, "seconds")

  private enum State {
    case Open
    case Studying(rows: Future[Seq[BootRow]])
    case Closed
  }
}
