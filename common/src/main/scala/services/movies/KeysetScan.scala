package services.movies

import tools.{RetryWithBackoff, ScanOutcome}

import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.{Failure, Success, Try}

/**
 * Keyset-paged scan of a whole Mongo collection by a unique, immutable `_id`-like
 * string key. Reads one `batchSize`-row page at a time (the next one prefetched while the
 * current one is consumed) — each a fresh, bounded,
 * independently-retried `find(_id > lastSeen).sort(_id).limit(batchSize)` — and hands
 * every batch to `onBatch`, rather than pulling the whole collection through ONE
 * unbounded `find().toFuture()`.
 *
 * Why paged, not one cursor: a single unbounded find over a large collection recurses
 * the async Mongo driver's per-message read-completion chain (`AsyncSupplier.finish` →
 * `AsyncCompletionHandler` → `SingleResultCallback.completed`) deep enough to throw
 * `StackOverflowError` on a driver I/O thread once the collection grows past a threshold
 * (Sentry KINOWO-19 on `movies`, then the same crash on `screenings`). Because the crash
 * lands on an uncaught I/O thread — not on the caller's `Await` — no `Try.recover` catches
 * it; it kills whatever boot/rehydrate path triggered the read. Keyset paging caps how
 * many rows any ONE cursor delivers synchronously, so the completion chain stays shallow.
 *
 * Exactly-once under concurrent writes: `fetchPage` must run a server-side
 * `_id > afterId` + sort-ascending + limit query against a unique, immutable `_id`, so a
 * concurrent write can neither resurface a visited row nor hide one — no duplicate at a
 * page boundary, no skip. `keyOf` extracts that `_id` from a decoded row.
 *
 * Answers [[ScanOutcome.Complete]] only when the scan reached the last page, and
 * [[ScanOutcome.Incomplete]] (with the read failure) when a page still failed after its
 * retries. Rows delivered so far still reached `onBatch`, so a PRUNING caller must treat
 * an incomplete scan as "not the complete collection" and skip its destructive step — the
 * outcome cannot be dropped unread (see [[ScanOutcome]]). `onIncomplete` is invoked once
 * with the failure so the caller can log it.
 *
 * Only a READ failure is "incomplete". An exception `onBatch` throws is the caller's own
 * bug and PROPAGATES: reporting it as incomplete read as "Mongo failed after retries", sent
 * pruning callers down their skip path under a misleading warning, and kept the bug
 * hidden for as long as it lasted. The in-memory repositories already behave this way
 * (their `foreachRecord` is a plain `foreach`), so this also keeps the fakes honest.
 */
object KeysetScan {

  def scan[A](
    label:          String,
    batchSize:      Int,
    maxAttempts:    Int,
    initialBackoff: FiniteDuration,
    keyOf:          A => String,
    fetchPage:      (Option[String], Int) => Seq[A],
    onIncomplete:   Throwable => Unit = _ => ()
  )(onBatch: Seq[A] => Unit): ScanOutcome = {
    // The page AND where the next one starts are the read: a last row whose key cannot be
    // taken is a scan that cannot continue, so it is incomplete like a failed fetch.
    def read(afterId: Option[String]): Future[(Seq[A], Option[String])] = Future(blocking {
      val batch = RetryWithBackoff(
        label          = label,
        maxAttempts    = maxAttempts,
        initialBackoff = initialBackoff
      )(fetchPage(afterId, batchSize))
      (batch, batch.lastOption.map(keyOf))
    })(using ExecutionContext.global)
    // The next page is fetched while this one is consumed: where it starts is known the moment
    // this one arrives, and a consumer (a boot hydrate stitching side rows and populating the
    // cache) spends about as long on a page as Mongo takes to deliver one, so reading them in
    // turn left each waiting on the other. At most two pages are ever held.
    var pending: Future[(Seq[A], Option[String])] = read(None)
    var outcome: Option[ScanOutcome] = None
    while (outcome.isEmpty) {
      Try(Await.result(pending, Duration.Inf)) match {
        case Success((batch, nextAfter)) =>
          val more = batch.sizeIs == batchSize
          if (more) pending = read(nextAfter)
          onBatch(batch)   // outside the Try: a consumer failure is not a read failure
          if (!more) outcome = Some(ScanOutcome.Complete)
        case Failure(exception) =>
          onIncomplete(exception)
          outcome = Some(ScanOutcome.Incomplete(exception))
      }
    }
    outcome.get
  }

  /** [[scan]], gathering what `decode` makes of each row — answered only when the scan read
   *  the whole collection, so a partial collection is never handed out as the whole one. */
  def collect[A, B](
    label:          String,
    batchSize:      Int,
    maxAttempts:    Int,
    initialBackoff: FiniteDuration,
    keyOf:          A => String,
    fetchPage:      (Option[String], Int) => Seq[A],
    onIncomplete:   Throwable => Unit = _ => ()
  )(decode: A => IterableOnce[B]): tools.ReadOutcome[Vector[B]] = {
    val rows = Vector.newBuilder[B]
    scan(label, batchSize, maxAttempts, initialBackoff, keyOf, fetchPage, onIncomplete)(_.foreach(rows ++= decode(_)))
      .collected(rows.result())
  }

  /** The rows of `keys`, a `batchSize` page at a time — `inFlight` pages fetched side by side and
   *  handed to `onBatch` in `keys`' order, on the calling thread. For a collection whose pages are
   *  each several round trips and a decode of large documents (a scrape archive row is a venue's
   *  whole listing): read one after another, the US archive's 179 pages were ~6 s of every identity
   *  projection, the CPU idle. Each page is retried as [[scan]] retries one, and the outcome is
   *  [[scan]]'s: incomplete once a page still failed, the batches before it already handed on, and a
   *  failure of `onBatch` propagates. A key whose row is gone by its page's read is simply absent. */
  def byKeys[A](
    label:          String,
    keys:           Seq[String],
    batchSize:      Int,
    inFlight:       Int,
    maxAttempts:    Int,
    initialBackoff: FiniteDuration,
    fetchKeys:      Seq[String] => Seq[A],
    onIncomplete:   Throwable => Unit = _ => ()
  )(onBatch: Seq[A] => Unit): ScanOutcome = {
    val pages = keys.grouped(batchSize).toVector
    def read(page: Seq[String]): Future[Seq[A]] = Future(blocking {
      RetryWithBackoff(label = label, maxAttempts = maxAttempts, initialBackoff = initialBackoff)(fetchKeys(page))
    })(using ExecutionContext.global)
    val reading = scala.collection.mutable.Queue.from(pages.take(inFlight).map(read))
    var next     = inFlight
    var outcome: ScanOutcome = ScanOutcome.Complete
    while (outcome.isComplete && reading.nonEmpty) {
      Try(Await.result(reading.dequeue(), Duration.Inf)) match {
        case Success(batch) =>
          if (next < pages.size) { reading.enqueue(read(pages(next))); next += 1 }
          onBatch(batch)   // outside the Try: a consumer failure is not a read failure
        case Failure(exception) =>
          onIncomplete(exception)
          outcome = ScanOutcome.Incomplete(exception)
      }
    }
    outcome
  }
}
