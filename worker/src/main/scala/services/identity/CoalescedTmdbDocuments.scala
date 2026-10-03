package services.identity

import org.bson.BsonDocument

import java.util.concurrent.{CompletableFuture, ConcurrentLinkedQueue, ExecutionException, Semaphore, TimeUnit, TimeoutException}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
 * `inner`, its concurrent whole-document reads ([[get]]) and writes ([[put]]) each made as ONE
 * round-trip per batch: a caller that finds a batch slot of its kind free runs the requests queued
 * meanwhile (up to [[CoalescedTmdbDocuments.MaxBatch]], [[CoalescedTmdbDocuments.InFlight]] batches at
 * once), as one `$in` or one bulk write, and the others wait for theirs.
 *
 * [[TmdbStore]] reads and writes back a document per answer it files, and a take-up of an empty store
 * files every film its questions name from the prefetch's 64 threads: four round-trips per film record,
 * ~216,000 on the US corpus, each its own command for the server and the driver. Coalesced they are a
 * few thousand. Nothing the store decides moves: its per-document locks still order two writes of one
 * document, and every call returns only once its own documents are read or written.
 *
 * A batch holding two writes of one id is written request by request, in the order they were queued;
 * one that fails is retried request by request, so one bad document fails only its own caller. Every
 * other call passes through.
 */
final class CoalescedTmdbDocuments(inner: TmdbDocuments, maxBatch: Int = CoalescedTmdbDocuments.MaxBatch) extends TmdbDocuments {
  import CoalescedTmdbDocuments.Coalescer

  private val reads = TmdbKind.values.map(kind => kind -> new Coalescer[Seq[String], Map[String, BsonDocument]](maxBatch)({ asked =>
    val found = inner.get(kind, asked.flatten.distinct)
    // A document two callers asked for is each one's own copy: a caller edits what it is given.
    val shared = asked.flatMap(_.distinct).groupBy(identity).collect { case (id, askers) if askers.sizeIs > 1 => id }.toSet
    asked.map(ids => Success(ids.flatMap(id => found.get(id).map(d => id -> (if (shared(id)) d.clone() else d))).toMap))
  })).toMap

  private val writes = TmdbKind.values.map(kind => kind -> new Coalescer[Seq[(String, BsonDocument)], Unit](maxBatch)({ batch =>
    val ids = batch.flatMap(_.map(_._1))
    if (ids.distinct.sizeIs == ids.size && Try(inner.put(kind, batch.flatten)).isSuccess) batch.map(_ => Success(()))
    else batch.map(docs => Try(inner.put(kind, docs)))
  })).toMap

  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = if (ids.isEmpty) Map.empty else reads(kind)(ids)
  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit    = if (docs.nonEmpty) writes(kind)(docs)

  override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.answers(kind, ids)
  def scan(kind: TmdbKind)(page: Seq[(String, Option[Long])] => Unit): Boolean      = inner.scan(kind)(page)
  def delete(kind: TmdbKind, ids: Seq[String]): Unit                                 = inner.delete(kind, ids)
}

object CoalescedTmdbDocuments {
  /** The most callers one round-trip serves. */
  val MaxBatch = 256
  /** How many batches of one kind may be in flight at once, each on a pooled connection: with one, a
   *  take-up's 64 prefetch threads waited on each other's round-trips while the server was idle. */
  val InFlight = 8
  /** How soon a caller whose request no batch has taken yet looks again for a free slot. */
  private val Recheck = 2L

  /** Concurrent `run`s of single requests as one `run` of many: `run` answers a batch with one
   *  outcome per request, in order, or throws for the whole batch. A caller that finds a slot free runs
   *  up to `maxBatch` queued requests, its own among them or not; at most `inFlight` batches run. */
  private[identity] final class Coalescer[A, B](maxBatch: Int, inFlight: Int = InFlight)(run: Seq[A] => Seq[Try[B]]) {
    private val queued = new ConcurrentLinkedQueue[(A, CompletableFuture[B])]()
    private val slots  = new Semaphore(inFlight)

    def apply(request: A): B = {
      val result = new CompletableFuture[B]()
      queued.add(request -> result)
      while (!result.isDone) {
        if (!queued.isEmpty && slots.tryAcquire()) try runBatch() finally slots.release()
        // Its request taken by another batch, or every slot taken: look again shortly.
        else try result.get(Recheck, TimeUnit.MILLISECONDS) catch { case _: TimeoutException | _: ExecutionException => () }
      }
      try result.get() catch { case e: ExecutionException => throw e.getCause }
    }

    // Every request the batch took is answered whatever `run` throws: an interrupt (a shutdown) or a
    // fatal error fails them all, then goes on up the runner's own stack.
    private def runBatch(): Unit = {
      val batch = Iterator.continually(queued.poll()).takeWhile(_ != null).take(maxBatch).toSeq
      if (batch.nonEmpty) {
        val outcomes =
          try run(batch.map(_._1))
          catch {
            case NonFatal(failed) => batch.map(_ => Failure(failed))
            case fatal: Throwable => batch.foreach(_._2.completeExceptionally(fatal)); throw fatal
          }
        batch.zip(outcomes).foreach { case ((_, result), outcome) =>
          outcome.fold(failed => result.completeExceptionally(failed), value => result.complete(value)) }
      }
    }
  }
}
