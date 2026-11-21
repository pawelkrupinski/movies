package services.movies

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

import com.mongodb.WriteConcern
import com.mongodb.client.model.ReplaceOptions
import org.bson.BsonDocument
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, SingleObservableFuture}
import play.api.Logging

/**
 * Where a change stream's resume position is kept, one document per stream (`_id = streamId`) —
 * the storage half of [[ChangeStreamResumeToken]], which owns when a position is advanced, saved,
 * cleared and opened from.
 *
 * Chosen at the composition root: the WORKER (the durable read-model / cache mirror) keeps its
 * positions in Mongo ([[MongoResumeTokenStore]]) so a restart replays what landed while it was down;
 * web /debug, scripts and specs keep none ([[ResumeTokenStore.Ephemeral]]), so an ephemeral viewer's
 * cursor position cannot clobber the worker's shared token. Best-effort: a failed save or clear is
 * logged and dropped, the consumers' backstops cover a miss.
 */
trait ResumeTokenStore {
  /** The saved position: absent when none was saved, FAILED when it could not be read. */
  def load(streamId: String): tools.ReadOutcome[BsonDocument]
  /** Save `token` as the stream's position; `synchronously` waits for the write (a clean
   *  shutdown), otherwise it is fire-and-forget. Completes once the write has landed or failed
   *  (a failure is logged here, never handed on): what a [[clear]] must wait for. */
  def save(streamId: String, token: BsonDocument, synchronously: Boolean): scala.concurrent.Future[Unit]
  /** Forget the stream's position, so the next open starts at "now". */
  def clear(streamId: String): Unit
}

object ResumeTokenStore {
  /** Keeps nothing: every open starts at "now". */
  val Ephemeral: ResumeTokenStore = new ResumeTokenStore {
    def load(streamId: String): tools.ReadOutcome[BsonDocument] = tools.ReadOutcome.none(s"$streamId resume token (not persisted)")
    def save(streamId: String, token: BsonDocument, synchronously: Boolean): scala.concurrent.Future[Unit] =
      scala.concurrent.Future.unit
    def clear(streamId: String): Unit = ()
  }

  /** The positions in `database`, or none kept where there is no database. */
  def in(database: Option[MongoDatabase]): ResumeTokenStore = database.fold(Ephemeral)(new MongoResumeTokenStore(_))
}

/** The positions in `change_stream_tokens`, written `{w:1, j:false}`: a lost save costs a replay
 *  from an older position, never data. */
final class MongoResumeTokenStore(database: MongoDatabase) extends ResumeTokenStore with Logging {
  private lazy val coll: MongoCollection[Document] =
    database.getCollection[Document]("change_stream_tokens").withWriteConcern(WriteConcern.W1.withJournal(false))

  def load(streamId: String): tools.ReadOutcome[BsonDocument] =
    tools.MongoRead.one(s"$streamId resume token", 5.seconds)(coll.find(Filters.eq("_id", streamId)).headOption())
      .flatMap(_.get("token").fold[tools.ReadOutcome[BsonDocument]](tools.ReadOutcome.none(s"$streamId resume token"))(
        token => tools.ReadOutcome.Answered(token.asDocument())))

  def save(streamId: String, token: BsonDocument, synchronously: Boolean): scala.concurrent.Future[Unit] = {
    val write = coll.replaceOne(Filters.eq("_id", streamId),
      Document("_id" -> streamId, "token" -> token), new ReplaceOptions().upsert(true)).toFuture()
      .transform(outcome => { outcome.failed.foreach(failed(s"save($streamId)")); scala.util.Success(()) })(
        using scala.concurrent.ExecutionContext.parasitic)
    if (synchronously) Try(Await.ready(write, 5.seconds)).failed.foreach(failed(s"save($streamId)"))
    write
  }

  def clear(streamId: String): Unit =
    Try(Await.result(coll.deleteOne(Filters.eq("_id", streamId)).toFuture(), 5.seconds)).failed.foreach(failed(s"clear($streamId)"))

  private def failed(what: String)(e: Throwable): Unit =
    logger.warn(s"change-stream resume token $what failed, dropped: ${e.getMessage}")
}
