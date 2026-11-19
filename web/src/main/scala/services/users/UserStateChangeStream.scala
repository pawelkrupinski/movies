package services.users

import tools.ChangeStreamLiveness

import com.mongodb.client.model.changestream.{ChangeStreamDocument, FullDocument}
import models.UserState
import org.bson.BsonDocument
import org.mongodb.scala.{MongoCollection, MongoDatabase, Observer, Subscription}
import play.api.Logging
import services.movies.{ChangeEventDecoder, ChangeStreamReopen}

/** The `userStates` change stream behind [[MongoUserStateRepository.watchChanges]]: one consumer at a time (see
 *  [[UserStateRepository.watchChanges]]), a dead cursor reopened by its [[ChangeStreamReopen]] driver, and its
 *  [[ChangeStreamLiveness]]. */
class UserStateChangeStream(
  // The users database; `None` watches nothing.
  database: Option[MongoDatabase],
  // The composition root's clock: the `userStates` cursor's liveness ages on it.
  clock: java.time.Clock,
  // The composition root's monotonic clock: the cursor's decoder log throttle times on it.
  stopwatch: tools.Stopwatch,
  // The reopen driver for the `userStates` cursor. Production schedules on a daemon
  // thread; a spec hands over one that fires when it says so.
  reopenDriver: (String, () => Unit) => ChangeStreamReopen = ChangeStreamReopen.onDaemonScheduler,
  // Where a row the change stream could not decode is counted — the web's decode-failure
  // counter in production. See [[services.movies.ChangeEventDecoder]].
  decodeFailures: services.movies.DecodeFailureMetrics = services.movies.DecodeFailureMetrics.noop
) extends Logging {

  private lazy val coll: Option[MongoCollection[UserState]] = database.map(UserStatesCollection.of)

  val liveness = new ChangeStreamLiveness(clock)
  // Written by the caller's thread, read on the driver's: volatile so a registration (or
  // its close) is seen by the next event rather than whenever the cache line happens to move.
  @volatile private var listener: Option[(UserState => Unit, String => Unit, () => Unit)] = None
  @volatile private var subscription: Option[Subscription] = None
  private lazy val reopen: ChangeStreamReopen = reopenDriver("userStates", () => subscribe(reopened = true))

  /** Replaces any existing registration (see the trait doc — one consumer at a
   *  time). Opens at "now": unlike `MovieChangeStream` this cache has nothing
   *  durable to resume — a missed event just means that one user's cache entry
   *  stays a miss until their next write, which is harmless (the caller reads
   *  storage directly on a miss). */
  def watch(
    onUpsert:     UserState => Unit,
    onDelete:     String => Unit,
    onDisconnect: () => Unit
  ): Option[AutoCloseable] = coll.map { _ =>
    listener = Some((onUpsert, onDelete, onDisconnect))
    subscribe(reopened = false)
    new AutoCloseable {
      override def close(): Unit = {
        reopen.close()
        subscription.foreach(_.unsubscribe())
        subscription = None
        listener = None
      }
    }
  }

  /** `reopened`: this open replaces a cursor that DIED. It opens at "now", so every write
   *  between the death and this open was never delivered — and the death's `onDisconnect`
   *  only cleared what was cached THEN, not what callers cached during the gap. So a reopen
   *  reports losing track again, once the new cursor is live. */
  private def subscribe(reopened: Boolean): Unit = coll.foreach { c =>
    // Decoded here rather than in the driver: one row the codec refuses used to END the cursor.
    val decoder = ChangeEventDecoder.of[UserState](UserStateRepository.Collection, c.codecRegistry, decodeFailures, stopwatch)
    c.watch[BsonDocument]().fullDocument(FullDocument.UPDATE_LOOKUP).subscribe(new Observer[ChangeStreamDocument[BsonDocument]] {
      override def onSubscribe(s: Subscription): Unit = {
        subscription = Some(s)
        s.request(Long.MaxValue)
        liveness.watching(UserStateRepository.Collection)
        if (reopened) listener.foreach { case (_, _, onLostTrack) => onLostTrack() }
      }
      override def onNext(change: ChangeStreamDocument[BsonDocument]): Unit = deliver(decoder.postImage(change), change)
      override def onError(e: Throwable): Unit = {
        logger.warn(s"UserStateRepository change stream ended (${e.getMessage}) — invalidating the cache; a reopen resumes it.")
        cursorEnded()
      }
      override def onComplete(): Unit = cursorEnded()
    })
  }

  /** One change, to the listener. `image` is decoded whether or not one listens: a refused row is counted either way. */
  private def deliver(image: ChangeEventDecoder.PostImage[UserState], change: ChangeStreamDocument[BsonDocument]): Unit = {
    reopen.opened()
    liveness.delivered(UserStateRepository.Collection)
    listener.foreach { case (onUpsert, onDelete, onLostTrack) =>
      image match {
        case ChangeEventDecoder.PostImage.Present(state) => onUpsert(state)
        // A row changed and we cannot say whose, or what to: stop vouching, as for a delete.
        case ChangeEventDecoder.PostImage.Undecodable    => onLostTrack()
        // A delete carries only its documentKey — and a row's `_id` is a driver-generated
        // ObjectId, not the userId, so the key cannot say WHOSE row went (and there is no
        // pre-image to ask). Deletes are rare (account deletion), so rather than enable
        // collection pre-images, report "can no longer vouch for what changed" — the same
        // signal as a dead cursor, which makes the change-time cache drop everything.
        case ChangeEventDecoder.PostImage.Absent =>
          Option(change.getDocumentKey).flatMap(k => Option(k.get("userId")))
            .fold(onLostTrack())(v => onDelete(if (v.isString) v.asString.getValue else v.toString))
      }
    }
  }

  private def cursorEnded(): Unit = {
    subscription = None
    listener.foreach { case (_, _, onDisconnect) => onDisconnect() }
    reopen.failed()
  }

  def close(): Unit = reopen.close()
}
