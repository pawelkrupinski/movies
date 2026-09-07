package services.tasks

import com.mongodb.client.model.changestream.{ChangeStreamDocument, OperationType}
import org.mongodb.scala.{Document, Observer, Subscription}
import play.api.Logging
import services.movies.ChangeStreamReopen

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.FiniteDuration

/**
 * The `tasks` change-stream SUBSCRIPTION behind [[MongoTaskQueue.watchWaiting]]: one
 * cursor over the collection's inserts, ringing `onWaiting` per insert (an insert is
 * exactly "new claimable work" — see `watchWaiting`), REOPENED after it dies.
 *
 * A change stream's `onError` is terminal — the driver auto-resumes across transient
 * blips, but once it reports the death nothing brings the cursor back. `watchWaiting`
 * used to only log it, so one terminal error (a primary step-down, a network reset)
 * left every worker on the node deaf to new tasks until the process restarted: parked
 * on the 30s idle backstop instead of woken on enqueue. [[ChangeStreamReopen]] — the
 * same driver the movies stream uses — reopens on a capped backoff, reset only once
 * the new cursor DELIVERS an event. No resume token: the cursor reopens at "now", and
 * the pool's backstop claim picks up whatever was enqueued while it was down. On a
 * standalone (non-replica-set) Mongo every open dies at once and the reopen settles
 * at its longest delay — one attempt a minute, the backstop doing the work.
 *
 * `open` is the seam: production subscribes the collection's insert-filtered cursor
 * ([[MongoTaskQueue.watchWaiting]]); a spec keeps the observer and drives it by hand.
 * `schedule` is the reopen timer (a daemon scheduler in production, a recorder in specs).
 */
final class TaskInsertStream(
  open:      Observer[ChangeStreamDocument[Document]] => Unit,
  onWaiting: () => Unit,
  schedule:  (FiniteDuration, () => Unit) => Unit
) extends Logging with AutoCloseable {

  private val subscription = new AtomicReference[Subscription]()
  private val reopen       = new ChangeStreamReopen("MongoTaskQueue", () => subscribe(), schedule)

  /** Open the cursor; from here on it reopens itself until [[close]]. */
  def start(): Unit = subscribe()

  private def subscribe(): Unit = open(new Observer[ChangeStreamDocument[Document]] {
    override def onSubscribe(s: Subscription): Unit = { subscription.set(s); s.request(Long.MaxValue) }
    override def onNext(change: ChangeStreamDocument[Document]): Unit = {
      reopen.opened() // a delivered event is what proves the cursor healthy — reset the backoff
      // The server-side `$match` already keeps this to inserts; the guard is defence-in-depth.
      if (change.getOperationType == OperationType.INSERT)
        try onWaiting()
        catch { case exception: Throwable => logger.warn(s"Task queue doorbell ring failed: ${exception.getMessage}") }
    }
    override def onError(e: Throwable): Unit = {
      logger.warn(s"Task queue change stream ended (${e.getMessage}) — reopening on a backoff; the worker pool's idle backstop covers the meantime.")
      died()
    }
    override def onComplete(): Unit = died()
  })

  private def died(): Unit = { subscription.set(null); reopen.failed() }

  override def close(): Unit = {
    reopen.close()
    Option(subscription.getAndSet(null)).foreach(_.unsubscribe())
  }
}
