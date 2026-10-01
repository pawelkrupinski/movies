package services.movies

import play.api.Logging

import java.util.concurrent.CopyOnWriteArrayList

/**
 * Fans a SINGLE change source out to many consumers, so the source is read and
 * decoded once regardless of how many listeners are attached.
 *
 * Why this exists: each `MovieRepository.watchChanges` caller used to open its
 * OWN Mongo change-stream cursor, so every write to `movies` was delivered and
 * BSON-decoded once PER consumer. The worker runs two consumers — `MovieCache`
 * (in-memory mirror) and `ReadModelProjector` (read-model projection) — so every
 * write was decoded and dispatched twice, and a live profiler showed that async
 * change-stream I/O completion was the worker's dominant CPU cost (it floored a
 * shared-cpu-4x machine). Routing both consumers through one cursor + this
 * registry halves that work: decode once, fan the decoded record out to all.
 *
 * `register` returns an `AutoCloseable` that removes exactly that listener; the
 * repository starts the underlying cursor on the first registration and stops it
 * once the last listener leaves (see [[isEmpty]]). Listeners live in a
 * copy-on-write list so dispatch (on the driver thread) never blocks
 * registration (on app threads), and each listener runs under its own try/catch
 * so one throwing consumer can't starve the others.
 */
final class ChangeStreamFanout[A, V](name: String) extends Logging {
  private final case class Listener(onUpsert: A => Unit, onDelete: String => Unit, onPart: V => VenueVerdict)
  private val listeners = new CopyOnWriteArrayList[Listener]()

  /** Attach a consumer; the returned handle detaches only that consumer. `onPart` takes a change
   *  delivered as a PART of the record (see [[dispatchPart]]) and answers whether it applied it —
   *  a consumer that cannot never does, and is handed the whole record instead. */
  def register(onUpsert: A => Unit, onDelete: String => Unit,
               onPart: V => VenueVerdict = (_: V) => VenueVerdict.Declined(ChangeStreamFanout.NoPartHandler)): AutoCloseable = {
    val entry = Listener(onUpsert, onDelete, onPart)
    listeners.add(entry)
    new AutoCloseable { override def close(): Unit = { listeners.remove(entry); () } }
  }

  /** True once every registered listener has detached — the signal the owning
   *  repository uses to stop the shared cursor. */
  def isEmpty: Boolean = listeners.isEmpty

  def dispatchUpsert(record: A): Unit = listeners.forEach { l =>
    try l.onUpsert(record)
    catch { case exception: Throwable => logger.warn(s"$name change-stream apply failed: ${exception.getMessage}") }
  }

  /** Offer every listener a part of the record; the reasons of those that declined it — empty only
   *  when EVERY one applied it. A declined part obliges the caller to [[dispatchUpsert]] the whole
   *  record — to every listener, the ones that applied the part included: a whole record is a
   *  superset of any part of it. A listener that throws declines it. */
  def dispatchPart(part: V): Seq[String] = {
    val declined = Seq.newBuilder[String]
    listeners.forEach { l =>
      try l.onPart(part) match {
        case VenueVerdict.Applied          => ()
        case VenueVerdict.Declined(reason) => declined += reason
      }
      catch { case exception: Throwable =>
        declined += ChangeStreamFanout.PartFailed
        logger.warn(s"$name change-stream part apply failed: ${exception.getMessage}") }
    }
    declined.result()
  }

  def dispatchDelete(id: String): Unit = listeners.forEach { l =>
    try l.onDelete(id)
    catch { case exception: Throwable => logger.warn(s"$name change-stream delete apply failed: ${exception.getMessage}") }
  }
}

object ChangeStreamFanout {
  /** A listener registered without a part handler declines every part. */
  val NoPartHandler = "no_part_handler"
  /** A listener's part handler threw. */
  val PartFailed    = "part_failed"
}
