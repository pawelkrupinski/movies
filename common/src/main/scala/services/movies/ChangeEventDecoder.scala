package services.movies

import com.mongodb.client.model.changestream.ChangeStreamDocument
import org.bson.{BsonDocument, BsonDocumentReader}
import org.bson.codecs.{Decoder, DecoderContext}
import org.bson.codecs.configuration.CodecRegistry
import play.api.Logging
import services.readmodel.DecodeFailureMetrics

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/**
 * Decodes a change event's post-image OURSELVES, after the driver has handed it over as a
 * plain `BsonDocument` — so a document the codec refuses costs that one event, not the cursor.
 *
 * WHY. Every watcher used to open its stream typed (`collection.watch()` on a
 * `MongoCollection[Dto]`), which makes the DRIVER decode each post-image inside the cursor. A
 * document the codec refuses — a `screenings` row with no `filmId`, a wrong-typed field left by
 * a writer that changed shape — then fails the cursor itself: `onError`, terminal. Found
 * 2026-09-24 through an itAll flake (a sibling spec's `filmId`-less `screenings` row ended
 * another spec's stream), and worse in production than in a test: a stream that resumes from a
 * persisted token resumes from BEFORE the bad event, meets it again, and dies again on every
 * reopen — a stream dead for good, and SILENT, since nothing downstream fails loudly (see
 * [[ChangeStreamReopen]] for the last silent-stream outage).
 *
 * What a watcher does with [[ChangeEventDecoder.PostImage.Undecodable]] is its own call (skip
 * it, release its demand, keep its resume position where it is — the event was not applied);
 * what this does is make the skip possible, count it on `decode_failures_total{collection}`
 * (the `DocumentsUndecodable` alert), and say so in the log — at most once a minute per
 * collection, naming the `_id` and how many more it skipped since, so a writer emitting bad
 * documents in bulk cannot flood the log.
 */
final class ChangeEventDecoder[T](
  collection: String,
  decoder:    Decoder[T],
  failures:   DecodeFailureMetrics,
  // Monotonic nanoseconds for the log's rate limit — injected so a spec can step it.
  nanoTime:   () => Long = () => System.nanoTime()
) extends Logging {
  import ChangeEventDecoder.PostImage

  private val lastLogged = new AtomicLong(Long.MinValue)
  private val suppressed = new AtomicInteger(0)

  /** The event's post-image, decoded — `Absent` when it carries none (a delete, a drop). */
  def postImage(change: ChangeStreamDocument[BsonDocument]): PostImage[T] =
    Option(change.getFullDocument) match {
      case None      => PostImage.Absent
      case Some(raw) =>
        try PostImage.Present(decoder.decode(new BsonDocumentReader(raw), DecoderContext.builder().build()))
        catch {
          case NonFatal(exception) =>
            failures.recordDecodeFailure(collection)
            warn(ChangeEventDecoder.idOf(raw), exception)
            PostImage.Undecodable
        }
    }

  private def warn(id: String, exception: Throwable): Unit = {
    val now  = nanoTime()
    val last = lastLogged.get()
    if ((last == Long.MinValue || now - last >= ChangeEventDecoder.LogIntervalNanos) && lastLogged.compareAndSet(last, now)) {
      val since = suppressed.getAndSet(0)
      logger.warn(s"$collection change stream: skipping undecodable document _id=$id " +
        s"(${exception.getClass.getSimpleName}: ${exception.getMessage}) — the stream stays open, the event is " +
        s"not applied" + (if (since > 0) s"; $since more skipped since the last line" else "") + ".")
    } else suppressed.incrementAndGet()
  }
}

object ChangeEventDecoder {
  /** What an event's post-image turned out to be. */
  enum PostImage[+T] {
    case Present(value: T)
    case Absent
    case Undecodable

    /** The decoded document, if there is one — for a watcher that treats "none" and "could
     *  not decode" alike (skip the event). */
    def presentOption: Option[T] = this match {
      case Present(value) => Some(value)
      case _              => None
    }
  }

  private[movies] val LogIntervalNanos: Long = 60L * 1000 * 1000 * 1000

  /** The decoder for `T` out of the registry the collection itself decodes with. */
  def of[T: ClassTag](collection: String, registry: CodecRegistry, failures: DecodeFailureMetrics): ChangeEventDecoder[T] =
    new ChangeEventDecoder(collection, registry.get(implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]), failures)

  private def idOf(raw: BsonDocument): String =
    Option(raw.get("_id")).map(v => if (v.isString) v.asString.getValue else v.toString).getOrElse("<none>")
}
