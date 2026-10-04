package services.identity

import com.github.benmanes.caffeine.cache.Cache
import org.bson.codecs.{BsonDocumentCodec, EncoderContext}
import org.bson.io.BasicOutputBuffer
import org.bson.{BsonBinaryWriter, BsonDocument, RawBsonDocument}

import java.util.concurrent.atomic.AtomicLongArray
import scala.jdk.CollectionConverters._

/**
 * `inner`, its ANSWER reads ([[answers]]) kept across projection ticks: each tick's prefetch asked the
 * backend again for every search and film its slices name, though almost none had changed since the
 * last tick (worker-us: 237 `tmdb_films` finds in 90 s, the second-largest steady Mongo outbound).
 * A document read once is answered from here until something writes or deletes it.
 *
 * Sound because every write to these collections goes through this worker's one instance — the
 * store's filings, the gap memory's markers (both under [[CoalescedTmdbDocuments]], over this) and the
 * sweep's deletes (this, as its [[TmdbDocumentRetention]]) — and the worker runs as one replica. A
 * write drops the ids it wrote once it has landed, and a read keeps what it got only when no write of
 * those ids began while it was in flight, so a read that raced a write never keeps the value the
 * write replaced. A document no one holds is kept too (as absent): it is `Unknown` until a write.
 *
 * Kept encoded (the BSON bytes, ~300 per film answer, ~140 per question on PL) and bounded by
 * [[MaxBytes]], least recently used out first; a hit is decoded afresh, so a reader may edit its copy.
 * Whole-document reads ([[get]]: the store's write path) are not kept — each compares with the server.
 */
final class CachedTmdbDocuments(inner: TmdbDocuments & TmdbDocumentRetention, maxBytes: Long = CachedTmdbDocuments.MaxBytes)
    extends TmdbDocuments with TmdbDocumentRetention {
  import CachedTmdbDocuments._

  private val held: Cache[Key, Array[Byte]] =
    tools.BoundedCache.ofWeight[Key, Array[Byte]](maxBytes, EntryOverhead)((key, bytes) => 2 * key.id.length + bytes.length)
      .executor((task: Runnable) => task.run())   // eviction on the caller's thread: no pool task per write
      .build[Key, Array[Byte]]()

  /** Writes begun and ended per stripe of ids: a read keeps what it got only when no write of its ids'
   *  stripes was in flight as it began, and none began before it ended. */
  private val writesBegun = new AtomicLongArray(Stripes)
  private val writesEnded = new AtomicLongArray(Stripes)
  private def stripe(key: Key): Int = (key.hashCode & Int.MaxValue) % Stripes

  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.get(kind, ids)

  override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
    val keys   = ids.distinct.map(Key(kind, _))
    val hits   = held.getAllPresent(keys.asJava).asScala
    val missed = keys.filterNot(hits.contains)
    val read   = if (missed.isEmpty) Map.empty[String, BsonDocument] else {
      val still = missed.map { k => val ended = writesEnded.get(stripe(k)); k -> Option.when(writesBegun.get(stripe(k)) == ended)(ended) }.toMap
      val got   = inner.answers(kind, missed.map(_.id))
      missed.foreach { k =>
        if (still(k).contains(writesBegun.get(stripe(k)))) held.put(k, got.get(k.id).fold(Absent)(encode))
      }
      got
    }
    read ++ hits.collect { case (k, bytes) if bytes.nonEmpty => k.id -> decode(bytes) }
  }

  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = writing(kind, docs.map(_._1))(inner.put(kind, docs))

  def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)] = inner.fetchedBefore(kind, cutoff)
  def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]): Int =
    writing(kind, stamped.map(_._1))(inner.deleteIfStill(kind, stamped))

  /** Run `write` of `ids`, marked in flight, then drop them — whether or not it landed: a failed write may have half-landed. */
  private def writing[A](kind: TmdbKind, ids: Seq[String])(write: => A): A = {
    val keys = ids.distinct.map(Key(kind, _))
    keys.foreach(k => writesBegun.incrementAndGet(stripe(k)))
    try write finally { keys.foreach(k => writesEnded.incrementAndGet(stripe(k))); held.invalidateAll(keys.asJava) }
  }

  /** The bytes the cache holds, as its bound weighs them. */
  private[identity] def heldBytes: Long = held.policy().eviction().get().weightedSize().getAsLong
}

object CachedTmdbDocuments {
  /** The bound: PL's whole store as answers is ~10 MB (16.5k films × ~300 B, 9.2k questions × ~140 B,
   *  plus keys and entries); US's a few times that at most. */
  val MaxBytes: Long = 32L * 1024 * 1024
  /** A cache entry's own heap beyond its key's characters and value's bytes: node, key, array headers. */
  private val EntryOverhead = 96
  private val Stripes       = 4096
  private val Absent        = Array.emptyByteArray
  private val Codec         = new BsonDocumentCodec()

  private final case class Key(kind: TmdbKind, id: String)

  private def encode(d: BsonDocument): Array[Byte] = {
    val out = new BasicOutputBuffer()
    Codec.encode(new BsonBinaryWriter(out), d, EncoderContext.builder().build())
    out.toByteArray
  }
  private def decode(bytes: Array[Byte]): BsonDocument = new RawBsonDocument(bytes).decode(Codec)
}
