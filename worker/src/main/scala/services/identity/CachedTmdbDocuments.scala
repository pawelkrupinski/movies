package services.identity

import com.github.benmanes.caffeine.cache.Cache
import org.bson.codecs.{BsonDocumentCodec, EncoderContext}
import org.bson.io.BasicOutputBuffer
import org.bson.{BsonBinaryWriter, BsonDocument, BsonDouble, BsonInt32, BsonString, RawBsonDocument}

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
 *
 * A film's hit ([[filmHits]]) is kept apart, as the few fields a resolve reads of it, bounded by [[MaxHitBytes]]:
 * a resolve asks every film its questions name, and a recorded film's hit is read off its record, so keeping
 * the answer for it held each named film's whole record — 31k of worker-uk's 33k cached film answers, 9.2 MiB
 * of their 12.3, beside the same records the corpus keeps decoded (heap dump 2026-10-07). For that reason, too,
 * an answer read through ([[answersReadThrough]]: the corpus loading records) is not kept.
 */
final class CachedTmdbDocuments(inner: TmdbDocuments & TmdbDocumentRetention, maxBytes: Long = CachedTmdbDocuments.MaxBytes,
                                maxHitBytes: Long = CachedTmdbDocuments.MaxHitBytes)
    extends TmdbDocuments with TmdbDocumentRetention {
  import CachedTmdbDocuments._

  private val held: Cache[Key, Array[Byte]] =
    tools.BoundedCache.ofWeight[Key, Array[Byte]](maxBytes, EntryOverhead)((key, bytes) => 2 * key.id.length + bytes.length)
      .executor((task: Runnable) => task.run())   // eviction on the caller's thread: no pool task per write
      .build[Key, Array[Byte]]()
  // Films' hits by film id, encoded: `Absent` for a film holding none.
  private val hits: Cache[String, Array[Byte]] =
    tools.BoundedCache.ofWeight[String, Array[Byte]](maxHitBytes, EntryOverhead)((id, bytes) => 2 * id.length + bytes.length)
      .executor((task: Runnable) => task.run())
      .build[String, Array[Byte]]()

  /** Writes begun and ended per stripe of ids: a read keeps what it got only when no write of its ids'
   *  stripes was in flight as it began, and none began before it ended. */
  private val writesBegun = new AtomicLongArray(Stripes)
  private val writesEnded = new AtomicLongArray(Stripes)
  private def stripe(key: Key): Int = (key.hashCode & Int.MaxValue) % Stripes

  def get(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = inner.get(kind, ids)

  override def answers(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
    val (found, missed) = heldAnswers(kind, ids)
    found ++ (if (missed.isEmpty) Map.empty else
      unraced(missed.map(Key(kind, _)))(inner.answers(kind, missed))((id, doc) => held.put(Key(kind, id), doc.fold(Absent)(encode))))
  }

  override def answersReadThrough(kind: TmdbKind, ids: Seq[String]): Map[String, BsonDocument] = {
    val (found, missed) = heldAnswers(kind, ids)
    found ++ (if (missed.isEmpty) Map.empty else inner.answers(kind, missed))
  }

  /** A film's hit: kept apart from its answer, and derived from an answer the cache already holds before the backend is asked. */
  override def filmHits(ids: Seq[String]): Map[String, Hit] = {
    val wanted = ids.distinct
    val kept   = hits.getAllPresent(wanted.asJava).asScala
    val (fromAnswers, missed) = heldAnswers(TmdbKind.Film, wanted.filterNot(kept.contains))
    val derived = fromAnswers.flatMap { case (id, d) => TmdbStore.filmHit(id.toInt, d).map(id -> _) }
    val read = if (missed.isEmpty) Map.empty[String, Hit] else
      unraced(missed.map(Key(TmdbKind.Film, _)))(inner.filmHits(missed))((id, hit) => hits.put(id, hit.fold(Absent)(encodeHit)))
    derived ++ read ++ kept.collect { case (id, bytes) if bytes.nonEmpty => id -> decodeHit(id.toInt, bytes) }
  }

  /** What the cache holds of `ids`' answers — decoded, absent ones left out — and the ids it holds nothing of. */
  private def heldAnswers(kind: TmdbKind, ids: Seq[String]): (Map[String, BsonDocument], Seq[String]) = {
    val keys  = ids.distinct.map(Key(kind, _))
    val found = held.getAllPresent(keys.asJava).asScala
    (found.collect { case (k, bytes) if bytes.nonEmpty => k.id -> decode(bytes) }.toMap, keys.filterNot(found.contains).map(_.id))
  }

  /** `read` the ids of `keys`, then `keep` each one's value (or its absence) — only when no write of its stripe was in
   *  flight as the read began, and none began before it ended, so a read that raced a write never keeps what it replaced. */
  private def unraced[V](keys: Seq[Key])(read: => Map[String, V])(keep: (String, Option[V]) => Unit): Map[String, V] = {
    val still = keys.map { k => val ended = writesEnded.get(stripe(k)); k -> Option.when(writesBegun.get(stripe(k)) == ended)(ended) }.toMap
    val got   = read
    keys.foreach(k => if (still(k).contains(writesBegun.get(stripe(k)))) keep(k.id, got.get(k.id)))
    got
  }

  def put(kind: TmdbKind, docs: Seq[(String, BsonDocument)]): Unit = writing(kind, docs.map(_._1))(inner.put(kind, docs))

  def fetchedBefore(kind: TmdbKind, cutoff: Long): Seq[(String, Long)] = inner.fetchedBefore(kind, cutoff)
  def deleteIfStill(kind: TmdbKind, stamped: Seq[(String, Long)]): Int =
    writing(kind, stamped.map(_._1))(inner.deleteIfStill(kind, stamped))

  /** Run `write` of `ids`, marked in flight, then drop them — whether or not it landed: a failed write may have half-landed. */
  private def writing[A](kind: TmdbKind, ids: Seq[String])(write: => A): A = {
    val keys = ids.distinct.map(Key(kind, _))
    keys.foreach(k => writesBegun.incrementAndGet(stripe(k)))
    try write finally {
      keys.foreach(k => writesEnded.incrementAndGet(stripe(k)))
      held.invalidateAll(keys.asJava)
      if (kind == TmdbKind.Film) hits.invalidateAll(ids.asJava)
    }
  }

  /** Whether the cache holds `kind`'s answer document of `id`. */
  private[identity] def holdsAnswer(kind: TmdbKind, id: String): Boolean = held.getIfPresent(Key(kind, id)) != null

  /** The bytes the cache holds, as its bounds weigh them. */
  private[identity] def heldBytes: Long =
    Seq(held, hits).map(_.policy().eviction().get().weightedSize().getAsLong).sum
}

object CachedTmdbDocuments {
  /** The bound: PL's whole store as answers is ~10 MB (16.5k films × ~300 B, 9.2k questions × ~140 B,
   *  plus keys and entries); US's a few times that at most. */
  val MaxBytes: Long = 32L * 1024 * 1024
  /** The hits' bound: ~45 bytes of fields per film (worker-uk names 36.7k films), plus each entry's own heap. */
  val MaxHitBytes: Long = 16L * 1024 * 1024
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

  /** A hit's fields exactly as they are — popularity unbucketed, as the record gives it. */
  private def encodeHit(hit: Hit): Array[Byte] = {
    val d = new BsonDocument("t", new BsonString(hit.title)).append("p", new BsonDouble(hit.popularity))
    hit.originalTitle.foreach(o => d.append("o", new BsonString(o)))
    hit.year.foreach(y => d.append("y", new BsonInt32(y)))
    encode(d)
  }
  private def decodeHit(id: Int, bytes: Array[Byte]): Hit = {
    val d = new RawBsonDocument(bytes)
    Hit(id, d.getString("t").getValue, Option(d.get("o")).map(_.asString.getValue), Option(d.get("y")).map(_.asInt32.getValue),
      d.getDouble("p").getValue)
  }
}
