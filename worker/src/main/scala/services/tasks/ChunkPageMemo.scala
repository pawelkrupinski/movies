package services.tasks

import org.mongodb.scala.{Document, MongoDatabase}
import play.api.Logging
import services.keyed.{DocCodec, InMemoryKeyedDocStore, KeyedDocStore, MongoKeyedDocStore}

import java.time.Clock
import scala.concurrent.duration.*
import scala.util.Try

/** A chunk's last page and what it parsed to, by (cinema, chunk key): what lets a page identical to
 *  the last one skip its parse (`ScrapeChunkHandler`). A memo that cannot answer answers None, and the
 *  page is parsed — it is a saving, never an input. */
trait ChunkPageMemo {
  def recall(cinema: String, key: String): Option[ChunkPageMemo.Entry]
  def remember(cinema: String, key: String, entry: ChunkPageMemo.Entry): Unit
}

object ChunkPageMemo {
  /** `page` is the page's fingerprint ([[digest]]), `parser` what parsed it (`PagedChunkScraper.pageParser`), `slice` the parse, encoded. */
  final case class Entry(page: String, parser: String, slice: String)

  /** No memo: every page parsed. */
  val none: ChunkPageMemo = new ChunkPageMemo {
    def recall(cinema: String, key: String): Option[Entry] = None
    def remember(cinema: String, key: String, entry: Entry): Unit = ()
  }

  /** A page's fingerprint: its length and `String.hashCode`, which the JVM computes vectorized. SHA-256
   *  over the page's UTF-8 bytes cost about as much as the parse it spared (2.4% + 2.0% of the US
   *  worker's CPU, JFR 2026-10-01). A collision — one in 2^32 for a changed page of the same length —
   *  reuses the old parse until the page changes again. */
  def digest(page: String): String = s"${page.length}:${page.hashCode}"
}

/** The memo over its rows, one small document per (cinema, chunk key): Mongo and in-memory differ only
 *  in the rows. A read that fails answers None and a write that fails is logged — a saving, never an input. */
class KeyedChunkPageMemo(rows: KeyedDocStore[ChunkPageMemo.Entry]) extends ChunkPageMemo with Logging {
  private def id(cinema: String, key: String) = s"$cinema|$key"

  def recall(cinema: String, key: String): Option[ChunkPageMemo.Entry] = Try(rows.get(id(cinema, key))).toOption.flatten

  def remember(cinema: String, key: String, entry: ChunkPageMemo.Entry): Unit =
    Try(rows.put(id(cinema, key), entry)).failed.foreach(e => logger.warn(s"scrape_chunk_pages write for $cinema/$key failed: ${e.getMessage}"))
}

object KeyedChunkPageMemo {
  /** `{_id: cinema|key, page, parser, slice, at}`, `at` when it was written by `clock`. */
  def codec(clock: Clock): DocCodec[ChunkPageMemo.Entry] = new DocCodec[ChunkPageMemo.Entry] {
    def encode(entry: ChunkPageMemo.Entry): Document =
      Document("page" -> entry.page, "parser" -> entry.parser, "slice" -> entry.slice, "at" -> new java.util.Date(clock.millis()))
    def decode(d: Document): Option[ChunkPageMemo.Entry] =
      for { page <- d.get("page").map(_.asString.getValue); parser <- d.get("parser").filter(_.isString).map(_.asString.getValue)
            slice <- d.get("slice").map(_.asString.getValue) } yield ChunkPageMemo.Entry(page, parser, slice)
    override def stampField: Option[String] = Some("at")
  }
}

/** The memo in a map: tests, and a worker without Mongo. */
final class InMemoryChunkPageMemo(clock: Clock) extends KeyedChunkPageMemo(new InMemoryKeyedDocStore(KeyedChunkPageMemo.codec(clock)))

/** The memo in Mongo (`scrape_chunk_pages`), because a worker restarts on every deploy, many times
 *  between two scrapes of a venue: each document gone a week after it was last written — a day page
 *  outlives its date by that at most. */
final class MongoChunkPageMemo(db: Option[MongoDatabase], clock: Clock) extends KeyedChunkPageMemo(
  db.fold(KeyedDocStore.absent[ChunkPageMemo.Entry])(d =>
    new MongoKeyedDocStore(d.getCollection(MongoChunkPageMemo.Collection), KeyedChunkPageMemo.codec(clock), expireAfter = Some(7.days))))

object MongoChunkPageMemo {
  val Collection = "scrape_chunk_pages"
}
