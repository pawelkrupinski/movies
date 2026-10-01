package services.tasks

import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture}
import org.mongodb.scala.model.{Filters, IndexOptions, Indexes, ReplaceOptions}
import play.api.Logging

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
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
  /** `page` is the page's digest, `parser` the version that parsed it, `slice` the parse, encoded. */
  final case class Entry(page: String, parser: Int, slice: String)

  /** No memo: every page parsed. */
  val none: ChunkPageMemo = new ChunkPageMemo {
    def recall(cinema: String, key: String): Option[Entry] = None
    def remember(cinema: String, key: String, entry: Entry): Unit = ()
  }

  /** A page's digest: SHA-256, hex. */
  def digest(page: String): String = tools.Digest.sha256Hex(page)
}

/** The memo in Mongo (`scrape_chunk_pages`), because a worker restarts on every deploy, many times
 *  between two scrapes of a venue: one small document per (cinema, chunk key), gone a week after it
 *  was last written — a day page outlives its date by that at most. */
final class MongoChunkPageMemo(db: Option[MongoDatabase], clock: java.time.Clock = java.time.Clock.systemUTC())
    extends ChunkPageMemo with Logging {
  private val pages: Option[MongoCollection[Document]] = db.map(_.getCollection("scrape_chunk_pages"))

  pages.foreach { c =>
    val t = new Thread(() => Try(Await.result(c.createIndex(Indexes.ascending("at"),
      IndexOptions().expireAfter(7L, TimeUnit.DAYS)).toFuture(), 10.seconds))
      .failed.foreach(e => logger.warn(s"scrape_chunk_pages index creation failed: ${e.getMessage}")), "chunk-page-memo-init")
    t.setDaemon(true); t.start()
  }

  private def id(cinema: String, key: String) = s"$cinema|$key"

  def recall(cinema: String, key: String): Option[ChunkPageMemo.Entry] = pages.flatMap { c =>
    Try(Await.result(c.find(Filters.eq("_id", id(cinema, key))).first().headOption(), 10.seconds)).toOption.flatten.flatMap { d =>
      for { page <- d.get("page").map(_.asString.getValue); parser <- d.get("parser").map(_.asInt32.getValue)
            slice <- d.get("slice").map(_.asString.getValue) } yield ChunkPageMemo.Entry(page, parser, slice)
    }
  }

  def remember(cinema: String, key: String, entry: ChunkPageMemo.Entry): Unit = pages.foreach { c =>
    Try(Await.result(c.replaceOne(Filters.eq("_id", id(cinema, key)),
      Document("_id" -> id(cinema, key), "page" -> entry.page, "parser" -> entry.parser, "slice" -> entry.slice,
        "at" -> new java.util.Date(clock.millis())), ReplaceOptions().upsert(true)).toFuture(), 10.seconds))
      .failed.foreach(e => logger.warn(s"scrape_chunk_pages write for $cinema/$key failed: ${e.getMessage}"))
  }
}
