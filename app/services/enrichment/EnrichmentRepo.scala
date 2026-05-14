package services.enrichment

import com.mongodb.MongoException
import com.mongodb.client.model.ReplaceOptions
import models.Enrichment
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonDateTime, BsonNull, BsonString}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, MongoCollection}
import play.api.Logging
import tools.Env

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Persistent store for `(title, year) → Enrichment` records. Backed by a
 * single MongoDB collection (`enrichments`).
 *
 * When `MONGODB_URI` is unset the repo silently no-ops — local dev / tests
 * without Atlas connectivity keep working off the in-memory cache only.
 *
 * The driver uses Reactive Streams, but the enrichment pipeline is a single
 * daemon worker so we use the blocking `.toFuture()` form throughout.
 *
 * Lifecycle: caller (`AppLoader`) registers a shutdown hook that calls
 * `close()` — the class doesn't self-register.
 */
class EnrichmentRepo extends Logging {

  private val (clientOpt, coll): (Option[MongoClient], Option[MongoCollection[Document]]) = init()

  /** Whether Mongo is wired up. Hot path uses `coll` directly. */
  def enabled: Boolean = coll.isDefined

  /** Snapshot of every persisted enrichment. Returns empty when disabled. */
  def findAll(): Seq[(String, Option[Int], Enrichment)] = coll match {
    case None => Seq.empty
    case Some(c) =>
      Try {
        val docs = Await.result(c.find().toFuture(), 30.seconds)
        docs.flatMap(decode)
      }.recover {
        case ex: MongoException =>
          logger.warn(s"EnrichmentRepo.findAll failed: ${ex.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
  }

  /** Remove a single (title, year) row. Best-effort — failures are logged. */
  def delete(title: String, year: Option[Int]): Unit = coll.foreach { c =>
    val id = docId(title, year)
    Try {
      Await.result(c.deleteOne(Filters.eq("_id", id)).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable => logger.warn(s"EnrichmentRepo.delete($title, $year) failed: ${ex.getMessage}")
    }
  }

  /** Write-through upsert. Best-effort — failures are logged, never thrown. */
  def upsert(title: String, year: Option[Int], e: Enrichment): Unit = coll.foreach { c =>
    val id   = docId(title, year)
    val doc  = encode(id, title, year, e)
    val opts = new ReplaceOptions().upsert(true)
    Try {
      Await.result(c.replaceOne(Filters.eq("_id", id), doc, opts).toFuture(), 10.seconds)
      ()
    }.recover {
      case ex: Throwable if isClusterClosed(ex) =>
        // Shutdown race — the lifecycle closed the MongoClient while a worker
        // was still mid-write. Harmless: the in-memory cache already has the
        // value and the next refresh will persist it.
        logger.debug(s"EnrichmentRepo.upsert($title, $year) skipped — Mongo client closing.")
      case ex: Throwable =>
        logger.warn(s"EnrichmentRepo.upsert($title, $year) failed: ${ex.getMessage}")
    }
  }

  /** Close the underlying MongoClient. No-op when Mongo isn't configured. */
  def close(): Unit = clientOpt.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoCollection[Document]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — EnrichmentRepo disabled (in-memory cache only).")
        (None, None)
      case Some(uri) =>
        Try {
          val db     = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client = MongoClient(uri)
          val coll   = client.getDatabase(db).getCollection[Document]("enrichments")
          // Touch the collection to surface connectivity errors at startup, not
          // on the first read after the app is "up".
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          logger.info(s"EnrichmentRepo connected to $db.enrichments")
          (client, coll)
        }.recover {
          case ex: Throwable =>
            logger.error(s"EnrichmentRepo init failed (${ex.getMessage}) — falling back to in-memory cache.")
            null
        }.toOption.filter(_ != null) match {
          case Some((c, coll)) => (Some(c), Some(coll))
          case None            => (None, None)
        }
    }

  // The driver throws IllegalStateException("state should be: open") from
  // BaseCluster / DefaultConnectionPool once MongoClient.close() has fired.
  private def isClusterClosed(ex: Throwable): Boolean =
    Option(ex.getMessage).exists(_.contains("state should be: open"))

  // Match the in-memory CacheKey's normalization rules so case-only and
  // diacritic variants of the same title share a single Mongo record. Without
  // this, "Tom i Jerry: Przygoda w muzeum" and "Tom i jerry: przygoda w
  // muzeum" — both reported by different cinemas for the same film — each get
  // their own row, and only one can be updated per hourly refresh tick (the
  // tick walks the deduplicated Caffeine cache).
  private def docId(title: String, year: Option[Int]): String =
    s"${EnrichmentService.normalize(title)}|${year.map(_.toString).getOrElse("")}"

  private def encode(id: String, title: String, year: Option[Int], e: Enrichment): Document =
    Document(
      "_id"          -> BsonString(id),
      "title"        -> BsonString(title),
      "year"         -> year.map(y => org.mongodb.scala.bson.BsonInt32(y)).getOrElse(BsonNull()),
      "imdbId"       -> e.imdbId.map(BsonString(_)).getOrElse(BsonNull()),
      "imdbRating"   -> e.imdbRating.map(org.mongodb.scala.bson.BsonDouble(_)).getOrElse(BsonNull()),
      "metascore"    -> e.metascore.map(org.mongodb.scala.bson.BsonInt32(_)).getOrElse(BsonNull()),
      "originalTitle"-> e.originalTitle.map(BsonString(_)).getOrElse(BsonNull()),
      "filmwebUrl"   -> e.filmwebUrl.map(BsonString(_)).getOrElse(BsonNull()),
      "filmwebRating"-> e.filmwebRating.map(org.mongodb.scala.bson.BsonDouble(_)).getOrElse(BsonNull()),
      "rottenTomatoes"-> e.rottenTomatoes.map(org.mongodb.scala.bson.BsonInt32(_)).getOrElse(BsonNull()),
      "tmdbId"       -> e.tmdbId.map(org.mongodb.scala.bson.BsonInt32(_)).getOrElse(BsonNull()),
      "metacriticUrl"-> e.metacriticUrl.map(BsonString(_)).getOrElse(BsonNull()),
      "rottenTomatoesUrl" -> e.rottenTomatoesUrl.map(BsonString(_)).getOrElse(BsonNull()),
      "updatedAt"    -> BsonDateTime(Instant.now().toEpochMilli)
    )

  private def decode(d: Document): Option[(String, Option[Int], Enrichment)] =
    for {
      // `imdbId` is optional now (TMDB hits without an IMDb cross-reference
      // still produce a row), but the row must at least have a `title` to be
      // usable as a cache key. Anything else missing is treated as None.
      title <- d.get("title").flatMap(v => Try(v.asString().getValue).toOption)
    } yield (
      title,
      d.get("year").flatMap(v => Try(v.asInt32().getValue).toOption),
      Enrichment(
        imdbId        = d.get("imdbId").flatMap(v => Try(v.asString().getValue).toOption),
        imdbRating    = d.get("imdbRating").flatMap(v => Try(v.asDouble().getValue).toOption),
        metascore     = d.get("metascore").flatMap(v => Try(v.asInt32().getValue).toOption),
        originalTitle = d.get("originalTitle").flatMap(v => Try(v.asString().getValue).toOption),
        filmwebUrl     = d.get("filmwebUrl").flatMap(v => Try(v.asString().getValue).toOption),
        filmwebRating  = d.get("filmwebRating").flatMap(v => Try(v.asDouble().getValue).toOption),
        rottenTomatoes = d.get("rottenTomatoes").flatMap(v => Try(v.asInt32().getValue).toOption),
        tmdbId         = d.get("tmdbId").flatMap(v => Try(v.asInt32().getValue).toOption),
        metacriticUrl  = d.get("metacriticUrl").flatMap(v => Try(v.asString().getValue).toOption),
        rottenTomatoesUrl = d.get("rottenTomatoesUrl").flatMap(v => Try(v.asString().getValue).toOption)
      )
    )
}
