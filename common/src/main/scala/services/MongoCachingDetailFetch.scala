package services

import com.mongodb.client.model.{IndexOptions => JIndexOptions}
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Indexes, Updates}
import play.api.Logging
import tools.HttpFetch

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * [[HttpFetch]] decorator that caches successful GET bodies in a Mongo
 * collection (keyed by URL, expired by a TTL index) — the cross-server analogue
 * of [[tools.CachingDetailFetch]]. Several worker servers share one cache, so a
 * cinema chain's per-film detail page is fetched once per TTL across the WHOLE
 * fleet rather than once per process. Used for the national chains' detail fetch
 * (Helios `/api/movie/{id}`, Cinema City film pages), whose detail is identical
 * across the chain's many locations.
 *
 * Only successful responses are cached (`get` throws on failure exactly like the
 * underlying fetch, so a transient blip isn't pinned for the TTL). A `null` db
 * disables caching — every call passes straight through. On a cold cache several
 * servers may each fetch+store the same URL once (last write wins); steady state
 * is one fetch per URL per TTL fleet-wide.
 */
class MongoCachingDetailFetch(
  underlying:     HttpFetch,
  db:             Option[MongoDatabase],
  ttl:            FiniteDuration,
  collectionName: String = "detailCache"
) extends HttpFetch with Logging {

  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection(collectionName))

  // TTL index built in a daemon thread so construction never blocks on Mongo.
  coll.foreach { c =>
    val thread = new Thread(() => {
      Try(
        Await.result(
          c.createIndex(Indexes.ascending("fetchedAt"),
            new JIndexOptions().expireAfter(ttl.toSeconds, TimeUnit.SECONDS)).toFuture(),
          10.seconds)
      ).recover { case exception => logger.warn(s"Detail-cache index creation failed: ${exception.getMessage}") }
      ()
    }, "detail-cache-init")
    thread.setDaemon(true)
    thread.start()
  }

  override def get(url: String): String = coll match {
    case None => underlying.get(url)
    case Some(c) =>
      cached(c, url).getOrElse {
        val body = underlying.get(url) // throws on failure → falls through uncached
        store(c, url, body)
        body
      }
  }

  // Detail fetches don't vary by request header; key on the URL alone.
  override def get(url: String, headers: Map[String, String]): String = get(url)

  // Raw bytes pass straight through to the underlying fetch (uncached): the
  // Mongo cache holds UTF-8 bodies, and re-encoding one would mojibake a legacy
  // single-byte page. Don't inherit the lossy base default.
  override def getBytes(url: String): Array[Byte] = underlying.getBytes(url)

  override def post(url: String, body: String, contentType: String): String =
    underlying.post(url, body, contentType)

  /** A document only survives in the collection while within the TTL (the TTL index
   *  reaps it after `fetchedAt + ttl`, modulo Mongo's ~60s reaper cadence), so
   *  any document found is fresh enough to reuse. */
  private def cached(c: MongoCollection[Document], url: String): Option[String] =
    Try(Await.result(c.find(Filters.eq("_id", url)).headOption(), 10.seconds))
      .toOption.flatten
      .flatMap(document => Option(document.getString("body")))

  private def store(c: MongoCollection[Document], url: String, body: String): Unit =
    Try {
      c.updateOne(
        Filters.eq("_id", url),
        Updates.combine(
          Updates.set("body", body),
          Updates.set("fetchedAt", new java.util.Date(System.currentTimeMillis()))
        ),
        new com.mongodb.client.model.UpdateOptions().upsert(true)
      ).subscribe(
        (_: org.mongodb.scala.result.UpdateResult) => (),
        (exception: Throwable) => logger.debug(s"Detail-cache write failed for $url: ${exception.getMessage}")
      )
    }.recover { case exception => logger.debug(s"Detail-cache write failed for $url: ${exception.getMessage}") }
}
