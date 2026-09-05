package services

import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Updates}
import play.api.Logging
import tools.{HttpFetch, HttpStatusException}

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
  // No default. Two chains once shared this collection with different TTLs (Helios 2h,
  // Cinema City 6h), so the second `createIndex` was rejected for redefining
  // `fetchedAt_1` and one chain silently ran on the other's expiry — logged as a warning
  // and otherwise invisible. A cache keyed by a TTL has to be named by whoever owns that
  // TTL; there is no sensible shared default. ONE OWNER PER COLLECTION IS STILL THE RULE:
  // `MongoTtlIndex.reconcile` below now applies whatever expiry it is handed, so two
  // owners sharing a collection would take turns rewriting the index instead of one
  // silently losing — visible rather than invisible, but no more correct.
  collectionName: String
) extends HttpFetch with Logging {

  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection(collectionName))

  // TTL index reconciled in a daemon thread so construction never blocks on Mongo.
  //
  // `MongoTtlIndex.reconcile` RATHER THAN A BARE `createIndex`, because a bare one cannot
  // change an expiry that already exists — it is rejected with `IndexOptionsConflict` and
  // the collection keeps reaping on the OLD ttl. So every past change to one of these
  // durations silently did not take on any collection that had already been indexed: the
  // constructor argument said 2h and Mongo went on expiring at 6h, with one warning line
  // to show for it. See that helper's comment for the same defect in two other places.
  coll.foreach { c =>
    val thread = new Thread(() => {
      db.foreach(MongoTtlIndex.reconcile(_, c, "fetchedAt", ttl.toSeconds, s"Detail-cache $collectionName"))
    }, "detail-cache-init")
    thread.setDaemon(true)
    thread.start()
  }

  override def get(url: String): String = coll match {
    case None => underlying.get(url)
    case Some(c) => cached(c, url) match {
      case Some(Right(body)) => body
      case Some(Left(code))  => throw new HttpStatusException(code, "GET", url, None)
      case None =>
        try {
          val body = underlying.get(url)
          store(c, url, Right(body))
          body
        } catch {
          // A 404/410 describes the URL, not the moment, so every server asking again
          // next pass gets the same answer — and the film whose detail this is stays
          // gated on a fetch that will not start succeeding. Remembering it here (rather
          // than only in-process) is the point of this class: one server learning a page
          // is gone spares the whole fleet. Everything else stays uncached and retries.
          case failure: HttpStatusException if HttpStatusException.isDurable(failure.code) =>
            store(c, url, Left(failure.code))
            throw failure
        }
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
  /** `Right(body)` for a cached success, `Left(status)` for a remembered permanent
   *  failure, `None` for a URL this cache has nothing to say about. A document with
   *  neither field reads as `None` — the safe direction, since it just re-fetches. */
  private def cached(c: MongoCollection[Document], url: String): Option[Either[Int, String]] =
    Try(Await.result(c.find(Filters.eq("_id", url)).headOption(), 10.seconds))
      .toOption.flatten
      .flatMap { document =>
        Option(document.getString("body")).map(Right(_))
          .orElse(Option(document.getInteger("goneStatus")).map(status => Left(status.intValue())))
      }

  private def store(c: MongoCollection[Document], url: String, outcome: Either[Int, String]): Unit =
    Try {
      val payload = outcome.fold(
        status => Updates.combine(Updates.set("goneStatus", status), Updates.unset("body")),
        body   => Updates.combine(Updates.set("body", body), Updates.unset("goneStatus")))
      c.updateOne(
        Filters.eq("_id", url),
        Updates.combine(
          payload,
          Updates.set("fetchedAt", new java.util.Date(System.currentTimeMillis()))
        ),
        new com.mongodb.client.model.UpdateOptions().upsert(true)
      ).subscribe(
        (_: org.mongodb.scala.result.UpdateResult) => (),
        (exception: Throwable) => logger.debug(s"Detail-cache write failed for $url: ${exception.getMessage}")
      )
    }.recover { case exception => logger.debug(s"Detail-cache write failed for $url: ${exception.getMessage}") }
}
