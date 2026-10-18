package services

import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, documentToUntypedDocument}
import org.mongodb.scala.model.{Filters, Updates}
import play.api.Logging
import tools.{ForwardingHttpFetch, HttpFetch, HttpStatusException}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * [[HttpFetch]] decorator that caches successful GET bodies in the ONE Mongo collection every chain
 * shares (`detail_cache`, keyed `<chain>|<url>`, each document expiring at its own `expireAt`, its body gzipped) — the cross-server analogue
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
  // Whose cache a document is: the key's prefix. Every chain shares the one collection, its own TTL
  // riding on each document (`expireAt`) rather than on a per-collection index — which is what once
  // made two chains sharing a collection silently run on one's expiry (Helios 2h, Cinema City 6h).
  chain:          DetailCacheChain,
  // Where a TTL index this cache could not bring into line is recorded — the process's
  // one set, which the worker's gauge reads.
  ttlMismatches:  TtlIndexMismatches,
  // What `fetchedAt`/`expireAt` are stamped from, and what a cached document's expiry is judged against.
  clock:          java.time.Clock
) extends ForwardingHttpFetch with Logging {

  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection(MongoCachingDetailFetch.Collection))

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
      // One index for every chain, expiring each document AT its `expireAt`: whatever TTL a chain asks
      // for is written on the documents, so no two owners ever disagree about the index.
      MongoTtlIndex.reconcile(c, "expireAt", 0L, "Detail-cache", ttlMismatches)
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

  // Raw bytes and posts pass straight through to the underlying fetch (uncached): the
  // Mongo cache holds UTF-8 bodies, and re-encoding one would mojibake a legacy
  // single-byte page.
  protected def around[T](url: String)(call: HttpFetch => T): T = call(underlying)

  /** `Right(body)` for a cached success, `Left(status)` for a remembered permanent
   *  failure, `None` for a URL this cache has nothing to say about. A document with
   *  neither field reads as `None` — the safe direction, since it just re-fetches. */
  private def cached(c: MongoCollection[Document], url: String): Option[Either[Int, String]] =
    // Not past its own expiry, whatever the TTL reaper (a ~60s sweep) has got round to.
    Try(Await.result(c.find(Filters.and(Filters.eq("_id", idOf(url)), Filters.gt("expireAt", java.util.Date.from(clock.instant())))).headOption(), 10.seconds))
      .toOption.flatten
      .flatMap { document =>
        // The plain fields first: this code's every write leaves exactly one of the three, so a `body` or `goneStatus`
        // beside a `bodyGzip` is the later write of a pod running the code from before compression (2026-10-12) — a
        // rollback — which `$set` its own field and left the gzip. Such documents expire within one TTL of it.
        Option(document.getString("body")).map(Right(_))
          .orElse(Option(document.getInteger("goneStatus")).map(status => Left(status.intValue())))
          .orElse(document.get("bodyGzip").flatMap(stored => Try(MongoCachingDetailFetch.decompress(stored.asBinary().getData)).toOption).map(Right(_)))
      }

  private def store(c: MongoCollection[Document], url: String, outcome: Either[Int, String]): Unit =
    Try {
      val payload = outcome.fold(
        status => Updates.combine(Updates.set("goneStatus", status), Updates.unset("bodyGzip"), Updates.unset("body")),
        body   => Updates.combine(Updates.set("bodyGzip", new org.bson.BsonBinary(MongoCachingDetailFetch.compress(body))),
                                  Updates.unset("body"), Updates.unset("goneStatus")))
      val now = clock.millis()
      c.updateOne(
        Filters.eq("_id", idOf(url)),
        Updates.combine(
          payload,
          Updates.set("fetchedAt", new java.util.Date(now)),
          Updates.set("expireAt", new java.util.Date(now + ttl.toMillis))
        ),
        new com.mongodb.client.model.UpdateOptions().upsert(true)
      ).subscribe(
        (_: org.mongodb.scala.result.UpdateResult) => (),
        (exception: Throwable) => logger.debug(s"Detail-cache write failed for $url: ${exception.getMessage}")
      )
    }.recover { case exception => logger.debug(s"Detail-cache write failed for $url: ${exception.getMessage}") }

  private def idOf(url: String): String = s"${chain.name}|$url"
}

/** A chain whose detail pages [[MongoCachingDetailFetch]] caches: the prefix of its documents' keys. */
final case class DetailCacheChain(name: String)

object MongoCachingDetailFetch {
  /** The one collection every chain's detail cache shares. */
  val Collection = "detail_cache"

  /** A body as stored: gzipped UTF-8. A refresh past a document's `expireAt` re-inserts it whole, so its
   *  size is paid in oplog once per URL per TTL: a Flicks film page is 200-370 KB of HTML
   *  (`kinowo_us.detail_cache`, measured 2026-10-12: ~300 KB per insert, 107 MB/h of oplog) and ~25 KB
   *  gzipped. The whole page rather than the parsed fields because this is an [[HttpFetch]]: a reader
   *  gets back exactly the string the site sent, hit or miss. */
  private[services] def compress(body: String): Array[Byte] = {
    val bytes = new java.io.ByteArrayOutputStream(body.length / 6 + 64)
    val gzip  = new java.util.zip.GZIPOutputStream(bytes)
    try gzip.write(body.getBytes(java.nio.charset.StandardCharsets.UTF_8)) finally gzip.close()
    bytes.toByteArray
  }

  /** Throws on a corrupt payload, which [[MongoCachingDetailFetch]] reads as a miss and re-fetches. */
  private[services] def decompress(stored: Array[Byte]): String = {
    val gzip = new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(stored))
    try new String(gzip.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally gzip.close()
  }
}
