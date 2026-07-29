package tools

import com.mongodb.client.model.{IndexOptions => JIndexOptions, UpdateOptions}
import models.Country
import org.mongodb.scala.model.{Filters, Indexes, Updates}
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import play.api.Logging

import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * The per-country enrichment cache, in its own database that survives the run.
 *
 * PER COUNTRY because the countries are the thing that differs: the same film has
 * a different TMDB language, a different Filmweb answer and a different "no such
 * slug" verdict per deployment, and the three convergence legs run concurrently in
 * separate JVMs. One collection each keeps a leg's fill from being read as another
 * leg's hit, and lets a single country's cache be dropped without disturbing the
 * others.
 *
 * NOT in the spec's `IsolatedMongoDatabase` — that one is dropped in `finally`,
 * which is right for a corpus and fatal for a cache. This database is created once
 * and left in place; the TTL index is the only thing that removes anything from it.
 */
class MongoEnrichmentCacheStore(
  val database: MongoDatabase,
  country:      Country,
  ttl:          FiniteDuration,
  owned:        Option[MongoClient] = None
) extends EnrichmentCacheStore with Logging {

  val collectionName: String =
    s"${MongoEnrichmentCacheStore.CollectionPrefix}_${country.code.toLowerCase(java.util.Locale.ROOT)}"

  private val collection: MongoCollection[Document] = database.getCollection(collectionName)

  ensureTtlIndex()

  /**
   * The whole cache, read in keyset-paged batches.
   *
   * Paged because this collection is BIG in bytes even when it is small in rows:
   * every entry holds a full HTTP response body, so 13k entries is ~380 MB at
   * ~30 KB apiece. One unbounded `find()` over that never finished inside the
   * 120s ceiling across CI's tunnel — and because a failed preload degrades to
   * "no entries", the cache silently looked EMPTY on every run. The legs then
   * missed on every lookup, swept the live services for an hour, wrote thousands
   * of entries back, and the next run repeated it: the collection grew steadily
   * while never once being read. Exactly the shape
   * [[services.movies.KeysetScan]] exists for, and the same one that bit
   * `MongoScrapeArchiveRepository.findAll`.
   *
   * Smaller pages than the archive's: at ~30 KB a document these are megabytes
   * apiece, so the batch size is chosen by BYTES-per-page rather than rows.
   *
   * Still degrades to what it managed to read — a short preload costs live fills,
   * never correctness — but now says so, and no longer silently returns nothing.
   */
  override def loadAll(): Map[String, CachedResponse] = {
    // Belt-and-braces `fetchedAt` floor on top of the TTL index: Mongo's reaper
    // runs on a ~60s cadence, so an entry can outlive its TTL on disk. Reading it
    // back would resurrect a day-old failure the run was entitled to retry.
    val floor     = new java.util.Date(System.currentTimeMillis() - ttl.toMillis)
    val collected = Map.newBuilder[String, CachedResponse]
    val complete  = services.movies.KeysetScan.scan[Document](
      label          = s"$collectionName preload batch",
      batchSize      = MongoEnrichmentCacheStore.PreloadBatchSize,
      // Same reasoning as the archive read: survive a tunnel restart rather than
      // degrade to a partial preload, which costs a full live enrichment sweep.
      maxAttempts    = 5,
      initialBackoff = 2.seconds,
      keyOf          = _.getString("_id"),
      fetchPage      = (afterId, limit) => {
        val fresh  = Filters.gte("fetchedAt", floor)
        val filter = afterId.fold(fresh)(id => Filters.and(fresh, Filters.gt("_id", id)))
        Await.result(
          collection.find(filter).sort(org.mongodb.scala.model.Sorts.ascending("_id")).limit(limit).toFuture(),
          120.seconds)
      },
      onIncomplete   = failure =>
        logger.warn(s"Enrichment cache preload incomplete for $collectionName: " +
          s"${failure.getClass.getSimpleName}: ${failure.getMessage} — the run will re-fetch what is missing")
    )(batch => collected ++= batch.flatMap(document =>
        MongoEnrichmentCacheStore.decode(document).map(document.getString("_id") -> _)))

    val entries = collected.result()
    if (!complete) logger.warn(s"Enrichment cache preload for $collectionName stopped early with ${entries.size} entries")
    entries
  }

  /** Written synchronously, unlike `MongoCachingDetailFetch`'s fire-and-forget: the
   *  only writer is a run that is about to close its client, and a dropped tail
   *  would silently re-fetch on the next run. A miss has just paid for a network
   *  round-trip, so the local write is noise beside it. */
  override def put(key: String, response: CachedResponse): Unit =
    Try(Await.result(
      collection.updateOne(
        Filters.eq("_id", key),
        Updates.combine((MongoEnrichmentCacheStore.encode(response) :+
          Updates.set("fetchedAt", new java.util.Date(System.currentTimeMillis())))*),
        new UpdateOptions().upsert(true)
      ).toFuture(), 30.seconds))
      .recover { case failure => logger.warn(s"Enrichment cache write failed for $key: ${failure.getMessage}") }
      .fold(_ => (), _ => ())

  /** Close the client this store opened, if it opened one. Dropping nothing — the
   *  cache is the artefact that outlives the run. */
  def close(): Unit = owned.foreach(_.close())

  /** `createIndex` can never ALTER an existing TTL, so a changed [[ttl]] is pushed
   *  through with `collMod` — otherwise the first run's value is pinned forever and
   *  a later change to the constant would silently do nothing. Same two-step as
   *  `ResolutionStore.ensureTtlIndex`. */
  private def ensureTtlIndex(): Unit = {
    val seconds = ttl.toSeconds
    Try {
      Await.result(
        collection.createIndex(Indexes.ascending("fetchedAt"),
          new JIndexOptions().expireAfter(seconds, TimeUnit.SECONDS)).toFuture(),
        30.seconds)
    }.recover { case failure =>
      logger.debug(s"$collectionName TTL index not (re)created — collMod will reconcile: ${failure.getMessage}")
    }.fold(_ => (), _ => ())

    Try {
      val collMod = new org.bson.Document("collMod", collectionName)
        .append("index", new org.bson.Document("keyPattern", new org.bson.Document("fetchedAt", 1))
          .append("expireAfterSeconds", seconds))
      Await.result(database.runCommand(collMod).toFuture(), 30.seconds)
    }.recover { case failure =>
      logger.debug(s"$collectionName TTL collMod skipped: ${failure.getMessage}")
    }.fold(_ => (), _ => ())
  }
}

object MongoEnrichmentCacheStore {

  /** The database the per-country caches share. Fixed and never dropped — a 1-day
   *  TTL only means anything if the database outlives the run that filled it. Sits
   *  beside the throwaway `kinowo_isolated_*` databases on the same local Mongo,
   *  and is deliberately NOT one of them. */
  val DatabaseName = "convergence_test"

  val CollectionPrefix = "enrichment_cache"

  /**
   * Entries per preload page — sized by BYTES, not rows: an entry is a whole HTTP
   * response body (~25–45 KB average, HTML rating pages being the fat ones).
   *
   * 30 keeps a page near 1 MB. The first attempt at 100 (~3 MB) was reasoned about
   * as "small enough for a proxied connection", which turned out to be the same
   * guess that failed for the archive at 9 MB — the driver's completion chain
   * recurses per SOCKET READ, so a multi-megabyte message across a tunnel is what
   * overflows it, whatever the row count. Sized to the evidence rather than to the
   * intuition this time.
   */
  val PreloadBatchSize = 30

  /** How long a remembered answer — success or failure — is allowed to stand in for
   *  the live service. A day: long enough that a morning's iteration on a country
   *  replays entirely from cache, short enough that a genuinely wrong answer (a
   *  rate-limited 429 pinned as a verdict) ages out overnight rather than needing a
   *  manual drop. */
  val Ttl: FiniteDuration = 1.day

  /**
   * Open the shared convergence database on `uri` and return this country's store,
   * owning the client it opened.
   *
   * Deliberately NOT behind `IntegrationMongo.requireThrowaway`, unlike every other
   * database this suite opens. That guard asks "is this cluster disposable?", and
   * for the cache the answer is meant to be NO: CI starts a fresh Mongo container
   * per leg, so a cache confined to disposable clusters is cold on every run and
   * buys nothing. It is supposed to live somewhere durable and shared.
   *
   * A narrower guarantee replaces it. The database is the [[DatabaseName]] constant
   * rather than a parameter, so this cannot open anything else — pointed at the
   * production cluster it creates and writes `convergence_test` and is structurally
   * unable to reach `kinowo`, `kinowo_uk` or `kinowo_de`. The credential it runs
   * under is scoped the same way (`readWrite` on `convergence_test`, `read` on the
   * country databases), so the rule is enforced on both sides instead of trusted on
   * one — and a bug that tried to write a country database would be refused by the
   * server, not just by this comment.
   */
  def open(uri: String, country: Country, ttl: FiniteDuration = Ttl): MongoEnrichmentCacheStore = {
    val client = MongoClient(uri)
    new MongoEnrichmentCacheStore(client.getDatabase(DatabaseName), country, ttl, owned = Some(client))
  }

  private[tools] def encode(response: CachedResponse): Seq[org.bson.conversions.Bson] = response match {
    case CachedResponse.Body(text) =>
      Seq(Updates.set("kind", "body"), Updates.set("text", text),
          Updates.unset("status"), Updates.unset("method"), Updates.unset("message"))
    case CachedResponse.Bytes(base64) =>
      Seq(Updates.set("kind", "bytes"), Updates.set("text", base64),
          Updates.unset("status"), Updates.unset("method"), Updates.unset("message"))
    case CachedResponse.Failed(status, method, message) =>
      Seq(Updates.set("kind", "failed"), Updates.unset("text"),
          status.map(code => Updates.set("status", code)).getOrElse(Updates.unset("status")),
          Updates.set("method", method), Updates.set("message", message))
  }

  private[tools] def decode(document: Document): Option[CachedResponse] =
    Option(document.getString("kind")).flatMap {
      case "body"   => Option(document.getString("text")).map(CachedResponse.Body.apply)
      case "bytes"  => Option(document.getString("text")).map(CachedResponse.Bytes.apply)
      case "failed" => Some(CachedResponse.Failed(
        status  = Try(document.getInteger("status").toInt).toOption,
        method  = Option(document.getString("method")).getOrElse("GET"),
        message = Option(document.getString("message")).getOrElse("cached failure")))
      case _ => None
    }
}
