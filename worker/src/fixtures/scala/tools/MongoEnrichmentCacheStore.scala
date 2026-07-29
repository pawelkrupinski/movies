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

  override def loadAll(): Map[String, CachedResponse] = {
    // Belt-and-braces `fetchedAt` floor on top of the TTL index: Mongo's reaper
    // runs on a ~60s cadence, so an entry can outlive its TTL on disk. Reading it
    // back would resurrect a day-old failure the run was entitled to retry.
    val floor = new java.util.Date(System.currentTimeMillis() - ttl.toMillis)
    Try(Await.result(collection.find(Filters.gte("fetchedAt", floor)).toFuture(), 120.seconds))
      .recover { case failure =>
        logger.warn(s"Enrichment cache preload failed for $collectionName: ${failure.getMessage}")
        Seq.empty
      }
      .get
      .flatMap(document => MongoEnrichmentCacheStore.decode(document).map(document.getString("_id") -> _))
      .toMap
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

  /** How long a remembered answer — success or failure — is allowed to stand in for
   *  the live service. A day: long enough that a morning's iteration on a country
   *  replays entirely from cache, short enough that a genuinely wrong answer (a
   *  rate-limited 429 pinned as a verdict) ages out overnight rather than needing a
   *  manual drop. */
  val Ttl: FiniteDuration = 1.day

  /** Open the shared convergence database on `uri` and return this country's store,
   *  owning the client it opened.
   *
   *  Guarded like every other test-owned database, and with more reason than most:
   *  `.env.local` points `MONGODB_URI` at the PROD tunnel on a dev box, and unlike
   *  the isolated corpus databases this one is never dropped — a mistake here would
   *  leave a `convergence_test` database sitting on the real cluster. */
  def open(uri: String, country: Country, ttl: FiniteDuration = Ttl): MongoEnrichmentCacheStore = {
    IntegrationMongo.requireThrowaway(
      uri, Env.get(IntegrationMongo.OverrideVar).exists(value => value == "1" || value.equalsIgnoreCase("true")))
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
