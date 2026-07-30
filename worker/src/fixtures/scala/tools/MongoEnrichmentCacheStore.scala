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
   *  round-trip, so the local write is noise beside it.
   *
   *  A failure is THROWN rather than swallowed here. Swallowing made this store the
   *  only thing that knew the cluster was unreachable, and it forgot immediately —
   *  so a run pointed at a dead tunnel paid the driver's full server-selection
   *  timeout on every one of ~780 misses and was cancelled at the CI ceiling. The
   *  policy for a store that keeps refusing belongs above the storage seam, in
   *  [[EnrichmentCache.writeThrough]], which is where both stores share it; this one
   *  only has to say that it failed. */
  override def put(key: String, response: CachedResponse): Unit =
    Await.result(
      collection.updateOne(
        Filters.eq("_id", key),
        Updates.combine((MongoEnrichmentCacheStore.encode(response) :+
          Updates.set("fetchedAt", new java.util.Date(System.currentTimeMillis())))*),
        new UpdateOptions().upsert(true)
      ).toFuture(), 30.seconds)

  /** Close the client this store opened, if it opened one. Dropping nothing — the
   *  cache is the artefact that outlives the run. */
  override def close(): Unit = owned.foreach(_.close())

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
   * Entries per preload page.
   *
   * 30 was sized for UNCOMPRESSED bodies at 25-45 KB apiece, to keep a page near
   * 1 MB. Gzipping the bodies made that stale on the same day and nobody revisited
   * it: an entry is now ~9 KB, so 30 rows is a ~250 KB page and a country becomes
   * ~290 round-trips across a proxied connection — which is why a CI leg sat in
   * `preloadEnrichmentCache` for minutes with nothing wrong.
   *
   * 300 restores roughly the same ~2.5 MB page the original reasoning wanted, at a
   * tenth of the round-trips. Sized by bytes, as the archive read had to learn too.
   */
  val PreloadBatchSize = 300

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
    // Tuned for the tunnel: the cache lives across a flyctl proxy that dies and
    // restarts, and the driver must fail fast into the retry rather than block on a
    // dead pool. See TunnelTunedUri.
    val client = MongoClient(TunnelTunedUri(uri))
    new MongoEnrichmentCacheStore(client.getDatabase(DatabaseName), country, ttl, owned = Some(client))
  }

  /**
   * Bodies are stored GZIPPED, which is what makes the preload possible at all.
   *
   * Poland's cache is 6,450 bodies totalling 361 MB — 57 KB on average, and a
   * Metacritic page is 750 KB of HTML. Dragging that across a `flyctl proxy` is
   * what left the preload grinding for ten minutes at 0% CPU after the paging and
   * tunnel faults were fixed: nothing was broken by then, there was simply far too
   * much of it. HTML and JSON are exactly the shapes that compress ~10-20x, so the
   * same cache crosses the wire as tens of megabytes instead of hundreds.
   *
   * Legacy uncompressed rows stay readable (see [[decode]]) and disappear on their
   * own as the 1-day TTL turns the collection over.
   */
  private[tools] def encode(response: CachedResponse): Seq[org.bson.conversions.Bson] = response match {
    case CachedResponse.Body(text) =>
      Seq(Updates.set("kind", "body"), Updates.set("gz", gzip(text)), Updates.unset("text"),
          Updates.unset("status"), Updates.unset("method"), Updates.unset("message"))
    case CachedResponse.Bytes(base64) =>
      Seq(Updates.set("kind", "bytes"), Updates.set("gz", gzip(base64)), Updates.unset("text"),
          Updates.unset("status"), Updates.unset("method"), Updates.unset("message"))
    case CachedResponse.Failed(status, method, message) =>
      Seq(Updates.set("kind", "failed"), Updates.unset("text"),
          status.map(code => Updates.set("status", code)).getOrElse(Updates.unset("status")),
          Updates.set("method", method), Updates.set("message", message))
  }


  /** Compressed first, then the legacy plain field — so entries written before
   *  compression keep working until the TTL retires them. */
  private[tools] def payload(document: Document): Option[String] =
    document.get("gz").flatMap(value => Try(gunzip(value.asBinary().getData)).toOption)
      .orElse(Option(document.getString("text")))

  private[tools] def gzip(text: String): Array[Byte] = {
    val bytes = new java.io.ByteArrayOutputStream()
    val out   = new java.util.zip.GZIPOutputStream(bytes)
    try out.write(text.getBytes(java.nio.charset.StandardCharsets.UTF_8)) finally out.close()
    bytes.toByteArray
  }

  private[tools] def gunzip(data: Array[Byte]): String = {
    val in = new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(data))
    try new String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally in.close()
  }

  private[tools] def decode(document: Document): Option[CachedResponse] =
    Option(document.getString("kind")).flatMap {
      case "body"   => payload(document).map(CachedResponse.Body.apply)
      case "bytes"  => payload(document).map(CachedResponse.Bytes.apply)
      case "failed" => Some(CachedResponse.Failed(
        status  = Try(document.getInteger("status").toInt).toOption,
        method  = Option(document.getString("method")).getOrElse("GET"),
        message = Option(document.getString("message")).getOrElse("cached failure")))
      case _ => None
    }
}
