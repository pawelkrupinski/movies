package services.fallback

import com.mongodb.client.model.UpdateOptions
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.{Filters, Updates}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Persistence for per-cinema Filmweb-fallback state. Pure storage — the
 * transition rules (when to enter, when to re-probe, when to recover) live above
 * the trait in `SourceFallbackScraper`, so the fake is a boring `HashMap` and
 * the real impl differs only at the Mongo boundary (CLAUDE.md "share business
 * logic" / "fake is boring").
 */
trait FallbackStore {
  def get(cinema: String): Option[FallbackState]
  def findAll(): Seq[FallbackState]
  def put(state: FallbackState): Unit
  def close(): Unit = ()
}

class InMemoryFallbackStore extends FallbackStore {
  private val map = new ConcurrentHashMap[String, FallbackState]()
  def get(cinema: String): Option[FallbackState] = Option(map.get(cinema))
  def findAll(): Seq[FallbackState] = map.values().asScala.toSeq
  def put(state: FallbackState): Unit = { map.put(state.cinema, state); () }
}

/**
 * Mongo-backed store with an in-process mirror (mirrors `MongoFreshnessStore`):
 * the worker is the sole writer, so reads come from the mirror — no Mongo
 * round-trip per scrape tick — while the web process constructs its own instance
 * and hydrates the mirror once at boot to render the status page. History rides
 * along as a string-encoded array (`epochMillis\tevent\treason`), reusing the
 * proven string-list idiom rather than nested-document parsing.
 */
class MongoFallbackStore(
  db: Option[MongoDatabase],
  clock: java.time.Clock,
  collectionName: String = MongoFallbackStore.CollectionName
) extends FallbackStore with Logging {
  import MongoFallbackStore._

  private val mirror = new ConcurrentHashMap[String, FallbackState]()
  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection(collectionName))

  // Whether the mirror holds what Mongo does. A hydrate that FAILED left it empty, and an empty
  // mirror read as "no cinema is on fallback": the scraper then rebuilt each state from nothing
  // and `put` wrote it over the stored one — its failure streak, its history and whether it had
  // already paged, gone. Until a hydrate lands, a read throws — and hydrates again first once
  // `HydrateRetry` has passed since the last attempt: retried on EVERY read, each a 10 s blocking
  // read under one lock, a boot asking once per cinema stalled for cinemas × 10 s and every web
  // /metrics scrape waited 10 s for its 500.
  @volatile private var hydrated = coll.isEmpty
  @volatile private var nextHydrateAt = Long.MinValue
  @volatile private var attempts = 0
  coll.foreach(attemptHydrate)

  /** How many hydrates were attempted — for the specs. */
  def hydrateAttempts: Int = attempts

  private def attemptHydrate(c: MongoCollection[Document]): Unit = {
    attempts += 1
    hydrated = hydrate(c)
    nextHydrateAt = clock.millis() + HydrateRetry.toMillis
  }

  private def ensureHydrated(): Unit =
    if (!hydrated) coll.foreach { c =>
      synchronized { if (!hydrated && clock.millis() >= nextHydrateAt) attemptHydrate(c) }
      if (!hydrated) throw new IllegalStateException(s"$collectionName could not be read — fallback state unknown")
    }

  def get(cinema: String): Option[FallbackState] = { ensureHydrated(); Option(mirror.get(cinema)) }
  def findAll(): Seq[FallbackState] = { ensureHydrated(); mirror.values().asScala.toSeq }

  /** Writes are SYNCHRONOUS (unlike the hot-path freshness store): a cinema
   *  enters/leaves fallback at most a few times a day, so the round-trip cost is
   *  irrelevant, and a deterministic write keeps the state machine + status page
   *  honest. Try-guarded so a Mongo hiccup can never break the scrape tick. */
  def put(state: FallbackState): Unit = {
    mirror.put(state.cinema, state)
    coll.foreach { c =>
      Try {
        Await.result(
          c.updateOne(Filters.eq("_id", state.cinema), toUpdate(state), new UpdateOptions().upsert(true)).toFuture(),
          10.seconds
        )
      }.recover { case exception => logger.debug(s"Filmweb-fallback write failed for ${state.cinema}: ${exception.getMessage}") }
    }
  }

  /** Load every stored state into the mirror (a state already there — written since — is kept);
   *  whether the read landed. */
  private def hydrate(c: MongoCollection[Document]): Boolean =
    tools.MongoRead(10.seconds)(c.find().batchSize(tools.MongoReplies.Default).toFuture()) match {
      case tools.ReadOutcome.Answered(documents) =>
        var count = 0
        documents.foreach(document => fromDocument(document).foreach { s => mirror.putIfAbsent(s.cinema, s); count += 1 })
        if (count > 0) logger.info(s"Hydrated $count Filmweb-fallback state(s) from Mongo.")
        true
      case other =>
        logger.warn(s"Filmweb-fallback hydrate ${other.explain} — reads hydrate again until it lands")
        false
    }
}

object MongoFallbackStore {
  val CollectionName = "filmwebFallback"

  /** How long after a failed hydrate the next read tries again; reads meanwhile throw at once. */
  val HydrateRetry: FiniteDuration = 1.minute

  private val Sep = "\t"

  private def eventToString(e: FallbackEvent): String =
    s"${e.at.toEpochMilli}$Sep${e.event}$Sep${e.reason}"

  private def eventFromString(s: String): Option[FallbackEvent] =
    s.split(Sep, 3) match {
      case Array(ms, event, reason) => Try(ms.toLong).toOption.map(m => FallbackEvent(Instant.ofEpochMilli(m), event, reason))
      case _                        => None
    }

  private def date(i: Instant): java.util.Date = new java.util.Date(i.toEpochMilli)

  private[fallback] def toUpdate(s: FallbackState): Bson = Updates.combine(
    Updates.set("active", s.active),
    Updates.set("fallbackSource", s.fallbackSource),
    Updates.set("fallbackRef", s.fallbackRef.orNull),
    Updates.set("failingSince", s.failingSince.map(date).orNull),
    Updates.set("since", s.since.map(date).orNull),
    Updates.set("lastReason", s.lastReason.orNull),
    Updates.set("consecutiveFailures", s.consecutiveFailures),
    Updates.set("lastPrimaryProbeAt", s.lastPrimaryProbeAt.map(date).orNull),
    Updates.set("nextPrimaryProbeAt", s.nextPrimaryProbeAt.map(date).orNull),
    Updates.set("updatedAt", date(s.updatedAt)),
    Updates.set("history", s.history.map(eventToString).asJava),
    Updates.set("alerted", s.alerted),
    Updates.set("failedRuns", s.failedRuns),
    Updates.set("emptyFallbackSince", s.emptyFallback.map(spell => date(spell.since)).orNull),
    Updates.set("emptyFallbackLastSeen", s.emptyFallback.map(spell => date(spell.lastSeen)).orNull)
  )

  private[fallback] def fromDocument(document: Document): Option[FallbackState] =
    Option(document.getString("_id")).map { id =>
      def instant(key: String): Option[Instant] = Option(document.getDate(key)).map(d => Instant.ofEpochMilli(d.getTime))
      FallbackState(
        cinema              = id,
        active              = Try(document.getBoolean("active", false)).getOrElse(false),
        fallbackSource      = Option(document.getString("fallbackSource")).getOrElse(FallbackState.DefaultSource),
        // New generic handle, else the pre-generalisation numeric `filmwebCinemaId`.
        fallbackRef         = Option(document.getString("fallbackRef"))
                                .orElse(document.get("filmwebCinemaId").filter(_.isNumber).map(_.asNumber().intValue().toString)),
        failingSince        = instant("failingSince"),
        since               = instant("since"),
        lastReason          = Option(document.getString("lastReason")),
        consecutiveFailures = Try(document.getInteger("consecutiveFailures", 0)).getOrElse(0),
        lastPrimaryProbeAt  = instant("lastPrimaryProbeAt"),
        nextPrimaryProbeAt  = instant("nextPrimaryProbeAt"),
        updatedAt           = instant("updatedAt").getOrElse(Instant.EPOCH),
        history             = Try(document.getList("history", classOf[String])).toOption.flatMap(Option(_))
                                .fold(List.empty[FallbackEvent])(_.asScala.toList.flatMap(eventFromString)),
        alerted             = Try(document.getBoolean("alerted", false)).getOrElse(false),
        failedRuns          = Try(document.getInteger("failedRuns", 0)).getOrElse(0),
        emptyFallback       = for (since <- instant("emptyFallbackSince"); lastSeen <- instant("emptyFallbackLastSeen"))
                                yield FallbackState.EmptySpell(since, lastSeen)
      )
    }
}
