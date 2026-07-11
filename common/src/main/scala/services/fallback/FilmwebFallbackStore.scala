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
 * the trait in `FilmwebFallbackScraper`, so the fake is a boring `HashMap` and
 * the real impl differs only at the Mongo boundary (CLAUDE.md "share business
 * logic" / "fake is boring").
 */
trait FilmwebFallbackStore {
  def get(cinema: String): Option[FilmwebFallbackState]
  def findAll(): Seq[FilmwebFallbackState]
  def put(state: FilmwebFallbackState): Unit
  def close(): Unit = ()
}

class InMemoryFilmwebFallbackStore extends FilmwebFallbackStore {
  private val map = new ConcurrentHashMap[String, FilmwebFallbackState]()
  def get(cinema: String): Option[FilmwebFallbackState] = Option(map.get(cinema))
  def findAll(): Seq[FilmwebFallbackState] = map.values().asScala.toSeq
  def put(state: FilmwebFallbackState): Unit = { map.put(state.cinema, state); () }
}

/**
 * Mongo-backed store with an in-process mirror (mirrors `MongoFreshnessStore`):
 * the worker is the sole writer, so reads come from the mirror — no Mongo
 * round-trip per scrape tick — while the web process constructs its own instance
 * and hydrates the mirror once at boot to render the status page. History rides
 * along as a string-encoded array (`epochMillis\tevent\treason`), reusing the
 * proven string-list idiom rather than nested-document parsing.
 */
class MongoFilmwebFallbackStore(
  db: Option[MongoDatabase] = None,
  collectionName: String = MongoFilmwebFallbackStore.CollectionName
) extends FilmwebFallbackStore with Logging {
  import MongoFilmwebFallbackStore._

  private val mirror = new ConcurrentHashMap[String, FilmwebFallbackState]()
  private val coll: Option[MongoCollection[Document]] = db.map(_.getCollection(collectionName))

  coll.foreach(hydrate)

  def get(cinema: String): Option[FilmwebFallbackState] = Option(mirror.get(cinema))
  def findAll(): Seq[FilmwebFallbackState] = mirror.values().asScala.toSeq

  /** Writes are SYNCHRONOUS (unlike the hot-path freshness store): a cinema
   *  enters/leaves fallback at most a few times a day, so the round-trip cost is
   *  irrelevant, and a deterministic write keeps the state machine + status page
   *  honest. Try-guarded so a Mongo hiccup can never break the scrape tick. */
  def put(state: FilmwebFallbackState): Unit = {
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

  private def hydrate(c: MongoCollection[Document]): Unit = Try {
    val documents = Await.result(c.find().toFuture(), 10.seconds)
    var count = 0
    documents.foreach(document => fromDocument(document).foreach { s => mirror.put(s.cinema, s); count += 1 })
    if (count > 0) logger.info(s"Hydrated $count Filmweb-fallback state(s) from Mongo.")
  }.recover { case exception => logger.warn(s"Filmweb-fallback hydrate failed: ${exception.getMessage}") }
}

object MongoFilmwebFallbackStore {
  val CollectionName = "filmwebFallback"

  private val Sep = "\t"

  private def eventToString(e: FallbackEvent): String =
    s"${e.at.toEpochMilli}$Sep${e.event}$Sep${e.reason}"

  private def eventFromString(s: String): Option[FallbackEvent] =
    s.split(Sep, 3) match {
      case Array(ms, event, reason) => Try(ms.toLong).toOption.map(m => FallbackEvent(Instant.ofEpochMilli(m), event, reason))
      case _                        => None
    }

  private def date(i: Instant): java.util.Date = new java.util.Date(i.toEpochMilli)

  private[fallback] def toUpdate(s: FilmwebFallbackState): Bson = Updates.combine(
    Updates.set("active", s.active),
    Updates.set("filmwebCinemaId", s.filmwebCinemaId.map(Int.box).orNull),
    Updates.set("failingSince", s.failingSince.map(date).orNull),
    Updates.set("since", s.since.map(date).orNull),
    Updates.set("lastReason", s.lastReason.orNull),
    Updates.set("consecutiveFailures", s.consecutiveFailures),
    Updates.set("lastPrimaryProbeAt", s.lastPrimaryProbeAt.map(date).orNull),
    Updates.set("nextPrimaryProbeAt", s.nextPrimaryProbeAt.map(date).orNull),
    Updates.set("updatedAt", date(s.updatedAt)),
    Updates.set("history", s.history.map(eventToString).asJava),
    Updates.set("alerted", s.alerted)
  )

  private[fallback] def fromDocument(document: Document): Option[FilmwebFallbackState] =
    Option(document.getString("_id")).map { id =>
      def instant(key: String): Option[Instant] = Option(document.getDate(key)).map(d => Instant.ofEpochMilli(d.getTime))
      FilmwebFallbackState(
        cinema              = id,
        active              = Try(document.getBoolean("active", false)).getOrElse(false),
        filmwebCinemaId     = document.get("filmwebCinemaId").filter(_.isNumber).map(_.asNumber().intValue()),
        failingSince        = instant("failingSince"),
        since               = instant("since"),
        lastReason          = Option(document.getString("lastReason")),
        consecutiveFailures = Try(document.getInteger("consecutiveFailures", 0)).getOrElse(0),
        lastPrimaryProbeAt  = instant("lastPrimaryProbeAt"),
        nextPrimaryProbeAt  = instant("nextPrimaryProbeAt"),
        updatedAt           = instant("updatedAt").getOrElse(Instant.EPOCH),
        history             = Try(document.getList("history", classOf[String])).toOption.flatMap(Option(_))
                                .fold(List.empty[FallbackEvent])(_.asScala.toList.flatMap(eventFromString)),
        alerted             = Try(document.getBoolean("alerted", false)).getOrElse(false)
      )
    }
}
