package services.venuepages

import org.mongodb.scala.{Document, MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonString}
import org.mongodb.scala.model.{Filters, ReplaceOptions, Sorts}
import play.api.Logging
import services.cinemas.common.FilmDetail

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/** One venue detail page: the enricher group that reads it and the page it names. */
final case class VenuePageKey(detailGroup: String, page: String) {
  def id: String = s"$detailGroup|$page"
}

/** What reading a venue page last said, and when: its detail, or that it is gone. */
final case class VenuePage(key: VenuePageKey, outcome: VenuePage.Outcome, readAt: Instant)

object VenuePage {
  sealed trait Outcome
  /** The page was read: everything it states about the film. */
  final case class Read(detail: FilmDetail) extends Outcome
  /** The page is gone (404/410): asking again buys the same answer. */
  final case class Gone(code: Int) extends Outcome
}

/**
 * Every venue detail page read, by page (`venue_pages`): the ONE place a page's facts are written.
 * Both identity paths read it — the old one derives its film slots from it, the identity model
 * reads it as a listing's venue detail — and only [[VenuePageReader]] writes it. Storage only: when
 * a page is due, how it is fetched and what a read announces are the reader's.
 */
trait VenuePageStore {
  def get(key: VenuePageKey): Option[VenuePage]
  /** Store `page`, replacing what it held; whether the store has it now. */
  def put(page: VenuePage): Boolean
  /** Every page, read through; whether the read reached every page (a failed read stops it short). */
  def foreach(onPage: VenuePage => Unit): Boolean
}

/** In memory, for tests and Mongo-less wiring. */
final class InMemoryVenuePageStore extends VenuePageStore {
  private val pages = new ConcurrentHashMap[String, VenuePage]()
  def get(key: VenuePageKey): Option[VenuePage] = Option(pages.get(key.id))
  def put(page: VenuePage): Boolean = { pages.put(page.key.id, page); true }
  def foreach(onPage: VenuePage => Unit): Boolean = { pages.values.asScala.toSeq.sortBy(_.key.id).foreach(onPage); true }
}

/** `venue_pages`: `{_id: "<group>|<page>", group, page, readAt, gone?, <FilmDetail fields>}`. */
final class MongoVenuePageStore(database: MongoDatabase) extends VenuePageStore with Logging {
  private val collection: MongoCollection[Document] = database.getCollection(MongoVenuePageStore.Collection)

  def get(key: VenuePageKey): Option[VenuePage] =
    Await.result(collection.find(Filters.eq("_id", key.id)).headOption(), 10.seconds).flatMap(MongoVenuePageStore.pageOf)

  def put(page: VenuePage): Boolean =
    Try(Await.result(collection.replaceOne(Filters.eq("_id", page.key.id), MongoVenuePageStore.documentOf(page),
      ReplaceOptions().upsert(true)).toFuture(), 10.seconds)).fold(
      exception => { logger.warn(s"venue_pages write failed for ${page.key.id}: ${exception.getMessage}"); false },
      _.wasAcknowledged())

  def foreach(onPage: VenuePage => Unit): Boolean =
    services.movies.KeysetScan.scan[Document](
      label          = "MongoVenuePageStore.foreach",
      batchSize      = 2000,
      maxAttempts    = 3,
      initialBackoff = 500.millis,
      keyOf          = _.getString("_id"),
      fetchPage      = (afterId, limit) => {
        val find = afterId.fold(collection.find())(after => collection.find(Filters.gt("_id", after)))
        Await.result(find.sort(Sorts.ascending("_id")).limit(limit).toFuture(), 30.seconds)
      },
      onIncomplete   = exception => logger.warn(s"venue_pages scan incomplete: ${exception.getMessage}")
    )(_.flatMap(MongoVenuePageStore.pageOf).foreach(onPage))
}

object MongoVenuePageStore {
  val Collection = "venue_pages"

  private[venuepages] def documentOf(page: VenuePage): Document = {
    val base = Document("_id" -> page.key.id, "group" -> page.key.detailGroup, "page" -> page.key.page,
      "readAt" -> new java.util.Date(page.readAt.toEpochMilli))
    page.outcome match {
      case VenuePage.Gone(code)     => base ++ Document("gone" -> code)
      case VenuePage.Read(detail)   => base ++ detailDocument(detail)
    }
  }

  private def detailDocument(d: FilmDetail): Document = {
    def strings(values: Seq[String]) = BsonArray.fromIterable(values.map(BsonString(_)))
    val fields = Seq(
      d.synopsis.map("synopsis" -> BsonString(_)),
      Option.when(d.cast.nonEmpty)("cast" -> strings(d.cast)),
      Option.when(d.director.nonEmpty)("director" -> strings(d.director)),
      d.runtimeMinutes.map("runtimeMinutes" -> BsonInt32(_)),
      d.releaseYear.map("releaseYear" -> BsonInt32(_)),
      d.originalTitle.map("originalTitle" -> BsonString(_)),
      Option.when(d.countries.nonEmpty)("countries" -> strings(d.countries)),
      Option.when(d.genres.nonEmpty)("genres" -> strings(d.genres)),
      d.posterUrl.map("posterUrl" -> BsonString(_)),
      d.trailerUrl.map("trailerUrl" -> BsonString(_)),
      d.ageRating.map("ageRating" -> BsonString(_)),
      Option.when(d.format.nonEmpty)("format" -> strings(d.format))
    ).flatten
    Document(BsonDocument(fields))
  }

  private[venuepages] def pageOf(d: Document): Option[VenuePage] = for {
    group  <- d.get("group").map(_.asString.getValue)
    page   <- d.get("page").map(_.asString.getValue)
    readAt <- d.get("readAt").map(v => Instant.ofEpochMilli(v.asDateTime.getValue))
  } yield {
    def string(k: String)  = d.get(k).map(_.asString.getValue)
    def int(k: String)     = d.get(k).map(_.asInt32.getValue)
    def strings(k: String) = d.get(k).map(_.asArray.getValues.asScala.map(_.asString.getValue).toSeq).getOrElse(Nil)
    val outcome = int("gone").fold[VenuePage.Outcome](VenuePage.Read(FilmDetail(
      synopsis = string("synopsis"), cast = strings("cast"), director = strings("director"),
      runtimeMinutes = int("runtimeMinutes"), releaseYear = int("releaseYear"), originalTitle = string("originalTitle"),
      countries = strings("countries"), genres = strings("genres"), posterUrl = string("posterUrl"),
      trailerUrl = string("trailerUrl"), ageRating = string("ageRating"), format = strings("format").toList)))(VenuePage.Gone(_))
    VenuePage(VenuePageKey(group, page), outcome, readAt)
  }
}
