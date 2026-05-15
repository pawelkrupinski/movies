package services.movies

import com.mongodb.MongoException
import com.mongodb.client.model.ReplaceOptions
import models.{Cinema, CinemaScrape, CinemaShowings, MovieRecord, Showtime}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson._
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, MongoCollection}
import play.api.Logging
import tools.Env

import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Persistent store for `(title, year) → MovieRecord` records. Backed by the
 * `movies` collection in MongoDB.
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
class MovieRepo extends Logging {

  private val (clientOpt, coll): (Option[MongoClient], Option[MongoCollection[Document]]) = init()

  /** Whether Mongo is wired up. Hot path uses `coll` directly. */
  def enabled: Boolean = coll.isDefined

  /** Snapshot of every persisted record. Returns empty when disabled. */
  def findAll(): Seq[(String, Option[Int], MovieRecord)] = coll match {
    case None => Seq.empty
    case Some(c) =>
      Try {
        val docs = Await.result(c.find().toFuture(), 30.seconds)
        docs.flatMap(decode)
      }.recover {
        case ex: MongoException =>
          logger.warn(s"MovieRepo.findAll failed: ${ex.getMessage}")
          Seq.empty
      }.getOrElse(Seq.empty)
  }

  /** Remove every doc matching the given (title, year). Filters by `title`
   *  + `year` fields rather than by `_id`, so legacy docs whose `_id` was
   *  computed with a prior `docId` formula still get caught — `deleteOne` by
   *  `_id` would silently match nothing and the orphan would survive every
   *  startup's merge. Best-effort: failures are logged, never thrown. */
  def delete(title: String, year: Option[Int]): Unit = coll.foreach { c =>
    val yearFilter = year match {
      case Some(y) => Filters.eq("year", y)
      // year=None in the in-memory model lands as either BsonNull() or a
      // missing field in legacy docs; cover both.
      case None    => Filters.or(Filters.eq("year", BsonNull()), Filters.exists("year", false))
    }
    val filter = Filters.and(Filters.eq("title", title), yearFilter)
    Try {
      val result = Await.result(c.deleteMany(filter).toFuture(), 10.seconds)
      if (result.getDeletedCount > 1)
        logger.info(s"MovieRepo.delete($title, $year) removed ${result.getDeletedCount} doc(s).")
      ()
    }.recover {
      case ex: Throwable => logger.warn(s"MovieRepo.delete($title, $year) failed: ${ex.getMessage}")
    }
  }

  /** Write-through upsert. Best-effort — failures are logged, never thrown. */
  def upsert(title: String, year: Option[Int], e: MovieRecord): Unit = coll.foreach { c =>
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
        logger.debug(s"MovieRepo.upsert($title, $year) skipped — Mongo client closing.")
      case ex: Throwable =>
        logger.warn(s"MovieRepo.upsert($title, $year) failed: ${ex.getMessage}")
    }
  }

  /** Update the row at `(title, year)` only if it currently exists in Mongo.
   *  Returns true on update, false when no doc matched (concurrent delete,
   *  or row never existed). Used by the cache's `putIfPresent` so a rating
   *  write that races against a concurrent `cache.invalidate` can't
   *  resurrect the row by upserting it back into existence. */
  def updateIfPresent(title: String, year: Option[Int], e: MovieRecord): Boolean = coll match {
    case None => false
    case Some(c) =>
      val id   = docId(title, year)
      val doc  = encode(id, title, year, e)
      val opts = new ReplaceOptions().upsert(false)
      Try {
        val result = Await.result(c.replaceOne(Filters.eq("_id", id), doc, opts).toFuture(), 10.seconds)
        result.getMatchedCount > 0
      }.recover {
        case ex: Throwable if isClusterClosed(ex) => false
        case ex: Throwable =>
          logger.warn(s"MovieRepo.updateIfPresent($title, $year) failed: ${ex.getMessage}")
          false
      }.getOrElse(false)
  }

  /** Close the underlying MongoClient. No-op when Mongo isn't configured. */
  def close(): Unit = clientOpt.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoCollection[Document]]) =
    Env.get("MONGODB_URI") match {
      case None =>
        logger.info("MONGODB_URI not set — MovieRepo disabled (in-memory cache only).")
        (None, None)
      case Some(uri) =>
        Try {
          val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
          val client = MongoClient(uri)
          val coll   = client.getDatabase(dbName).getCollection[Document]("movies")
          // Touch the collection to surface connectivity errors at startup,
          // not on the first read after the app is "up".
          Await.result(coll.countDocuments().toFuture(), 10.seconds)
          logger.info(s"MovieRepo connected to $dbName.movies")
          (client, coll)
        }.recover {
          case ex: Throwable =>
            logger.error(s"MovieRepo init failed (${ex.getMessage}) — falling back to in-memory cache.")
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
    s"${MovieService.normalize(title)}|${year.map(_.toString).getOrElse("")}"

  private def encode(id: String, title: String, year: Option[Int], e: MovieRecord): Document =
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
      // cinemaScrapes: per-(cinema, title, year) provenance the cache uses to
      // suppress redundant MovieRecordCreated events for tuples it has
      // already seen, and the source of the derived `cinemaTitles` view.
      // BsonArray of sub-documents. Sorted for stable Mongo diffs; empty
      // array (not null) when the set is empty so decoding never has to
      // disambiguate "missing field" from "no variants yet".
      "cinemaScrapes" -> BsonArray.fromIterable(
        e.cinemaScrapes.toSeq
          .sortBy(s => (s.cinema.displayName, s.title, s.year.getOrElse(Int.MinValue)))
          .map(encodeCinemaScrape)
      ),
      // Per-cinema data — sub-document keyed by Cinema.displayName.
      "cinemaShowings" -> encodeCinemaShowings(e.cinemaShowings),
      "updatedAt"    -> BsonDateTime(Instant.now().toEpochMilli)
    )

  // Sub-document keyed by Cinema.displayName so the keys are stable strings
  // (BSON doesn't have an enum type). Decoder maps the names back via
  // Cinema.all. Unknown cinema names are dropped silently — they can't be
  // resolved into a Cinema instance, so they don't go into the model.
  private def encodeCinemaShowings(map: Map[Cinema, CinemaShowings]): BsonDocument = {
    val doc = new BsonDocument()
    map.foreach { case (cinema, showings) =>
      doc.put(cinema.displayName, encodeOneCinemaShowings(showings))
    }
    doc
  }

  private def encodeOneCinemaShowings(s: CinemaShowings): BsonDocument = {
    val doc = new BsonDocument()
    s.filmUrl.foreach(u        => doc.put("filmUrl",        BsonString(u)))
    s.posterUrl.foreach(u      => doc.put("posterUrl",      BsonString(u)))
    s.synopsis.foreach(t       => doc.put("synopsis",       BsonString(t)))
    s.cast.foreach(t           => doc.put("cast",           BsonString(t)))
    s.director.foreach(t       => doc.put("director",       BsonString(t)))
    s.runtimeMinutes.foreach(n => doc.put("runtimeMinutes", BsonInt32(n)))
    s.releaseYear.foreach(n    => doc.put("releaseYear",    BsonInt32(n)))
    s.originalTitle.foreach(t  => doc.put("originalTitle",  BsonString(t)))
    doc.put("showtimes", BsonArray.fromIterable(s.showtimes.map(encodeShowtime)))
    doc
  }

  private def encodeCinemaScrape(s: CinemaScrape): BsonDocument = {
    val doc = new BsonDocument()
    doc.put("cinema", BsonString(s.cinema.displayName))
    doc.put("title",  BsonString(s.title))
    s.year.foreach(y => doc.put("year", BsonInt32(y)))
    doc
  }

  private def encodeShowtime(s: Showtime): BsonDocument = {
    val doc = new BsonDocument()
    // Store as epoch-millis in UTC. Tests pin a fixed clock; the wall-clock
    // semantics that matter are encoded in the LocalDateTime itself (no zone
    // attached), so we serialise via the UTC offset for symmetry on decode.
    doc.put("dateTime", BsonDateTime(s.dateTime.toInstant(ZoneOffset.UTC).toEpochMilli))
    s.bookingUrl.foreach(u => doc.put("bookingUrl", BsonString(u)))
    s.room.foreach(r => doc.put("room", BsonString(r)))
    if (s.format.nonEmpty) doc.put("format", BsonArray.fromIterable(s.format.map(BsonString(_))))
    doc
  }

  private def decode(d: Document): Option[(String, Option[Int], MovieRecord)] =
    for {
      // `imdbId` is optional now (TMDB hits without an IMDb cross-reference
      // still produce a row), but the row must at least have a `title` to be
      // usable as a cache key. Anything else missing is treated as None.
      title <- d.get("title").flatMap(v => Try(v.asString().getValue).toOption)
    } yield (
      title,
      d.get("year").flatMap(v => Try(v.asInt32().getValue).toOption),
      MovieRecord(
        imdbId        = d.get("imdbId").flatMap(v => Try(v.asString().getValue).toOption),
        imdbRating    = d.get("imdbRating").flatMap(v => Try(v.asDouble().getValue).toOption),
        metascore     = d.get("metascore").flatMap(v => Try(v.asInt32().getValue).toOption),
        originalTitle = d.get("originalTitle").flatMap(v => Try(v.asString().getValue).toOption),
        filmwebUrl     = d.get("filmwebUrl").flatMap(v => Try(v.asString().getValue).toOption),
        filmwebRating  = d.get("filmwebRating").flatMap(v => Try(v.asDouble().getValue).toOption),
        rottenTomatoes = d.get("rottenTomatoes").flatMap(v => Try(v.asInt32().getValue).toOption),
        tmdbId         = d.get("tmdbId").flatMap(v => Try(v.asInt32().getValue).toOption),
        metacriticUrl  = d.get("metacriticUrl").flatMap(v => Try(v.asString().getValue).toOption),
        rottenTomatoesUrl = d.get("rottenTomatoesUrl").flatMap(v => Try(v.asString().getValue).toOption),
        // Missing for rows that pre-date the provenance field; treat as
        // Set.empty so the very next scrape tick re-publishes once and
        // populates the set. The legacy `cinemaTitles` field, if present
        // in Mongo, is ignored — `cinemaTitles` is now derived from
        // `cinemaScrapes`, and the next write drops it from the doc.
        cinemaScrapes  = d.get("cinemaScrapes").flatMap(v => Try(decodeCinemaScrapes(v.asArray())).toOption).getOrElse(Set.empty),
        cinemaShowings = d.get("cinemaShowings").flatMap(v => Try(decodeCinemaShowings(v.asDocument())).toOption).getOrElse(Map.empty)
      )
    )

  private def decodeCinemaScrapes(arr: BsonArray): Set[CinemaScrape] = {
    val byName: Map[String, Cinema] = Cinema.all.map(c => c.displayName -> c).toMap
    arr.getValues.asScala.iterator.flatMap { v =>
      for {
        sub      <- Try(v.asDocument()).toOption
        cinemaN  <- Try(sub.get("cinema").asString().getValue).toOption
        cinema   <- byName.get(cinemaN)
        title    <- Try(sub.get("title").asString().getValue).toOption
      } yield CinemaScrape(
        cinema = cinema,
        title  = title,
        year   = Option(sub.get("year")).flatMap(v2 => Try(v2.asInt32().getValue).toOption)
      )
    }.toSet
  }

  private def decodeCinemaShowings(doc: BsonDocument): Map[Cinema, CinemaShowings] = {
    val byName: Map[String, Cinema] = Cinema.all.map(c => c.displayName -> c).toMap
    doc.entrySet().asScala.iterator.flatMap { entry =>
      for {
        cinema   <- byName.get(entry.getKey)
        sub      <- Try(entry.getValue.asDocument()).toOption
        showings <- Try(decodeOneCinemaShowings(sub)).toOption
      } yield cinema -> showings
    }.toMap
  }

  private def decodeOneCinemaShowings(doc: BsonDocument): CinemaShowings = {
    def str(field: String)  = Option(doc.get(field)).flatMap(v => Try(v.asString().getValue).toOption)
    def int32(field: String) = Option(doc.get(field)).flatMap(v => Try(v.asInt32().getValue).toOption)
    val showtimes = Option(doc.get("showtimes")).flatMap(v => Try(v.asArray()).toOption)
      .map(_.getValues.asScala.toSeq.flatMap(v => Try(decodeShowtime(v.asDocument())).toOption))
      .getOrElse(Seq.empty)
    CinemaShowings(
      filmUrl        = str("filmUrl"),
      posterUrl      = str("posterUrl"),
      synopsis       = str("synopsis"),
      cast           = str("cast"),
      director       = str("director"),
      runtimeMinutes = int32("runtimeMinutes"),
      releaseYear    = int32("releaseYear"),
      originalTitle  = str("originalTitle"),
      showtimes      = showtimes
    )
  }

  private def decodeShowtime(doc: BsonDocument): Showtime = {
    val epochMs = doc.get("dateTime").asDateTime().getValue
    val dateTime = LocalDateTime.ofInstant(Instant.ofEpochMilli(epochMs), ZoneOffset.UTC)
    val bookingUrl = Option(doc.get("bookingUrl")).flatMap(v => Try(v.asString().getValue).toOption)
    val room       = Option(doc.get("room")).flatMap(v => Try(v.asString().getValue).toOption)
    val format     = Option(doc.get("format")).flatMap(v => Try(v.asArray()).toOption)
      .map(_.getValues.asScala.toList.flatMap(v => Try(v.asString().getValue).toOption))
      .getOrElse(Nil)
    Showtime(dateTime, bookingUrl, room, format)
  }
}
