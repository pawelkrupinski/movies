package services.review

import org.bson.conversions.Bson
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonDocument, BsonValue}
import org.mongodb.scala.model.{Aggregates, Filters, Projections, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture}
import services.identity.{MongoIdentityModelStore, ResolverDecision, ResolverDecisionBson}
import services.movies.{KeysetScan, SlotsRepository}

import java.time.{Instant, LocalDateTime, ZoneOffset}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * One country's review reads, against ITS database on the local read-mirror — never prod: the wiring
 * builds these only from `MONGODB_MOVIES_MIRROR_URI`. Whole-collection reads are keyset-paged and
 * projected to what a page shows; every other read is an `$in` over the keys of the cards on screen.
 */
final class MongoReviewSource(db: MongoDatabase) extends ReviewSource {
  import MongoReviewSource._
  private val Timeout = 30.seconds
  private def collection(name: String): MongoCollection[Document] = db.getCollection[Document](name)
  private def await[A](f: scala.concurrent.Future[A]): A = Await.result(f, Timeout)

  /** A keyset scan of `name` under `filter`, projected — every page bounded, retried, `_id`-ordered. */
  private def scan(name: String, filter: Bson, projection: Bson, batch: Int)(onBatch: Seq[Document] => Unit): Unit = {
    val outcome = KeysetScan.scan[Document](s"review $name", batch, maxAttempts = 3, initialBackoff = 200.millis,
      keyOf = _.get("_id").map(idText).getOrElse(""),
      fetchPage = (after, limit) => await(collection(name)
        .find(after.fold(filter)(id => Filters.and(filter, Filters.gt("_id", id)))).projection(projection)
        .sort(Sorts.ascending("_id")).limit(limit).batchSize(limit).toFuture()))(onBatch)
    if (!outcome.isComplete) throw new IllegalStateException(s"review: $name read incomplete")
  }

  def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision] = {
    val out = Vector.newBuilder[ResolverDecision]
    scan(MongoIdentityModelStore.FamiliesCollection,
      if (unmatchedOnly) Filters.eq("decisions.film", null) else Filters.empty(),
      Projections.include("decisions"), tools.MongoReplies.Families) { batch =>
      batch.foreach(d => d.get("decisions").filter(_.isArray).foreach(_.asArray.getValues.asScala
        .foreach(v => out += ResolverDecisionBson.decode(v.asDocument))))
    }
    out.result()
  }

  def slots(listingKeys: Seq[String]): Map[String, SlotFacts] =
    listingKeys.distinct.grouped(500).flatMap { keys =>
      await(collection(SlotsRepository.Collection).find(Filters.in("listingKey", keys*))
        .projection(Projections.include("listingKey", "slot", "updatedAt")).batchSize(tools.MongoReplies.Default).toFuture())
        .flatMap { d =>
          val b = d.toBsonDocument
          for { key <- string(b, "listingKey"); slot <- doc(b, "slot") }
            yield key -> SlotFacts(facts(slot, poster = "posterUrl"), instant(b, "updatedAt").getOrElse(Instant.EPOCH))
        }
    }.toMap

  def updatedSince(since: Instant): Map[String, Instant] = {
    val out = Map.newBuilder[String, Instant]
    scan(SlotsRepository.Collection, Filters.and(Filters.gte("updatedAt", java.util.Date.from(since)), Filters.exists("listingKey")),
      Projections.include("listingKey", "updatedAt"), tools.MongoReplies.Default) { batch =>
      batch.foreach { d => val b = d.toBsonDocument; for { k <- string(b, "listingKey"); at <- instant(b, "updatedAt") } out += k -> at }
    }
    out.result()
  }

  def venuePages(urls: Seq[String]): Map[String, VenueFacts] =
    urls.distinct.grouped(200).flatMap { batch =>
      await(collection(VenuePagesCollection).find(Filters.and(Filters.in("page", batch*), Filters.exists("gone", false)))
        .sort(Sorts.ascending("readAt")).batchSize(tools.MongoReplies.Default).toFuture())
        .flatMap { d => val b = d.toBsonDocument; string(b, "page").map(_ -> facts(b, poster = "posterUrl")) }
    }.toMap

  def feeds(listings: Seq[(String, String)]): Map[(String, String), ListingFeed] = {
    val byVenue = listings.distinct.groupMap(_._1)(_._2)
    if (byVenue.isEmpty) Map.empty
    else {
      // Only the listed films, and of their showtimes only the count and the span, leave the server.
      val pipeline = Seq(
        Aggregates.`match`(Filters.in("_id", byVenue.keys.toSeq*)),
        Aggregates.unwind("$films"),
        Aggregates.project(Document(
          "raw" -> Document("$ifNull" -> org.mongodb.scala.bson.BsonArray("$films.movie.rawTitle", "$films.movie.title")),
          "externalIds" -> "$films.externalIds",
          "screenings" -> Document("$size" -> Document("$ifNull" -> org.mongodb.scala.bson.BsonArray("$films.showtimes", org.mongodb.scala.bson.BsonArray()))),
          "first" -> Document("$min" -> "$films.showtimes.dateTime"),
          "last" -> Document("$max" -> "$films.showtimes.dateTime"))),
        Aggregates.`match`(Filters.in("raw", byVenue.values.flatten.toSeq.distinct*)))
      await(collection(ListingsCollection).aggregate(pipeline).batchSize(tools.MongoReplies.Default).toFuture()).flatMap { d =>
        val b = d.toBsonDocument
        for { venue <- string(b, "_id"); raw <- string(b, "raw") if byVenue.get(venue).exists(_.contains(raw)) } yield
          (venue, raw) -> ListingFeed(
            ListingFeed.catalogueIdsOf(b.get("externalIds")),
            Option(b.get("screenings")).filter(_.isInt32).fold(0)(_.asInt32.getValue),
            instant(b, "first").map(localTime), instant(b, "last").map(localTime))
      }.toMap
    }
  }

  def filmLinks(refs: Seq[FilmRef]): Seq[Set[FilmRef]] = {
    def ids(source: String) = refs.filter(_.source == source).map(_.id).distinct
    // a site's id is the tail of the URL the record stores ("…/film/Franz+Kafka-2025-10008278", "…/m/dolly")
    def urlEndingIn(field: String, source: String, prefix: String) = ids(source).map(id =>
      Filters.regex(field, s"$prefix${java.util.regex.Pattern.quote(id)}/?$$"))
    val clauses = ids("tmdb").flatMap(_.toIntOption).map(Filters.eq("tmdbId", _)) ++ ids("imdb").map(Filters.eq("imdbId", _)) ++
      urlEndingIn("filmwebUrl", "filmweb", "-") ++ urlEndingIn("rottenTomatoesUrl", "rt", "/m/") ++
      urlEndingIn("metacriticUrl", "metacritic", "/movie/")
    if (clauses.isEmpty) Nil
    else await(collection(MoviesCollection).find(Filters.or(clauses*))
      .projection(Projections.include("tmdbId", "imdbId", "filmwebUrl", "rottenTomatoesUrl", "metacriticUrl"))
      .batchSize(tools.MongoReplies.Default).toFuture()).map(_.toBsonDocument).map { b =>
      (int(b, "tmdbId").map(FilmRef.tmdb).toSeq ++ string(b, "imdbId").flatMap(FilmRef.parse) ++
        Seq("filmwebUrl", "rottenTomatoesUrl", "metacriticUrl").flatMap(string(b, _)).flatMap(FilmRef.parse)).toSet
    }.filter(_.sizeIs > 1)
  }

  def filmRecords(tmdbIds: Seq[Int]): Map[Int, FilmCard] =
    tmdbIds.distinct.map(_.toString).grouped(500).flatMap { ids =>
      await(collection(TmdbFilmsCollection).find(Filters.in("_id", ids*)).projection(Projections.include("record", "hit"))
        .batchSize(tools.MongoReplies.Default).toFuture()).flatMap(d => filmRecord(d.toBsonDocument))
    }.toMap

  def films(tmdbIds: Seq[Int]): Map[Int, FilmCard] = {
    val ids = tmdbIds.distinct
    if (ids.isEmpty) Map.empty
    else {
      val records = await(collection(MoviesCollection).find(Filters.in("tmdbId", ids*))
        .projection(Projections.include("tmdbId", "imdbId")).batchSize(tools.MongoReplies.Default).toFuture()).map(_.toBsonDocument).flatMap { b =>
        Option(b.get("tmdbId")).filter(_.isNumber).map(t => (idText(b.get("_id")), t.asNumber.intValue, string(b, "imdbId")))
      }
      val filmIds = records.map(_._1)
      val served  = if (filmIds.isEmpty) Map.empty[String, BsonDocument]
        else await(collection(WebMoviesCollection).find(Filters.in("_id", filmIds*)).projection(Projections.exclude("synopsisByCity", "ratings"))
          .batchSize(tools.MongoReplies.Films).toFuture()).map(d => idText(d.toBsonDocument.get("_id")) -> d.toBsonDocument).toMap
      val tmdbSlots = if (filmIds.isEmpty) Map.empty[String, BsonDocument]
        else await(collection(SlotsRepository.Collection).find(Filters.and(Filters.in("filmId", filmIds*), Filters.eq("slotKey", "TMDB")))
          .projection(Projections.include("filmId", "slot")).batchSize(tools.MongoReplies.Default).toFuture())
          .flatMap(d => doc(d.toBsonDocument, "slot").map(idText(d.toBsonDocument.get("filmId")) -> _)).toMap
      records.map { case (filmId, tmdb, imdb) =>
        val web  = served.get(filmId).map(facts(_, poster = "posterUrl", directors = "directors", synopsis = "synopsis"))
        val slot = tmdbSlots.get(filmId).map(facts(_, poster = "posterUrl"))
        val f    = web.getOrElse(VenueFacts())
        val s    = slot.getOrElse(VenueFacts())
        tmdb -> FilmCard(tmdb, imdb, f.title.orElse(s.title), f.originalTitle.orElse(s.originalTitle), f.year.orElse(s.year),
          if (f.directors.nonEmpty) f.directors else s.directors, f.runtime.orElse(s.runtime), f.poster.orElse(s.poster),
          f.synopsis.orElse(s.synopsis))
      }.toMap
    }
  }
}

object MongoReviewSource {
  val VenuePagesCollection = services.DebugMirror.VenuePages
  val ListingsCollection   = services.DebugMirror.IdentityListings
  val TmdbFilmsCollection  = services.DebugMirror.TmdbFilms
  val MoviesCollection     = services.movies.MovieRepository.Collection
  val WebMoviesCollection  = services.readmodel.MongoReadModelRepository.MoviesCollection


  /** A `tmdb_films` document as a film card: its parsed `record` (worker `IdentityAnswerBson.film`), else the search
   *  `hit` that stands in until the record is fetched. `None` when neither carries a title. */
  private[review] def filmRecord(d: BsonDocument): Option[(Int, FilmCard)] =
    for {
      tmdb  <- Option(d.get("_id")).map(idText).flatMap(_.toIntOption)
      film  <- doc(d, "record").orElse(doc(d, "hit"))
      title <- string(film, "title")
    } yield tmdb -> FilmCard(tmdb, int(film, "imdbNumber").filter(_ > 0).map(n => f"tt$n%07d"), Some(title),
      string(film, "originalTitle"), int(film, "year"), strings(film, "directors"), int(film, "runtime"), None, None)

  private def idText(v: BsonValue): String = if (v.isString) v.asString.getValue else if (v.isObjectId) v.asObjectId.getValue.toHexString else v.toString
  private def string(d: BsonDocument, name: String): Option[String] =
    Option(d.get(name)).filter(_.isString).map(_.asString.getValue.trim).filter(_.nonEmpty)
  private def int(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).filter(_.isNumber).map(_.asNumber.intValue)
  private def strings(d: BsonDocument, name: String): Seq[String] =
    Option(d.get(name)).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.collect { case s if s.isString => s.asString.getValue })
  private def doc(d: BsonDocument, name: String): Option[BsonDocument] = Option(d.get(name)).filter(_.isDocument).map(_.asDocument)
  private def instant(d: BsonDocument, name: String): Option[Instant] =
    Option(d.get(name)).filter(_.isDateTime).map(v => Instant.ofEpochMilli(v.asDateTime.getValue))
  /** A showtime's stored date-time is a local time written as UTC. */
  private def localTime(at: Instant): String = LocalDateTime.ofInstant(at, ZoneOffset.UTC).toString.replace("T", " ")

  /** The facts a slot, venue page or served film document holds, under their field names. */
  private[review] def facts(d: BsonDocument, poster: String, directors: String = "director", synopsis: String = "synopsis"): VenueFacts =
    VenueFacts(string(d, "title"), string(d, "originalTitle"), int(d, "releaseYear"), strings(d, directors), int(d, "runtimeMinutes"),
      strings(d, "cast"), strings(d, "countries"), string(d, poster), string(d, synopsis))
}
