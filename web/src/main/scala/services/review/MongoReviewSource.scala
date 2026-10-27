package services.review

import org.bson.conversions.Bson
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonDocument, BsonValue}
import org.mongodb.scala.model.{Filters, Projections, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture}
import services.identity.{MongoIdentityModelStore, ResolverDecision, ResolverDecisionBson}
import services.movies.KeysetScan

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * One country's review reads, against ITS database on the local read-mirror — never prod: the wiring
 * builds these only from `MONGODB_MOVIES_MIRROR_URI`. Whole-collection reads are keyset-paged and
 * projected to what the review queue weighs; the film links are one `$or` over the refs asked about.
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

  def served(): Map[String, Option[Int]] = {
    val tmdbOf = Map.newBuilder[String, Int]
    scan(MoviesCollection, Filters.exists("tmdbId"), Projections.include("tmdbId"), tools.MongoReplies.Default) { batch =>
      batch.foreach { d => val b = d.toBsonDocument; int(b, "tmdbId").foreach(t => tmdbOf += idText(b.get("_id")) -> t) }
    }
    val films = tmdbOf.result()
    val out   = Map.newBuilder[String, Option[Int]]
    scan(ScreeningsCollection, Filters.exists("listingKey"), Projections.include("listingKey", "filmId"), tools.MongoReplies.Default) { batch =>
      batch.foreach { d =>
        val b = d.toBsonDocument
        string(b, "listingKey").foreach(k => out += k -> Option(b.get("filmId")).filterNot(_.isNull).map(idText).flatMap(films.get))
      }
    }
    out.result()
  }
}

object MongoReviewSource {
  val MoviesCollection     = services.movies.MovieRepository.Collection
  val ScreeningsCollection = services.movies.ScreeningsRepository.Collection

  private def idText(v: BsonValue): String = if (v.isString) v.asString.getValue else if (v.isObjectId) v.asObjectId.getValue.toHexString else v.toString
  private def string(d: BsonDocument, name: String): Option[String] =
    Option(d.get(name)).filter(_.isString).map(_.asString.getValue.trim).filter(_.nonEmpty)
  private def int(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).filter(_.isNumber).map(_.asNumber.intValue)
}
