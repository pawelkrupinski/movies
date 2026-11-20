package services.review

import scala.jdk.CollectionConverters.*

import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.{Filters, Projections}

/** The review reads of the resolver's stored TMDB records and posters, and of the corpus's served listings. */
trait MongoFilmRecordReads extends MongoReviewReads {
  import MongoFilmRecordReads._
  import ReviewBson._

  def served(): Map[String, Option[Int]] = {
    val tmdbOf = Map.newBuilder[String, Int]
    scan(ReviewCollections.Movies, Filters.exists("tmdbId"), Projections.include("tmdbId"), tools.MongoReplies.Default) { batch =>
      batch.foreach { d => val b = d.toBsonDocument; int(b, "tmdbId").foreach(t => tmdbOf += idText(b.get("_id")) -> t) }
    }
    val films = tmdbOf.result()
    val out   = Map.newBuilder[String, Option[Int]]
    scan(ReviewCollections.Screenings, Filters.exists("listingKey"), Projections.include("listingKey", "filmId"), tools.MongoReplies.Default) { batch =>
      batch.foreach { d =>
        val b = d.toBsonDocument
        string(b, "listingKey").foreach(k => out += k -> Option(b.get("filmId")).filterNot(_.isNull).map(idText).flatMap(films.get))
      }
    }
    out.result()
  }

  def filmRecords(tmdbIds: Seq[Int]): Map[Int, FilmCard] =
    tmdbIds.distinct.grouped(500).flatMap { ids =>
      val records = await(collection(ReviewCollections.TmdbFilms).find(Filters.in("_id", ids.map(_.toString)*)).projection(Projections.include("record", "hit", "local.credits.cast"))
        .batchSize(tools.MongoReplies.Default).toFuture()).flatMap(d => filmRecord(d.toBsonDocument))
      val posterIds = TmdbPosterLookup.answerIds(ids)
      val posters = await(collection(ReviewCollections.FamilyAnswers).find(Filters.in("_id", posterIds.keys.toSeq*))
        .projection(Projections.include(TmdbPosterLookup.AnswerPaths)).batchSize(tools.MongoReplies.Default).toFuture())
        .flatMap(d => posterIds.get(idText(d.toBsonDocument.get("_id"))).zip(posterOf(d.toBsonDocument))).toMap
      records.map { case (tmdb, card) => tmdb -> card.copy(poster = posters.get(tmdb)) }
    }.toMap
}

private[review] object MongoFilmRecordReads {
  import ReviewBson._

  /** A `tmdb_films` document as a film card: its parsed `record` (worker `IdentityAnswerBson.film`), else the search
   *  `hit` that stands in until the record is fetched. `None` when neither carries a title. Its cast is the top-billed
   *  names the localized response's `credits` block keeps (worker `TmdbNormalizer.minimal`): the record holds only their
   *  hashes. */
  private[review] def filmRecord(d: BsonDocument): Option[(Int, FilmCard)] =
    for {
      tmdb  <- Option(d.get("_id")).map(idText).flatMap(_.toIntOption)
      film  <- doc(d, "record").orElse(doc(d, "hit"))
      title <- string(film, "title")
    } yield tmdb -> FilmCard(tmdb, int(film, "imdbNumber").filter(_ > 0).map(n => f"tt$n%07d"), Some(title),
      string(film, "originalTitle"), int(film, "year"), strings(film, "directors"), int(film, "runtime"), None, None,
      doc(d, "local").flatMap(doc(_, "credits")).flatMap(c => Option(c.get("cast"))).filter(_.isArray).toSeq
        .flatMap(_.asArray.getValues.asScala).filter(_.isDocument).flatMap(n => string(n.asDocument, "name")))

  /** A film's poster document (`poster|film|<tmdbId>`) as the film's TMDB poster: the first of the paths filed beside its
   *  hashes, the one the film's country's language prefers. `None` for one filed before paths were. */
  def posterOf(d: BsonDocument): Option[String] = TmdbPosterLookup.posterOf(d)
}
