package services.review

import org.mongodb.scala.ObservableFuture
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.{Filters, Projections}

/** The review reads of the corpus's films: their cards from what the read model serves (else their TMDB slot), and
 *  the films several sites' ids name. */
trait MongoFilmReads extends MongoReviewReads {
  import MongoFilmReads._
  import ReviewBson._

  def filmLinks(refs: Seq[FilmRef]): Seq[Set[FilmRef]] = {
    def ids(source: String) = refs.filter(_.source == source).map(_.id).distinct
    // a site's id is the tail of the URL the record stores ("…/film/Franz+Kafka-2025-10008278", "…/m/dolly")
    def urlEndingIn(field: String, source: String, prefix: String) = ids(source).map(id =>
      Filters.regex(field, s"$prefix${java.util.regex.Pattern.quote(id)}/?$$"))
    val clauses = ids("tmdb").flatMap(_.toIntOption).map(Filters.eq("tmdbId", _)) ++ ids("imdb").map(Filters.eq("imdbId", _)) ++
      urlEndingIn("filmwebUrl", "filmweb", "-") ++ urlEndingIn("rottenTomatoesUrl", "rt", "/m/") ++
      urlEndingIn("metacriticUrl", "metacritic", "/movie/")
    if (clauses.isEmpty) Nil
    else await(collection(ReviewCollections.Movies).find(Filters.or(clauses*))
      .projection(Projections.include("tmdbId", "imdbId", "filmwebUrl", "rottenTomatoesUrl", "metacriticUrl"))
      .batchSize(tools.MongoReplies.Default).toFuture()).map(_.toBsonDocument).map { b =>
      (int(b, "tmdbId").map(FilmRef.tmdb).toSeq ++ string(b, "imdbId").flatMap(FilmRef.parse) ++
        Seq("filmwebUrl", "rottenTomatoesUrl", "metacriticUrl").flatMap(string(b, _)).flatMap(FilmRef.parse)).toSet
    }.filter(_.sizeIs > 1)
  }

  def films(tmdbIds: Seq[Int]): Map[Int, FilmCard] = {
    val records = corpusFilms(tmdbIds.distinct)
    val filmIds = records.map(_._1)
    if (filmIds.isEmpty) Map.empty
    else {
      val served    = servedMovies(filmIds)
      val tmdbSlots = tmdbSlotsOf(filmIds)
      records.map { case (filmId, tmdb, imdb) =>
        val web  = served.get(filmId).map(facts(_, poster = "posterUrl", directors = "directors", synopsis = "synopsis"))
        val slot = tmdbSlots.get(filmId).map(facts(_, poster = "posterUrl"))
        tmdb -> filmCard(tmdb, imdb, web.getOrElse(VenueFacts()), slot.getOrElse(VenueFacts()))
      }.toMap
    }
  }

  /** The corpus's films carrying these TMDB ids: (film id, TMDB id, IMDb id). */
  private def corpusFilms(tmdbIds: Seq[Int]): Seq[(String, Int, Option[String])] =
    if (tmdbIds.isEmpty) Nil
    else await(collection(ReviewCollections.Movies).find(Filters.in("tmdbId", tmdbIds*))
      .projection(Projections.include("tmdbId", "imdbId")).batchSize(tools.MongoReplies.Default).toFuture()).map(_.toBsonDocument).flatMap { b =>
      Option(b.get("tmdbId")).filter(_.isNumber).map(t => (idText(b.get("_id")), t.asNumber.intValue, string(b, "imdbId")))
    }

  /** The read model's documents of these films, by film id. */
  private def servedMovies(filmIds: Seq[String]): Map[String, BsonDocument] =
    await(collection(ReviewCollections.WebMovies).find(Filters.in("_id", filmIds*)).projection(Projections.exclude("synopsisByCity", "ratings"))
      .batchSize(tools.MongoReplies.Films).toFuture()).map(d => idText(d.toBsonDocument.get("_id")) -> d.toBsonDocument).toMap

  /** These films' TMDB slots, by film id. */
  private def tmdbSlotsOf(filmIds: Seq[String]): Map[String, BsonDocument] =
    await(collection(ReviewCollections.Slots).find(Filters.and(Filters.in("filmId", filmIds*), Filters.eq("slotKey", "TMDB")))
      .projection(Projections.include("filmId", "slot")).batchSize(tools.MongoReplies.Default).toFuture())
      .flatMap(d => doc(d.toBsonDocument, "slot").map(idText(d.toBsonDocument.get("filmId")) -> _)).toMap
}

private[review] object MongoFilmReads {
  /** A film's card from what the read model serves of it, each fact falling back to its TMDB slot's. */
  def filmCard(tmdb: Int, imdb: Option[String], served: VenueFacts, slot: VenueFacts): FilmCard = {
    def orSlot(own: Seq[String], fallback: Seq[String]): Seq[String] = if (own.nonEmpty) own else fallback
    FilmCard(tmdb, imdb, served.title.orElse(slot.title), served.originalTitle.orElse(slot.originalTitle),
      served.year.orElse(slot.year), orSlot(served.directors, slot.directors), served.runtime.orElse(slot.runtime),
      served.poster.orElse(slot.poster), served.synopsis.orElse(slot.synopsis), orSlot(served.cast, slot.cast))
  }
}
