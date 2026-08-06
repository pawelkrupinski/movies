package tools

import org.mongodb.scala.{MongoDatabase, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.model.{Aggregates, Filters}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Reads a production database's enrichment coverage, restricted to the films that
 * are actually SCREENING — the set a corpus snapshot describes.
 *
 * Lives beside the corpus recorder rather than in the convergence suite because the
 * suite must never hold a production connection; see [[ProdCoverageBaseline]] for
 * why the capture happens here instead.
 *
 * "Screening" means the film has at least one showtime still in the future at the
 * moment of the read. Without that restriction the comparison is meaningless: prod
 * keeps a film's row after its last showtime passes, so the collection is a running
 * total while the corpus is a single day.
 */
object ProdCoverage {

  /** Server-side throughout: the counts are computed in mongod and only the numbers
   *  cross the wire, which matters because this runs over a `flyctl proxy` that a
   *  streaming full-collection scan reliably wedges. */
  def of(database: MongoDatabase,
         now: java.time.Instant = java.time.Instant.now(),
         /** Restrict to the films carrying these CINEMA SLOT KEYS — how the SAMPLE
          *  baseline is taken, so it counts prod's coverage of exactly the films the
          *  sample corpus replays. `None` counts every film.
          *
          *  The key (`"<cinema>␟<titleKey>"`, `CinemaShowing.keyFor`) is what both
          *  sides derive from the same (cinema, title), which the stored slot TITLE is
          *  not: prod strips a listing's decoration before storing it, so a corpus
          *  "The Room [dubbing]" never equalled the "The Room" on prod's slot. */
         onlySlotKeys: Option[Set[String]] = None): ProdCoverageBaseline = {
    val screeningNow = Await.result(
      database.getCollection("screenings")
        .aggregate(Seq(
          Aggregates.filter(Filters.gte("showtimes.dateTime", now)),
          Aggregates.group("$filmId")))
        .allowDiskUse(true)
        .toFuture(), 10.minutes)
      .flatMap(_.get("_id").map(_.asString().getValue))

    // Exact-match the sampled slot keys — an `$in` on the keys prod already stores, so
    // no scan crosses the tunnel and no normalisation has to agree.
    //
    // Read from BOTH homes a cinema slot can live in, and union them. `movies.sourceData`
    // is being migrated into the `movie_slots` side collection and the migration is only
    // partway done — 259 of production's 940 rows still carried their slots embedded when
    // this was written. Reading the side collection alone therefore SHADOWS the embedded
    // rows rather than unioning them, and every film that has not migrated yet is counted
    // as one production does not have: Poland's sample leg read `films run=94 prod=73`,
    // and unioning the embedded slots back in makes it prod=90 on the same repertoire.
    // How wrong the number is depends only on which films the sample happens to draw,
    // which is why it moved from 4.4% (passing) to 28.8% (failing) overnight without
    // anything in the pipeline changing.
    val sampled: Option[Set[String]] = onlySlotKeys.map { keys =>
      val migrated = Await.result(
        database.getCollection("movie_slots")
          .find(Filters.in("slotKey", keys.toSeq*))
          .projection(org.mongodb.scala.model.Projections.include("filmId"))
          .toFuture(), 10.minutes)
        .flatMap(_.get("filmId").map(_.asString().getValue))

      // `sourceData` is a MAP keyed by the slot key, so it takes `$objectToArray` to
      // match a key server-side. Still server-side, still ids-only over the wire.
      val embedded = Await.result(
        database.getCollection("movies")
          .aggregate(Seq(
            Aggregates.project(org.mongodb.scala.bson.collection.immutable.Document(
              "slots" -> org.mongodb.scala.bson.collection.immutable.Document(
                "$objectToArray" -> org.mongodb.scala.bson.collection.immutable.Document(
                  "$ifNull" -> org.mongodb.scala.bson.BsonArray(
                    org.mongodb.scala.bson.BsonString("$sourceData"),
                    org.mongodb.scala.bson.BsonDocument()))))),
            Aggregates.unwind("$slots"),
            Aggregates.filter(Filters.in("slots.k", keys.toSeq*)),
            Aggregates.group("$_id")))
          .allowDiskUse(true)
          .toFuture(), 10.minutes)
        .flatMap(_.get("_id").map(_.asString().getValue))

      (migrated ++ embedded).toSet
    }
    val screening = sampled.fold(screeningNow)(keep => screeningNow.filter(keep.contains))

    val movies  = database.getCollection("movies")
    val present = (field: String) => Filters.and(
      Filters.in("_id", screening*), Filters.ne(field, null), Filters.exists(field))

    def count(filter: org.mongodb.scala.bson.conversions.Bson): Int =
      Await.result(movies.countDocuments(filter).toFuture(), 10.minutes).toInt

    ProdCoverageBaseline(
      recordedAt     = now,
      films          = count(Filters.in("_id", screening*)),
      tmdbId         = count(present("tmdbId")),
      imdbId         = count(present("imdbId")),
      imdbRating     = count(present("imdbRating")),
      filmwebRating  = count(present("filmwebRating")),
      metascore      = count(present("metascore")),
      rottenTomatoes = count(present("rottenTomatoes")))
  }
}
