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
  def of(database: MongoDatabase, now: java.time.Instant = java.time.Instant.now()): ProdCoverageBaseline = {
    val screeningNow = Await.result(
      database.getCollection("screenings")
        .aggregate(Seq(
          Aggregates.filter(Filters.gte("showtimes.dateTime", now)),
          Aggregates.group("$filmId")))
        .allowDiskUse(true)
        .toFuture(), 10.minutes)
      .flatMap(_.get("_id").map(_.asString().getValue))

    val movies  = database.getCollection("movies")
    val present = (field: String) => Filters.and(
      Filters.in("_id", screeningNow*), Filters.ne(field, null), Filters.exists(field))

    def count(filter: org.mongodb.scala.bson.conversions.Bson): Int =
      Await.result(movies.countDocuments(filter).toFuture(), 10.minutes).toInt

    ProdCoverageBaseline(
      recordedAt     = now,
      films          = count(Filters.in("_id", screeningNow*)),
      tmdbId         = count(present("tmdbId")),
      imdbId         = count(present("imdbId")),
      imdbRating     = count(present("imdbRating")),
      filmwebRating  = count(present("filmwebRating")),
      metascore      = count(present("metascore")),
      rottenTomatoes = count(present("rottenTomatoes")))
  }
}
