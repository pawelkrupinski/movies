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
         /** Restrict to the films listed under these CINEMA-REPORTED titles — how the
          *  SAMPLE baseline is taken, so it counts prod's coverage of exactly the films
          *  the sample corpus replays. Matched as exact strings against `movie_slots`,
          *  because prod stores each cinema's own spelling there and that is the one
          *  identity both sides share (see `CorpusSample.titlesOf`). `None` counts
          *  every film. */
         onlySlotTitles: Option[Set[String]] = None): ProdCoverageBaseline = {
    val screeningNow = Await.result(
      database.getCollection("screenings")
        .aggregate(Seq(
          Aggregates.filter(Filters.gte("showtimes.dateTime", now)),
          Aggregates.group("$filmId")))
        .allowDiskUse(true)
        .toFuture(), 10.minutes)
      .flatMap(_.get("_id").map(_.asString().getValue))

    // Exact-match the sampled titles against the slots — an `$in` on the values prod
    // already stores, so no scan crosses the tunnel and no normalisation has to agree.
    val sampled: Option[Set[String]] = onlySlotTitles.map { titles =>
      Await.result(
        database.getCollection("movie_slots")
          .find(Filters.in("slot.title", titles.toSeq*))
          .projection(org.mongodb.scala.model.Projections.include("filmId"))
          .toFuture(), 10.minutes)
        .flatMap(_.get("filmId").map(_.asString().getValue))
        .toSet
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
