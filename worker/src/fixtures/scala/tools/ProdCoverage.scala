package tools

import org.mongodb.scala.{MongoDatabase, ObservableFuture}
import org.mongodb.scala.model.{Accumulators, Aggregates, Filters, Projections}

import scala.concurrent.Await
import scala.jdk.CollectionConverters._

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
 *
 * [[CorpusCoverage]] is the other side of that comparison and applies the same
 * restriction to the replay, which has a running total of its own. The two must move
 * together: a change to what counts as screening here that is not made there turns
 * the band into a measure of the difference between the two definitions.
 */
object ProdCoverage {

  /** Production's coverage of the films screening at `now`: [[films]], counted. */
  def of(database: MongoDatabase, now: java.time.Instant, onlySlotKeys: Option[Set[String]] = None): ProdCoverageBaseline =
    ProdCoverageBaseline.of(now, films(database, now, onlySlotKeys))

  /** Every film screening at `now`, with the axes it holds a value on and its screening slot keys — the per-film
   *  record a leg's diff names films from, and what [[of]] counts, so the two can never disagree.
   *
   *  Narrow throughout, because this runs over a tunnel that a streaming full-collection scan reliably wedges:
   *  the screening films and their slot keys are grouped in mongod, and the `movies` read is by `_id` over just
   *  those films with only the fields counted projected — a few hundred KB for the largest country. */
  def films(database: MongoDatabase,
            now: java.time.Instant,
            /** Restrict to the films carrying these CINEMA SLOT KEYS — how the SAMPLE
             *  baseline is taken, so it counts prod's coverage of exactly the films the
             *  sample corpus replays. `None` counts every film.
             *
             *  The key (`"<cinema>␟<titleKey>"`, `CinemaShowing.keyFor`) is what both
             *  sides derive from the same (cinema, title), which the stored slot TITLE is
             *  not: prod strips a listing's decoration before storing it, so a corpus
             *  "The Room [dubbing]" never equalled the "The Room" on prod's slot. */
            onlySlotKeys: Option[Set[String]] = None): Seq[ProdFilm] = {
    val slotsOf: Map[String, Seq[String]] = Await.result(
      database.getCollection("screenings")
        .aggregate(Seq(
          Aggregates.filter(Filters.gte("showtimes.dateTime", now)),
          Aggregates.group("$filmId", Accumulators.addToSet("slots", "$slotKey"))))
        .allowDiskUse(true)
        .toFuture(), SpecTimeouts.Run)
      .flatMap(group => group.get("_id").map(_.asString().getValue -> group.get("slots").fold(Seq.empty[String])(
        _.asArray().getValues.asScala.toSeq.map(_.asString().getValue).sorted)))
      .toMap
    val screeningNow = slotsOf.keys.toSeq

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
          .projection(Projections.include("filmId"))
          .toFuture(), SpecTimeouts.Run)
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
          .toFuture(), SpecTimeouts.Run)
        .flatMap(_.get("_id").map(_.asString().getValue))

      (migrated ++ embedded).toSet
    }
    val screening = sampled.fold(screeningNow)(keep => screeningNow.filter(keep.contains))

    Await.result(
      database.getCollection("movies")
        .find(Filters.in("_id", screening*))
        .projection(Projections.include(("key" +: "tmdbBasis" +: ProdFilms.Axes)*))
        .toFuture(), SpecTimeouts.Run)
      .map { row =>
        def present(field: String) = row.get(field).exists(value => !value.isNull)
        val id = row.get("_id").fold("")(_.asString().getValue)
        ProdFilm(
          id        = id,
          key       = row.get("key").filter(_.isString).fold(id)(_.asString().getValue),
          tmdbId    = row.get("tmdbId").filter(_.isNumber).map(_.asNumber().intValue()),
          tmdbBasis = row.get("tmdbBasis").filter(_.isString).map(_.asString().getValue),
          imdbId    = row.get("imdbId").filter(_.isString).map(_.asString().getValue),
          covered   = ProdFilms.Axes.filter(present).toSet,
          slots     = slotsOf.getOrElse(id, Nil))
      }
      .sortBy(_.id)
  }
}
