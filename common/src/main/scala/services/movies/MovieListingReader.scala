package services.movies

import scala.concurrent.Await
import scala.concurrent.duration.*

import models.StoredMovieRecord
import org.bson.conversions.Bson
import org.mongodb.scala.model.{Aggregates, Sorts}
import org.mongodb.scala.{MongoDatabase, ObservableFuture}
import play.api.Logging
import services.evidence.LatestTitleYear
import services.titles.TitleNormalizer

/** The corpus LISTING read: every stored row with each source's `showtimes` dropped — the rows for a listing that
 *  renders only per-cinema metadata + counts, never the showtimes themselves (the dev `/debug` corpus table; showtimes
 *  there are fetched per-row on expand). Measured on prod, `showtimes` are ~58% of the corpus bytes, so omitting them
 *  roughly halves what the scan transfers and holds. Callers that NEED showtimes (cache hydrate, read-model
 *  projection) read the [[MovieRepositoryReader]] whole. */
trait MovieListingReader {
  def findAllForListing(): Seq[StoredMovieRecord]
}

object MovieListingReader {
  /** The listing of any store, stripped in-process via `StoredMovieRecord.withoutShowtimes` — the in-memory store's
   *  view, identical to [[MongoMovieListingReader]]'s. */
  def stripping(reader: MovieRepositoryReader): MovieListingReader = () => reader.findAll().map(_.withoutShowtimes)
}

/** The `movies` listing read SERVER-SIDE, so the showtimes never cross the wire, its slots stitched from `slots`
 *  (`movie_slots`) like every other reader's. The database is the composition root's, as [[MongoMovieRepository]]'s;
 *  `None` lists nothing. `normalizer` is the rules the rows were keyed under; `clock` the root's. */
final class MongoMovieListingReader(
  sharedDb:   Option[MongoDatabase],
  slots:      Option[SlotsRepository],
  normalizer: TitleNormalizer,
  clock:      java.time.Clock
) extends MovieListingReader with Logging {
  private given LatestTitleYear = LatestTitleYear(clock)
  private val stitch = new MovieSideStitch(screenings = None, slots, normalizer, logger)
  private lazy val coll = sharedDb.map(_.withCodecRegistry(MovieCodecs.registry).getCollection[StoredMovieDto](MovieRepository.Collection))

  /** Strips each source's `showtimes` SERVER-SIDE so they never cross the wire:
   *  rewrites `sourceData` (a dynamic-cinema-keyed subdocument, so a plain
   *  field-exclusion projection can't target it) by mapping every value through
   *  `$objectToArray` → `$filter` out the `showtimes` key → `$arrayToObject`.
   *  `$sort` on `_id` stays the FIRST stage (index-backed) for the same exactly-once guarantee as
   *  [[MongoMovieRepository.findAll]]. A missing `showtimes` decodes to
   *  `Seq.empty` (`MovieCodecs.BackwardCompatibleSourceDataCodec`), so the result
   *  round-trips through the normal `StoredMovieDto` codec. */
  def findAllForListing(): Seq[StoredMovieRecord] = coll match {
    case Some(c) =>
      // A failed read THROWS, and /debug shows the error: an empty table read as an empty corpus.
      val stripShowtimes = org.bson.Document.parse(
        """{ "$set": { "sourceData": { "$arrayToObject": { "$map": {
          |  "input": { "$objectToArray": { "$ifNull": ["$sourceData", {}] } },
          |  "as": "kv",
          |  "in": { "k": "$$kv.k", "v": { "$arrayToObject": { "$filter": {
          |    "input": { "$objectToArray": "$$kv.v" },
          |    "as": "f",
          |    "cond": { "$ne": ["$$f.k", "showtimes"] } } } } } } } } } }""".stripMargin)
      val pipeline = Seq[Bson](Aggregates.sort(Sorts.ascending("_id")), stripShowtimes)
      val rows = Await.result(c.aggregate[StoredMovieDto](pipeline).batchSize(tools.MongoReplies.Films).toFuture(), 60.seconds)
      // Stitch slots like every other reader. This one reads `movies.sourceData`
      // straight out of an aggregation, so a migrated film — whose slots have moved to
      // `movie_slots` — would otherwise list with NO cinemas at all. Showtimes stay
      // stripped: slots are stored without them, which is exactly what this path wants.
      // A failed slots load can't be recovered from here (the listing has no "partial"
      // shape to return), but it MUST NOT pass silently: every migrated film would
      // render cinema-less and the page would read as a corpus-wide outage. This one is
      // the dev /debug table, so it degrades loudly instead of refusing to render.
      val read     = stitch.allSlotRows()
      val allSlots = read.getOrElse(Map.empty)
      if (read.isEmpty)
        logger.warn("MovieRepository.findAllForListing: movie_slots load failed — every migrated film will " +
          "list with no cinemas. The listing is stale, not the corpus.")
      rows.map(dto => stitch.slotsInto(StoredMovieDto.toDomain(dto, normalizer), allSlots.getOrElse(dto._id, Map.empty)))
    case None => Seq.empty
  }
}
