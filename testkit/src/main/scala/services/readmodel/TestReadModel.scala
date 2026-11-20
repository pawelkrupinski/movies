package services.readmodel

import java.time.Clock

import scala.concurrent.duration.FiniteDuration

import models.MovieRecord
import models.StoredMovieRecord
import services.movies.SingleCountryNormalizer.titleNormalizer
import tools.SpecClock.given

/**
 * Test helper: project a set of `(title, year, MovieRecord)` rows through the
 * real [[ReadModelProjection]] into an in-memory read model and return a warm
 * [[WebReadModel]] over it — the same shape the web serves from in production,
 * built directly so a controller spec needn't stand up the worker pipeline.
 */
object TestReadModel {
  /** The single `ResolvedMovie` a record projects to — the value the web's
   *  `FilmSchedule.resolved` and `_movieCard` carry. Reuses the production
   *  projection so a view spec exercises the same materialisation as serving. */
  def resolved(title: String, year: Option[Int], record: MovieRecord): models.ResolvedMovie =
    ReadModelProjection.resolve(StoredMovieRecord.synthesised(title, year, record, titleNormalizer), titleNormalizer)

  /** The `ResolvedRatings` a record projects to — what `_ratingBadges` renders. */
  def ratings(title: String, record: MovieRecord): models.ResolvedRatings =
    ReadModelProjection.ratingsFor(record, title)

  /** `clock` stamps the model's change versions (`lastModified`): pass a [[tools.MutableClock]] to move a
   *  conditional GET's validator forward by hand rather than by sleeping past a second. */
  def fromRecords(records: Seq[(String, Option[Int], MovieRecord)], clock: Clock = tools.SpecClock.Pinned): WebReadModel =
    fromRows(records.map { case (title, year, record) => StoredMovieRecord.synthesised(title, year, record, titleNormalizer) }, clock)

  /** From stored rows AS THEY ARE — ids included. A card is keyed by its row's
   *  `FilmId`, so a spec that matches rendered rows back to cache rows must project
   *  the cache's own rows, not rebuild them from (title, year, record). */
  def fromRows(rows: Seq[StoredMovieRecord], clock: Clock = tools.SpecClock.Pinned): WebReadModel = {
    val readModel = new WebReadModel(storeRows(rows), clock = clock)
    readModel.reload()
    readModel
  }

  /** A [[WebReadModel]] over `reader` with the [[ReadModelStreams]] and the [[ReadModelUpkeep]] that keep it
   *  current, wired as the web's root wires them (unscheduled pools), for a spec that starts, ticks or stops it. */
  def kept(reader: ReadModelReader,
           clock: Clock = tools.SpecClock.Pinned,
           servingCountry: Option[models.Country] = None,
           streamMetrics: ReadModelStreamMetrics = ReadModelStreamMetrics.noop,
           driftSettle: ReadModelUpkeep.DriftSettle = ReadModelUpkeep.DefaultDriftSettle,
           ticks: tools.TickRecorder = tools.TickRecorder.Unrecorded,
           sleep: FiniteDuration => Unit = tools.Sleep.Blocking): KeptReadModel = {
    val model   = new WebReadModel(reader, clock = clock, servingCountry = servingCountry)
    val streams = new ReadModelStreams(reader, model, ReadModelUpkeep.DefaultReloadInterval, ReadModelUpkeep.DefaultColdRetryInterval,
      streamMetrics, clock)
    new KeptReadModel(model, streams, new ReadModelUpkeep(model, streams, ReadModelUpkeep.DefaultReloadInterval,
      ReadModelUpkeep.DefaultColdRetryInterval, driftSettle, ticks, tools.ThreadPools.Unmanaged, sleep))
  }

  /** The projected store alone, for a spec that wants its own [[WebReadModel]]
   *  subclass over it (one that counts reads, say). Call `reload()` on it. */
  def store(records: Seq[(String, Option[Int], MovieRecord)]): InMemoryReadModelRepository =
    storeRows(records.map { case (title, year, record) => StoredMovieRecord.synthesised(title, year, record, titleNormalizer) })

  def storeRows(rows: Seq[StoredMovieRecord]): InMemoryReadModelRepository = {
    val store = new InMemoryReadModelRepository()
    rows.foreach { stored =>
      // Split-aware: a multi-title record fans out into one card per shown title,
      // exactly as the worker's projector publishes it.
      ReadModelProjection.projectAll(stored, titleNormalizer).foreach { case (movie, screenings) =>
        store.upsertMovie(movie)
        screenings.foreach(store.upsertScreening)
      }
    }
    store
  }
}

/** A [[WebReadModel]] with what keeps it current ([[TestReadModel.kept]]): the model's reads, the streams'
 *  cold-retry tick and liveness, and the upkeep's lifecycle and backstop tick, on one value. */
final class KeptReadModel(val model: WebReadModel, val streams: ReadModelStreams, val upkeep: ReadModelUpkeep) {
  export model.*
  export streams.{coldRetryTick, streamLive, streamsLive}
  export upkeep.{backstopTick, start, stop}
}
