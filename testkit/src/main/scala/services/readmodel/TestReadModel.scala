package services.readmodel

import models.MovieRecord
import services.movies.StoredMovieRecord

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
    ReadModelProjection.resolve(StoredMovieRecord(title, year, record))

  /** The `ResolvedRatings` a record projects to — what `_ratingBadges` renders. */
  def ratings(title: String, record: MovieRecord): models.ResolvedRatings =
    ReadModelProjection.ratingsFor(record, title)

  def fromRecords(records: Seq[(String, Option[Int], MovieRecord)]): WebReadModel = {
    val store = new InMemoryReadModelRepository()
    records.foreach { case (title, year, record) =>
      val stored = StoredMovieRecord(title, year, record)
      // Split-aware: a multi-title record fans out into one card per shown title,
      // exactly as the worker's projector publishes it.
      ReadModelProjection.projectAll(stored).foreach { case (movie, screenings) =>
        store.upsertMovie(movie)
        screenings.foreach(store.upsertScreening)
      }
    }
    val readModel = new WebReadModel(store)
    readModel.reload()
    readModel
  }
}
