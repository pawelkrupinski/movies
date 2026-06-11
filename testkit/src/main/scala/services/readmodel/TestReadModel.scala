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
  def fromRecords(records: Seq[(String, Option[Int], MovieRecord)]): WebReadModel = {
    val store = new InMemoryReadModelRepo()
    records.foreach { case (title, year, record) =>
      val stored = StoredMovieRecord(title, year, record)
      store.upsertMovie(ReadModelProjection.resolve(stored))
      ReadModelProjection.screenings(stored).foreach(store.upsertScreening)
    }
    val readModel = new WebReadModel(store)
    readModel.reload()
    readModel
  }
}
