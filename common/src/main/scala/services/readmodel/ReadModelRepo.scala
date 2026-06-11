package services.readmodel

import models.{CityScreening, ResolvedMovie}

/**
 * Read side of the denormalised read model — what the **web** depends on.
 * Segregated from [[ReadModelWriter]] (ISP): the serving app never writes, so
 * it compiles against reads + change-stream watches only.
 *
 * `watchMovies` / `watchScreenings` deliver each insert / update / replace as
 * an `onUpsert` and each delete as an `onDelete(id)` — so a consumer cache can
 * apply both incrementally instead of reloading. Best-effort: a store that
 * can't stream (disabled, or a standalone Mongo) returns `None` and the caller
 * falls back to a periodic full reload.
 */
trait ReadModelReader {
  def enabled: Boolean
  def findAllMovies(): Seq[ResolvedMovie]
  def findAllScreenings(): Seq[CityScreening]
  def watchMovies(onUpsert: ResolvedMovie => Unit, onDelete: String => Unit): Option[AutoCloseable]
  def watchScreenings(onUpsert: CityScreening => Unit, onDelete: String => Unit): Option[AutoCloseable]
  def close(): Unit
}

/**
 * Write side of the denormalised read model — what the **worker's projector**
 * depends on. Each write is keyed by the document's own `_id`, best-effort
 * (failures logged, never thrown), mirroring `MovieRepo`'s contract.
 */
trait ReadModelWriter {
  def enabled: Boolean
  def upsertMovie(m: ResolvedMovie): Unit
  def deleteMovie(id: String): Unit
  def upsertScreening(s: CityScreening): Unit
  def deleteScreening(id: String): Unit
  def close(): Unit
}
