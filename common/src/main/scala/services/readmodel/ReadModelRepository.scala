package services.readmodel

import models.{CityScreening, ResolvedMovie}

/**
 * A live change-stream subscription. `live` reports whether the stream is still
 * delivering: it flips to `false` once the stream has terminally ended (the
 * driver gave up resuming). A consumer watching `live` knows when its in-memory
 * view may have drifted and a full reload is owed.
 */
trait StreamSubscription extends AutoCloseable {
  def live: Boolean
}

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
 *
 * `countMovies` / `countScreenings` are the cheap integrity probe: a server-side
 * document count (no payload decode) the consumer's backstop compares against
 * its in-memory size to detect drift without re-reading the whole corpus. A
 * negative result means the count is unavailable (disabled / failed) — treat it
 * as "unknown" and reload.
 */
trait ReadModelReader {
  def enabled: Boolean
  def findAllMovies(): Seq[ResolvedMovie]
  def findAllScreenings(): Seq[CityScreening]
  def countMovies(): Long
  def countScreenings(): Long
  def watchMovies(onUpsert: ResolvedMovie => Unit, onDelete: String => Unit): Option[StreamSubscription]
  def watchScreenings(onUpsert: CityScreening => Unit, onDelete: String => Unit): Option[StreamSubscription]
  def close(): Unit
}

/**
 * Write side of the denormalised read model — what the **worker's projector**
 * depends on. Each write is keyed by the document's own `_id`, best-effort
 * (failures logged, never thrown), mirroring `MovieRepository`'s contract.
 */
trait ReadModelWriter {
  def enabled: Boolean
  def upsertMovie(m: ResolvedMovie): Unit
  def deleteMovie(id: String): Unit
  def upsertScreening(s: CityScreening): Unit
  def deleteScreening(id: String): Unit
  def close(): Unit
}
