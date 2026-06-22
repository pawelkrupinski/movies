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
 * A screening's identity without its payload — `_id` to delete it, `filmId` to
 * test against the live source. The reconcile prune reads only these two fields,
 * so [[ReadModelReader.findAllScreeningRefs]] projects them server-side instead of
 * decoding the whole `web_screenings` corpus (6k+ documents) onto the heap.
 */
final case class ScreeningRef(_id: String, filmId: String)

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

  /** Just the `_id`s of every read-model movie — the projector's reconcile prune
   *  needs only the id set to spot orphaned films, never the full `ResolvedMovie`
   *  payload. Default derives from [[findAllMovies]] (fine for the in-memory
   *  store); the Mongo store projects `{_id}` so the worker's 30-min reconcile
   *  never decodes the whole `web_movies` collection just to diff ids. */
  def findAllMovieIds(): Seq[String] = findAllMovies().map(_._id)

  /** The (`_id`, `filmId`) of every read-model screening — the prune deletes a
   *  screening whose `filmId` is no longer live and reads no other field. Default
   *  derives from [[findAllScreenings]]; the Mongo store projects `{_id, filmId}`
   *  so 6k+ screening documents collapse to a few id strings instead of full
   *  `CityScreening` payloads on the heap. */
  def findAllScreeningRefs(): Seq[ScreeningRef] = findAllScreenings().map(s => ScreeningRef(s._id, s.filmId))

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
