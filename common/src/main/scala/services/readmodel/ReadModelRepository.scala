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
 * A position in the read model's change history, taken with
 * [[ReadModelReader.streamCheckpoint]] and handed back to `watchMovies` /
 * `watchScreenings` to replay every change made since. Opaque: its value means
 * something only to the store that issued it (Mongo's cluster time).
 */
final case class StreamCheckpoint(value: Long)

/**
 * A screening's identity without its payload — `_id` to delete it, `filmId` to
 * test against the live source. The reconcile prune reads only these two fields,
 * so [[ReadModelReader.findAllScreeningRefs]] projects them server-side instead of
 * decoding the whole `web_screenings` corpus (6k+ documents) onto the heap.
 */
final case class ScreeningRef(_id: String, filmId: String)

/** What the share-card janitor needs of one `web_movies` document: the card it points at — so it
 *  can tell which films' files are still referenced without decoding the whole film. */
final case class ShareCardRef(filmId: String, shareCard: Option[String])

/** One card as the read model holds it: its `web_movies` document (None when there is none) and
 *  every `web_screenings` row filed under its id. */
final case class StoredCard(movie: Option[ResolvedMovie], screenings: Seq[CityScreening])

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
 * A watch opened with `from = None` starts NOW; with a [[StreamCheckpoint]] from
 * [[streamCheckpoint]] it first replays every change made since that point. A
 * consumer that hydrates with a full read and then watches must take the
 * checkpoint BEFORE the read: a write landing between the read and the watch is
 * otherwise in neither.
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

  /** Like [[findAllMovies]] but says whether the READ was complete: an incomplete scan
   *  returns empty, and "no films" is a different fact from "could not read the films".
   *  The in-memory store cannot fail, so the default reports `true`. */
  def findAllMoviesChecked(): (Seq[ResolvedMovie], Boolean) = (findAllMovies(), true)
  def findAllScreenings(): Seq[CityScreening]

  /** Just the `_id`s of every read-model movie — the projector's reconcile prune
   *  needs only the id set to spot orphaned films, never the full `ResolvedMovie`
   *  payload. Default derives from [[findAllMovies]] (fine for the in-memory
   *  store); the Mongo store projects `{_id}` so the worker's 30-min reconcile
   *  never decodes the whole `web_movies` collection just to diff ids. */
  def findAllMovieIds(): Seq[String] = findAllMovieIdsChecked()._1

  /** Like [[findAllMovieIds]] but says whether the READ was complete. An incomplete
   *  keyset scan returns empty, and "no cards" is a very different fact from "could not
   *  read the cards": a heal that trusted the empty answer would re-project the whole
   *  corpus. The in-memory store cannot fail, so the default reports `true`. */
  def findAllMovieIdsChecked(): (Seq[String], Boolean) = (findAllMovies().map(_._id), true)

  /** The (`_id`, `filmId`) of every read-model screening — the prune deletes a
   *  screening whose `filmId` is no longer live and reads no other field. Default
   *  derives from [[findAllScreenings]]; the Mongo store projects `{_id, filmId}`
   *  so 6k+ screening documents collapse to a few id strings instead of full
   *  `CityScreening` payloads on the heap. */
  def findAllScreeningRefs(): Seq[ScreeningRef] = findAllScreeningRefsChecked()._1

  /** Like [[findAllScreeningRefs]] but says whether the READ was complete — the venue heals'
   *  counterpart of [[findAllMovieIdsChecked]]: an incomplete scan returns empty, which a heal
   *  must not read as "no venue has a row". The in-memory store cannot fail. */
  def findAllScreeningRefsChecked(): (Seq[ScreeningRef], Boolean) =
    (findAllScreenings().map(s => ScreeningRef(s._id, s.filmId)), true)

  /** Every document's [[ShareCardRef]], with the same completeness flag as
   *  [[findAllMovieIdsChecked]]: an incomplete read must prune nothing, since a card missing
   *  from the answer would read as unreferenced. The default decodes whole documents; the
   *  Mongo reader projects the two fields server-side. */
  def findAllShareCardRefsChecked(): (Seq[ShareCardRef], Boolean) =
    (findAllMovies().map(m => ShareCardRef(m._id, m.shareCard)), true)

  /** One card's stored documents — what the worker's content audit compares with a fresh
   *  projection — or None when the read FAILED, which is not the fact "no such card". The default
   *  derives from the whole-collection reads (fine for the in-memory store); the Mongo reader
   *  makes two reads by `_id`. */
  def findCard(id: String): Option[StoredCard] =
    Some(StoredCard(findAllMovies().find(_._id == id), findAllScreenings().filter(_.filmId == id)))

  def countMovies(): Long
  def countScreenings(): Long
  /** Where the change history stands now — `None` when the store cannot replay from a
   *  point (disabled, a standalone Mongo), in which case a watch can only start now. */
  def streamCheckpoint(): Option[StreamCheckpoint]
  def watchMovies(onUpsert: ResolvedMovie => Unit, onDelete: String => Unit, from: Option[StreamCheckpoint]): Option[StreamSubscription]
  def watchScreenings(onUpsert: CityScreening => Unit, onDelete: String => Unit, from: Option[StreamCheckpoint]): Option[StreamSubscription]
  def close(): Unit
}

/**
 * Write side of the denormalised read model — what the **worker's projector**
 * depends on. Each write is keyed by the document's own `_id`, and a write that
 * FAILED THROWS: the projector remembers each card and venue it wrote and skips an
 * identical projection next time, so a failure swallowed here left it remembering a
 * document the store never took — skipped by every later projection, written only by
 * the sweep's heal or content slice. Thrown, the projector forgets it and the row's
 * next change retries it. (`MovieRepository` answers a `WriteOutcome` instead; the
 * projector's only need is "did it land", which a throw already carries.)
 */
trait ReadModelWriter {
  def enabled: Boolean
  def upsertMovie(m: ResolvedMovie): Unit
  def deleteMovie(id: String): Unit
  def upsertScreening(s: CityScreening): Unit
  def deleteScreening(id: String): Unit
  def close(): Unit
}
