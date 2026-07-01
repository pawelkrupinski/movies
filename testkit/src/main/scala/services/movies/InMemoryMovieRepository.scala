package services.movies

import models.MovieRecord

import scala.collection.mutable

/**
 * In-memory `MovieRepository` for tests — full write-through semantics without
 * needing a real Mongo cluster. Implements the `MovieRepository` trait so it slots
 * in wherever the production cache expects a repository.
 *
 * Indexed by the same normalized documentId formula as the production repository
 * (`TitleNormalizer.sanitize(title)|year`), so case + diacritic + whitespace
 * variants of the same title collapse to one row exactly as they do in Mongo.
 *
 * Every upsert / delete is recorded in `upserts` / `deletes` in order so a
 * test can assert write-through behavior. Tests that don't care simply
 * ignore the buffers.
 */
class InMemoryMovieRepository(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty) extends MovieRepository {

  private val store   = mutable.LinkedHashMap.empty[String, StoredMovieRecord]
  val upserts         = mutable.ListBuffer.empty[(String, Option[Int], MovieRecord)]
  val deletes         = mutable.ListBuffer.empty[(String, Option[Int])]

  // The enrichment cascade writes from several worker pools at once (TMDB,
  // IMDb, and the four *Ratings stages), so every store access is guarded by
  // this monitor. Production's `MongoMovieRepository` gets the same atomicity for
  // free (per-document `$set`/`$unset`, single-document writes are atomic in
  // Mongo); without it here the bare `mutable.Map` loses concurrent updates —
  // e.g. a rating stage's read-modify-write `updateIfPresent` interleaving with
  // a TMDB full-record `upsert` dropped the just-written `metacriticUrl`,
  // making the persisted row order-dependent even though the cache was correct.
  private val lock = new AnyRef

  // Stand-in for Mongo's change stream: any write (in-process or simulated
  // out-of-band) notifies the registered watchers, just like a real change
  // stream fires for every persisted edit — upserts AND deletes. Like the real
  // repository it fans ONE write path out to every registered listener (prod
  // attaches the cache AND the read-model projector); a single-watcher stub
  // would model only one and hide the multiplexing the real cursor now does.
  private val changes = new ChangeStreamFanout[StoredMovieRecord]("InMemoryMovieRepository")
  private def notifyWatcher(t: String, y: Option[Int], e: MovieRecord): Unit =
    changes.dispatchUpsert(StoredMovieRecord.fromStorage(idOf(t, y), e))

  override def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: String => Unit
  ): Option[AutoCloseable] = Some(changes.register(onUpsert, onDelete))

  seed.foreach { case (t, y, e) => store.put(idOf(t, y), StoredMovieRecord(t, y, e)) }

  def enabled: Boolean = true

  // Re-derive `title`/`year` from the stored `_id` + record via
  // `StoredMovieRecord.fromStorage` on read — EXACTLY as `MongoMovieRepository`'s
  // codec does (Mongo persists only `_id` + `sourceData`, dropping the title/
  // year columns). A faithful fake must do the same: returning the verbatim
  // upsert title instead hid two real bugs — title `_id`-drift (a row whose
  // display form moved away from its `_id`) and the `displayTitle`-based settle
  // non-determinism — because the fake's title could never drift and was always
  // deterministic where Mongo's re-derivation isn't. The fake differs from Mongo
  // only at the infra boundary (a HashMap, not BSON), never in read semantics.
  def findAll(): Seq[StoredMovieRecord] = lock.synchronized {
    store.iterator.map { case (id, s) => StoredMovieRecord.fromStorage(id, s.record) }.toSeq
  }

  def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = lock.synchronized {
    store.put(idOf(t, y), StoredMovieRecord(t, y, e))
    upserts.append((t, y, e))
    notifyWatcher(t, y, e)
  }

  def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord): Boolean = lock.synchronized {
    val id = idOf(t, y)
    store.get(id) match {
      case None => false
      case Some(stored) =>
        // Mirror production's `$set`/`$unset` semantics: only the fields where
        // `before` and `after` differ get overwritten; everything else keeps
        // whatever the current store has. Tests for the audit-clobber race
        // depend on this.
        val patch = MovieRecordPatch.diff(before, after)
        // Nothing changed — skip the write + change notification, exactly as
        // MongoMovieRepository does (no pure-`updatedAt` no-op event).
        if (patch.isEmpty) true
        else {
          val merged = patch.applyTo(stored.record)
          store.put(id, StoredMovieRecord(t, y, merged))
          upserts.append((t, y, merged))
          notifyWatcher(t, y, merged)
          true
        }
    }
  }

  def delete(t: String, y: Option[Int]): Unit = lock.synchronized {
    val id = idOf(t, y)
    store.remove(id)
    deletes.append((t, y))
    changes.dispatchDelete(id)
  }

  def deleteById(id: String): Unit = lock.synchronized {
    if (store.remove(id).isDefined) {
      deletes.append((id, None))
      changes.dispatchDelete(id)
    }
  }

  def close(): Unit = ()

  /** Out-of-band edit: drop the `filmwebUrl` + `filmwebRating` for the row.
   *  Used by tests that simulate `FilmwebUrlAudit` mutating Mongo while a
   *  running cache holds stale values. */
  def dropFilmwebUrl(t: String, y: Option[Int]): Unit = {
    val id = idOf(t, y)
    store.get(id).foreach { s =>
      val updated = s.record.copy(filmwebUrl = None, filmwebRating = None)
      store.put(id, s.copy(record = updated))
      notifyWatcher(t, y, updated)
    }
  }

  private def idOf(t: String, y: Option[Int]): String = StoredMovieRecord.idFor(t, y)
}
