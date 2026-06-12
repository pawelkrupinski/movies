package services.movies

import models.MovieRecord

import scala.collection.mutable

/**
 * In-memory `MovieRepo` for tests — full write-through semantics without
 * needing a real Mongo cluster. Implements the `MovieRepo` trait so it slots
 * in wherever the production cache expects a repo.
 *
 * Indexed by the same normalized docId formula as the production repo
 * (`TitleNormalizer.sanitize(title)|year`), so case + diacritic + whitespace
 * variants of the same title collapse to one row exactly as they do in Mongo.
 *
 * Every upsert / delete is recorded in `upserts` / `deletes` in order so a
 * test can assert write-through behavior. Tests that don't care simply
 * ignore the buffers.
 */
class InMemoryMovieRepo(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty) extends MovieRepo {

  private val store   = mutable.LinkedHashMap.empty[String, StoredMovieRecord]
  val upserts         = mutable.ListBuffer.empty[(String, Option[Int], MovieRecord)]
  val deletes         = mutable.ListBuffer.empty[(String, Option[Int])]

  // The enrichment cascade writes from several worker pools at once (TMDB,
  // IMDb, and the four *Ratings stages), so every store access is guarded by
  // this monitor. Production's `MongoMovieRepo` gets the same atomicity for
  // free (per-document `$set`/`$unset`, single-document writes are atomic in
  // Mongo); without it here the bare `mutable.Map` loses concurrent updates —
  // e.g. a rating stage's read-modify-write `updateIfPresent` interleaving with
  // a TMDB full-record `upsert` dropped the just-written `metacriticUrl`,
  // making the persisted row order-dependent even though the cache was correct.
  private val lock = new AnyRef

  // Stand-in for Mongo's change stream: any write (in-process or simulated
  // out-of-band) notifies the registered watchers, just like a real change
  // stream fires for every persisted edit — upserts AND deletes.
  @volatile private var upsertWatcher: Option[StoredMovieRecord => Unit] = None
  @volatile private var deleteWatcher: Option[String => Unit]            = None
  private def notifyWatcher(t: String, y: Option[Int], e: MovieRecord): Unit =
    upsertWatcher.foreach(_(StoredMovieRecord(t, y, e)))

  override def watchChanges(
    onUpsert: StoredMovieRecord => Unit,
    onDelete: String => Unit
  ): Option[AutoCloseable] = {
    upsertWatcher = Some(onUpsert)
    deleteWatcher = Some(onDelete)
    Some(new AutoCloseable { override def close(): Unit = { upsertWatcher = None; deleteWatcher = None } })
  }

  seed.foreach { case (t, y, e) => store.put(idOf(t, y), StoredMovieRecord(t, y, e)) }

  def enabled: Boolean = true

  // This fake stores the full `StoredMovieRecord` and returns its title
  // verbatim — it has no lossy BSON `_id` to recover from, unlike the Mongo
  // repo, whose codec drops the `title`/`year` columns and re-derives them via
  // `StoredMovieRecord.fromStorage`. For realistic rows (the record carries its
  // title in a cinema slot) the two agree; the fake simply doesn't need the
  // derivation step.
  def findAll(): Seq[StoredMovieRecord] = lock.synchronized { store.values.toSeq }

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
        val patch  = MovieRecordPatch.diff(before, after)
        val merged = patch.applyTo(stored.record)
        store.put(id, StoredMovieRecord(t, y, merged))
        upserts.append((t, y, merged))
        notifyWatcher(t, y, merged)
        true
    }
  }

  def delete(t: String, y: Option[Int]): Unit = lock.synchronized {
    val id = idOf(t, y)
    store.remove(id)
    deletes.append((t, y))
    deleteWatcher.foreach(_(id))
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
