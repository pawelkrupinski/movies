package services.movies

import models.MovieRecord

import scala.collection.mutable

/**
 * In-memory `MovieRepo` for tests — full write-through semantics without
 * needing a real Mongo cluster. Implements the `MovieRepo` trait so it slots
 * in wherever the production cache expects a repo.
 *
 * Indexed by the same normalized docId formula as the production repo
 * (`MovieService.normalize(title)|year`), so case + diacritic + whitespace
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

  seed.foreach { case (t, y, e) => store.put(idOf(t, y), StoredMovieRecord(t, y, e)) }

  def enabled: Boolean = true

  def findAll(): Seq[StoredMovieRecord] = store.values.toSeq

  def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = {
    store.put(idOf(t, y), StoredMovieRecord(t, y, e))
    upserts.append((t, y, e))
  }

  def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord): Boolean = {
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
        true
    }
  }

  def delete(t: String, y: Option[Int]): Unit = {
    store.remove(idOf(t, y))
    deletes.append((t, y))
  }

  def close(): Unit = ()

  /** Out-of-band edit: drop the `filmwebUrl` + `filmwebRating` for the row.
   *  Used by tests that simulate `FilmwebUrlAudit` mutating Mongo while a
   *  running cache holds stale values. */
  def dropFilmwebUrl(t: String, y: Option[Int]): Unit = {
    val id = idOf(t, y)
    store.get(id).foreach { s =>
      store.put(id, s.copy(record = s.record.copy(filmwebUrl = None, filmwebRating = None)))
    }
  }

  private def idOf(t: String, y: Option[Int]): String =
    s"${MovieService.normalize(t)}|${y.map(_.toString).getOrElse("")}"
}
