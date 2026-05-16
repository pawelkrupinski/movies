package services.movies

import models.MovieRecord

import scala.collection.mutable

/**
 * In-memory MovieRepo for tests — full write-through semantics without
 * needing a real Mongo cluster. Subclasses MovieRepo so it slots in wherever
 * the production cache expects a repo, and overrides every method that
 * touches the wire.
 *
 * Indexed by the same normalized docId formula as the production repo, so
 * case + diacritic + whitespace variants of the same title collapse to one
 * row exactly as they do in Mongo. The first non-overridden behavior — the
 * superclass's Mongo init in the constructor — is harmless because `enabled`
 * is forced to true and the wire methods are all overridden; the init still
 * runs and logs a "MovieRepo init failed" line when no MONGODB_URI is set,
 * but it doesn't affect correctness.
 *
 * Every upsert / delete is recorded in `upserts` / `deletes` in order so a
 * test can assert write-through behavior. Tests that don't care simply
 * ignore the buffers.
 */
class InMemoryMovieRepo(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty) extends MovieRepo {

  private val store   = mutable.LinkedHashMap.empty[String, (String, Option[Int], MovieRecord)]
  val upserts         = mutable.ListBuffer.empty[(String, Option[Int], MovieRecord)]
  val deletes         = mutable.ListBuffer.empty[(String, Option[Int])]

  seed.foreach { case (t, y, e) => store.put(idOf(t, y), (t, y, e)) }

  override def enabled: Boolean = true

  override def findAll(): Seq[(String, Option[Int], MovieRecord)] = store.values.toSeq

  override def upsert(t: String, y: Option[Int], e: MovieRecord): Unit = {
    store.put(idOf(t, y), (t, y, e))
    upserts.append((t, y, e))
  }

  override def updateIfPresent(t: String, y: Option[Int], e: MovieRecord): Boolean = {
    val id = idOf(t, y)
    if (store.contains(id)) {
      store.put(id, (t, y, e))
      upserts.append((t, y, e))
      true
    } else false
  }

  override def delete(t: String, y: Option[Int]): Unit = {
    store.remove(idOf(t, y))
    deletes.append((t, y))
  }

  override def close(): Unit = ()

  private def idOf(t: String, y: Option[Int]): String =
    s"${MovieService.normalize(t)}|${y.map(_.toString).getOrElse("")}"
}
