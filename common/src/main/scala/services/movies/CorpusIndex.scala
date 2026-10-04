package services.movies

import scala.collection.mutable

/**
 * The permanent [[FilmId]] behind each resident key, and back — what the cache asks before every
 * write (is this key some other film's?) and on every change-stream apply (which key is this film
 * under now?), answered without a store round-trip. A retitle moves a key between ids' entries; the
 * id itself never changes — see `FilmId`.
 *
 * Kept AS ROWS ARE WRITTEN by the cache's write funnels, the only writers.
 */
private[movies] final class CorpusIndex {
  private val idByKey = mutable.Map.empty[CacheKey, FilmId]
  private val keyById = mutable.Map.empty[FilmId, CacheKey]

  /** Index film `id` under `key`, replacing whatever that key named before. */
  def put(key: CacheKey, id: FilmId): Unit = synchronized {
    forget(key)
    idByKey.update(key, id); keyById.update(id, key)
  }

  def remove(key: CacheKey): Unit = synchronized(forget(key))

  def idOf(key: CacheKey): Option[FilmId] = synchronized(idByKey.get(key))
  def keyOf(id: FilmId): Option[CacheKey] = synchronized(keyById.get(id))
  def holdsId(id: FilmId): Boolean        = synchronized(keyById.contains(id))

  /** The whole map, for the specs comparing two caches' indexes. */
  private[movies] def snapshot: Map[CacheKey, FilmId] = synchronized(idByKey.toMap)

  // Only this key's own id: `put` on another key may already have claimed the id.
  private def forget(key: CacheKey): Unit =
    idByKey.remove(key).foreach(id => if (keyById.get(id).contains(key)) keyById -= id)
}
