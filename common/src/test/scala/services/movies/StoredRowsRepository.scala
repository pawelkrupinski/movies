package services.movies

import models.MovieRecord

import scala.collection.mutable

/** A [[MovieRepository]] that answers `findAll` with the rows it was given and RECORDS
 *  writes without applying them — for specs about what a cache or a fold does with a
 *  corpus it has been handed (a hydrate over colliding documents, a fold plan), where the
 *  write-through of [[InMemoryMovieRepository]] would fold the seed into one row per key
 *  and hide the very shape under test. `rows` is by-name so a spec can hand over a
 *  sequence that changes between calls (a boot hydrate that finds Mongo empty once). */
class StoredRowsRepository(rows: => Seq[StoredMovieRecord],
                           override val normalizer: TitleNormalizer = TitleNormalizer.deployment) extends MovieRepository with KeyAddressedMovieWrites {
  val upserts = mutable.ListBuffer.empty[(FilmId, String, MovieRecord)]
  def enabled: Boolean = true
  def findAll(): Seq[StoredMovieRecord] = rows
  def delete(id: FilmId): Unit = ()
  def upsert(id: FilmId, key: CacheKey, e: MovieRecord): Unit = { upserts += ((id, key.cleanTitle, e)); () }
  def updateIfPresent(id: FilmId, key: CacheKey, before: MovieRecord, after: MovieRecord): Boolean = false
  override def close(): Unit = ()
}
