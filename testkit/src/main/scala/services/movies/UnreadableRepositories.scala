package services.movies

import models.{MovieRecord, SourceData}

/**
 * Test doubles for the ONE thing every "checked" read contract exists to express: a read
 * that did not see the whole collection.
 *
 * The bugs these pin are all the same shape — a failed read returns an empty collection,
 * a caller reads that emptiness as "there is nothing there", and acts destructively on it:
 * a film served with no cinemas, a verifier crying corpus-wide FATAL, a reaper deleting
 * the rows it simply failed to read. The only way to test that a caller HONOURS the
 * completeness flag is to hand it a store that reports false, so both doubles live here
 * rather than being re-declared per spec.
 */

/** A [[SlotsRepository]] whose reads always fail — empty result, `complete = false`.
 *  Writes still land, so a spec can seed state and then fail only the read. */
class UnreadableSlotsRepository extends InMemorySlotsRepository {
  override def findForFilmChecked(filmId: String): (Map[String, SourceData], Boolean) = (Map.empty, false)
  override def findAllChecked(): (Map[String, Map[String, SourceData]], Boolean)      = (Map.empty, false)
}

/** A [[SlotsRepository]] whose WRITES always fail. The mirror-image guard: `upsert` may
 *  only drop a film's embedded copy once its slots have actually landed, so a store that
 *  reports every write as failed is what proves the embedded copy is kept. */
class UnwritableSlotsRepository extends InMemorySlotsRepository {
  override def replaceFilm(filmId: String, slots: Map[String, SourceData]): Boolean = false
  override def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit   = ()
  override def deleteSlot(filmId: String, slotKey: String): Unit                     = ()
}

/** A [[MovieRepository]] whose corpus scan stops short: it delivers `delivered` rows and
 *  then reports the scan INCOMPLETE, exactly as a keyset batch that exhausted its retries
 *  does. `delivered` defaults to none — the shape where a caller sees `films = 0` and must
 *  not mistake it for an empty corpus. */
class IncompleteScanMovieRepository(delivered: Seq[(String, Option[Int], MovieRecord)] = Seq.empty)
  extends InMemoryMovieRepository(delivered) {
  override def foreachRecord(f: StoredMovieRecord => Unit): Boolean = { super.foreachRecord(f); false }
}
