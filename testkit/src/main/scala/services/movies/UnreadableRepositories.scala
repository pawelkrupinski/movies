package services.movies

import models.{MovieRecord, Showtime, SourceData}

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

/** A [[ScreeningsRepository]] whose per-film reads always fail — empty result,
 *  `complete = false` — while every write is delegated to `store` so a spec can seed real
 *  screenings and then fail only the read. Decorates rather than extends the in-memory
 *  store so an integration spec can fail the read in front of the REAL Mongo repository. */
class UnreadableScreeningsRepository(store: ScreeningsRepository = new InMemoryScreeningsRepository)
  extends ScreeningsRepository {
  def findForFilmChecked(filmId: String): (Map[String, Seq[Showtime]], Boolean) = (Map.empty, false)
  def findAll(): Map[String, Map[String, Seq[Showtime]]]                        = store.findAll()
  def replaceFilm(filmId: String, slots: Map[String, Seq[Showtime]],
                  stored: Option[Map[String, Seq[Showtime]]] = None): Unit      = store.replaceFilm(filmId, slots, stored)
  def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[Showtime]): Unit = store.upsertSlot(filmId, slotKey, showtimes)
  def deleteSlot(filmId: String, slotKey: String): Unit                         = store.deleteSlot(filmId, slotKey)
  def deleteFilm(filmId: String): Unit                                          = store.deleteFilm(filmId)
}

/** A [[SlotsRepository]] whose WRITES always fail. The mirror-image guard: `upsert` may
 *  only drop a film's embedded copy once its slots have actually landed, so a store that
 *  reports every write as failed is what proves the embedded copy is kept. */
class UnwritableSlotsRepository extends InMemorySlotsRepository {
  override def replaceFilm(filmId: String, slots: Map[String, SourceData],
                           stored: Option[Map[String, SourceData]] = None): Boolean = false
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

/** A [[ScreeningsRepository]] whose WRITES silently fail — which is the real shape here,
 *  because `replaceFilm` returns `Unit` and swallows its own errors. A caller that copies
 *  rows to a new id and then deletes the old ones must verify the copy landed; a Mongo
 *  transaction would not save it, since nothing throws and nothing rolls back. */
class UnwritableScreeningsRepository extends InMemoryScreeningsRepository {
  /** Populate a film's rows, bypassing the write block — so a spec can set up the state a
   *  failed copy is supposed to preserve. */
  def seed(filmId: String, slots: Map[String, Seq[models.Showtime]]): Unit =
    super.replaceFilm(filmId, slots)
  override def replaceFilm(filmId: String, slots: Map[String, Seq[models.Showtime]],
                           stored: Option[Map[String, Seq[models.Showtime]]] = None): Unit = ()
  override def upsertSlot(filmId: String, slotKey: String, showtimes: Seq[models.Showtime]): Unit = ()
}

/** A [[MovieRepository]] whose BY-ID read fails while the row is genuinely there, and whose
 *  every other operation is real. The shape a Mongo timeout or a slot-read failure produces:
 *  `findByIdChecked` reports `(None, false)`, and a caller that only looks at the `None` sees
 *  a film that does not exist.
 *
 *  `findAll` deliberately keeps working — the corruption this exposes is a WRITE built on a
 *  failed point read, so the spec must still be able to see what the write did. */
class UnreadableByIdMovieRepository(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty)
  extends InMemoryMovieRepository(seed) {
  @volatile var failing: Boolean = true
  override def findByIdChecked(id: String): (Option[StoredMovieRecord], Boolean) =
    if (failing) (None, false) else super.findByIdChecked(id)
}
