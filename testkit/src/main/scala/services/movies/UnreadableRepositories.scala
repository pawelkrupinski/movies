package services.movies

import models.{MovieRecord, Showtime, SourceData}
import tools.contracts.FailsOnPurpose

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
class UnreadableSlotsRepository extends InMemorySlotsRepository with FailsOnPurpose {
  override def findForFilmChecked(filmId: String): (Map[String, SourceData], Boolean) = (Map.empty, false)
  override def findAllChecked(): (Map[String, Map[String, SourceData]], Boolean)      = (Map.empty, false)
}

/** A [[ScreeningsRepository]] whose per-film reads always fail — empty result,
 *  `complete = false` — while every write is delegated to `store` so a spec can seed real
 *  screenings and then fail only the read. Decorates rather than extends the in-memory
 *  store so an integration spec can fail the read in front of the REAL Mongo repository. */
class UnreadableScreeningsRepository(store: ScreeningsRepository = new InMemoryScreeningsRepository)
  extends ScreeningsRepository with FailsOnPurpose {
  def findListedForFilmChecked(filmId: String): (Map[String, ListedShowtimes], Boolean) = (Map.empty, false)
  def findAll(): Map[String, Map[String, Seq[Showtime]]]                        = store.findAll()
  def replaceFilm(filmId: String, slots: Map[String, ListedShowtimes],
                  stored: Option[Map[String, ListedShowtimes]] = None): WriteOutcome    = store.replaceFilm(filmId, slots, stored)
  def upsertSlot(filmId: String, slotKey: String, row: ListedShowtimes): WriteOutcome   = store.upsertSlot(filmId, slotKey, row)
  def deleteSlot(filmId: String, slotKey: String): WriteOutcome                         = store.deleteSlot(filmId, slotKey)
  def deleteFilm(filmId: String): WriteOutcome                                          = store.deleteFilm(filmId)
  def deleteFilms(filmIds: Set[String]): Long                                   = store.deleteFilms(filmIds)
  /** A READ, so it fails like the others: the stranded-row sweep must skip this store,
   *  not clear it on the strength of an id list it never saw. */
  def filmIdsChecked(): (Set[String], Boolean)                                  = (Set.empty, false)
  def rowIdsChecked(): (Set[String], Boolean)                                   = (Set.empty, false)
  def rowWrittenAtChecked(): (Map[String, java.time.Instant], Boolean)          = (Map.empty, false)
  def rowListingKeysChecked(): (Map[String, Option[String]], Boolean)          = (Map.empty, false)
  def rowIdsForListingKeyChecked(listingKey: String): (Set[String], Boolean)    = (Set.empty, false)
  def deleteRows(ids: Set[String]): Long                                        = store.deleteRows(ids)
  // DELEGATED LIKE EVERY OTHER NON-READ. These two were missing until 2026-09-06, so decorating a
  // real Mongo store — which is the case this class documents itself as existing for — silently
  // dropped the screenings change stream and never persisted its resume token: the spec looked
  // like it was exercising the real repository and was quietly running without half of it. Only
  // the per-film READS are meant to fail here.
  //
  // `close` IS LOAD-BEARING, MEASURED, NOT ARGUED. A review reasoned it should go — a decorator
  // tearing down a store its caller owns looks like a lifetime bug — and removing it turned
  // `MovieRepositoryIntegrationSpec`'s screenings-resume case red: a decorator that swallows
  // `close` leaves the underlying cursor open, and a stray cursor on the same collection is
  // exactly what a resume-token test cannot survive. Both delegations stay.
  override def watchApplied(onChange: (String, () => Unit) => Unit,
                            demand:   ChangeStreamDemand = ChangeStreamDemand.unbounded): Option[AutoCloseable] =
    store.watchApplied(onChange, demand)
  override def close(): Unit = store.close()
}

/** A write that failed the way a caught repository exception reports it — for the
 *  doubles below, which have no store to throw from. */
object SimulatedWriteFailure {
  def apply(collection: String, op: String): WriteOutcome =
    WriteOutcome.Failed(collection, op, new RuntimeException(s"simulated $collection.$op failure"))
}

/** A [[SlotsRepository]] whose WRITES always fail. The mirror-image guard: `upsert` may
 *  only drop a film's embedded copy once its slots have actually landed, so a store that
 *  reports every write as failed is what proves the embedded copy is kept. */
class UnwritableSlotsRepository extends InMemorySlotsRepository with FailsOnPurpose {
  override def replaceFilm(filmId: String, slots: Map[String, SourceData],
                           stored: Option[Map[String, SourceData]] = None): WriteOutcome =
    SimulatedWriteFailure(SlotsRepository.Collection, "replaceFilm")
  override def upsertSlot(filmId: String, slotKey: String, slot: SourceData): WriteOutcome =
    SimulatedWriteFailure(SlotsRepository.Collection, "upsertSlot")
  override def deleteSlot(filmId: String, slotKey: String): WriteOutcome =
    SimulatedWriteFailure(SlotsRepository.Collection, "deleteSlot")
}

/** A [[MovieRepository]] whose corpus scan stops short: it delivers `delivered` rows and
 *  then reports the scan INCOMPLETE, exactly as a keyset batch that exhausted its retries
 *  does. `delivered` defaults to none — the shape where a caller sees `films = 0` and must
 *  not mistake it for an empty corpus. */
class IncompleteScanMovieRepository(delivered: Seq[(String, Option[Int], MovieRecord)] = Seq.empty,
                                    titleNormalizer: TitleNormalizer)
  extends InMemoryMovieRepository(delivered, normalizer = titleNormalizer) with FailsOnPurpose {
  override def foreachRecord(f: StoredMovieRecord => Unit): Boolean = { super.foreachRecord(f); false }
}

/** A [[ScreeningsRepository]] whose WRITES fail. A caller that copies rows to a new id and
 *  then deletes the old ones must verify the copy landed; a Mongo transaction would not
 *  save it, since the repository catches the exception and nothing rolls back. */
class UnwritableScreeningsRepository extends InMemoryScreeningsRepository with FailsOnPurpose {
  /** Populate a film's rows, bypassing the write block — so a spec can set up the state a
   *  failed copy is supposed to preserve. */
  def seed(filmId: String, slots: Map[String, ListedShowtimes]): Unit = {
    super.replaceFilm(filmId, slots); ()
  }
  override def replaceFilm(filmId: String, slots: Map[String, ListedShowtimes],
                           stored: Option[Map[String, ListedShowtimes]] = None): WriteOutcome =
    SimulatedWriteFailure(ScreeningsRepository.Collection, "replaceFilm")
  override def upsertSlot(filmId: String, slotKey: String, row: ListedShowtimes): WriteOutcome =
    SimulatedWriteFailure(ScreeningsRepository.Collection, "upsertSlot")
}

/** A [[MovieRepository]] whose BY-ID read fails while the row is genuinely there, and whose
 *  every other operation is real. The shape a Mongo timeout or a slot-read failure produces:
 *  `findByIdChecked` reports `(None, false)`, and a caller that only looks at the `None` sees
 *  a film that does not exist.
 *
 *  `findAll` deliberately keeps working — the corruption this exposes is a WRITE built on a
 *  failed point read, so the spec must still be able to see what the write did. */
class UnreadableByIdMovieRepository(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty,
                                    keyReadsFail: Boolean = true,
                                    titleNormalizer: TitleNormalizer)
  extends InMemoryMovieRepository(seed, normalizer = titleNormalizer) with FailsOnPurpose {
  @volatile var failing: Boolean = true
  override def findByIdChecked(id: FilmId): (Option[StoredMovieRecord], Boolean) =
    if (failing) (None, false) else super.findByIdChecked(id)
  /** `keyReadsFail = false` fails only the by-ID read: the key lookup answers, and a
   *  caller then asks whether a candidate id is free — the read that must not pass. */
  override def findByKeyChecked(key: CacheKey): (Option[StoredMovieRecord], Boolean) =
    if (failing && keyReadsFail) (None, false) else super.findByKeyChecked(key)
}

/** A [[MovieRepository]] whose whole-record `upsert` is DECLINED while `declining` — what
 *  `MongoMovieRepository.upsert` answers when another document already holds the key or
 *  tmdbId ([[WriteOutcome.IdentityHeld]]): nothing threw, and nothing was written. Every other
 *  operation is real; `canMoveFilm` false makes every film move fail, as a degraded Mongo does. */
class IdentityHeldMovieRepository(titleNormalizer: TitleNormalizer)
  extends InMemoryMovieRepository(normalizer = titleNormalizer) with FailsOnPurpose {
  @volatile var declining: Boolean   = true
  @volatile var canMoveFilm: Boolean = true
  override def upsert(film: FilmId, key: CacheKey, e: MovieRecord): WriteOutcome =
    if (declining) WriteOutcome.IdentityHeld else super.upsert(film, key, e)
  override def moveFilm(oldFilm: FilmId, newFilm: FilmId): Boolean =
    oldFilm == newFilm || (canMoveFilm && super.moveFilm(oldFilm, newFilm))
}

/** The exception a write THROWS in the doubles below — the 2026-09-24 incident's shape, a
 *  codec that could not encode the document. */
private[movies] object SimulatedCodecFailure {
  def apply(): Throwable = new org.bson.codecs.configuration.CodecConfigurationException("simulated codec failure")
}

/** A [[MovieRepository]] whose whole-record `upsert` THROWS while `failing` — the incident
 *  shape — routed through [[RepositoryWrite]] exactly as `MongoMovieRepository`'s writes
 *  are, so the failure is logged, counted and reported the production way rather than by a
 *  rule this double would have to restate. */
class ThrowingUpsertMovieRepository(metrics: RepositoryWriteMetrics,
                                    screenings: Option[ScreeningsRepository] = None,
                                    slots: Option[SlotsRepository] = None,
                                    titleNormalizer: TitleNormalizer)
  extends InMemoryMovieRepository(screenings = screenings, slots = slots, normalizer = titleNormalizer) with FailsOnPurpose {
  @volatile var failing: Boolean = true
  private val log = play.api.Logger(getClass)
  override def upsert(film: FilmId, key: CacheKey, e: MovieRecord): WriteOutcome =
    RepositoryWrite.attempt(MovieRepository.Collection, "upsert", s"upsert(${key.cleanTitle})", metrics, log) {
      if (failing) throw SimulatedCodecFailure() else super.upsert(film, key, e)
    }
}

/** A [[SlotsRepository]] whose per-slot and whole-film writes THROW while `failing`, through
 *  [[RepositoryWrite]] like `MongoSlotsRepository`'s. */
class ThrowingSlotsRepository(metrics: RepositoryWriteMetrics) extends InMemorySlotsRepository with FailsOnPurpose {
  @volatile var failing: Boolean = true
  private val log = play.api.Logger(getClass)
  private def write(op: String)(body: => WriteOutcome): WriteOutcome =
    RepositoryWrite.attempt(SlotsRepository.Collection, op, s"slots.$op", metrics, log) {
      if (failing) throw SimulatedCodecFailure() else body
    }
  override def replaceFilm(filmId: String, slots: Map[String, SourceData],
                           stored: Option[Map[String, SourceData]] = None): WriteOutcome =
    write("replaceFilm")(super.replaceFilm(filmId, slots, stored))
  override def upsertSlot(filmId: String, slotKey: String, slot: SourceData): WriteOutcome =
    write("upsertSlot")(super.upsertSlot(filmId, slotKey, slot))
}
