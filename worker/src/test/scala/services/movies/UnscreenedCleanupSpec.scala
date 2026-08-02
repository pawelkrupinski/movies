package services.movies

import services.movies.SingleCountryNormalizer.given

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `UnscreenedCleanup` drops rows whose `cinemaShowings` map is empty — i.e.
 * the film stopped showing at every cinema since the last scrape tick's
 * prune step removed each cinema's slot. Reverses the older "keep forever"
 * policy: re-screenings will pay the cost of a fresh TMDB resolution rather
 * than the DB carrying dead enrichment data indefinitely.
 *
 * The in-memory view alone is NOT sufficient evidence of that, which is what
 * the corroboration specs below pin: on 2026-07-27 a pass fired 20s after a
 * restart, read an empty `cinemaData` off a cache that had not finished
 * hydrating, and deleted 19 still-playing arthouse features — each of whose
 * `movie_slots` rows the cascade then cleared, logging `slots=3` on the way
 * out.
 *
 * The corroborating witness is the durable RECORD (`MovieRepository.findByIdChecked`),
 * not `movie_slots` alone. Mid-migration a film's cinemas live in EITHER store, and
 * asking only the slot store convicts every film that has not been rewritten since
 * the split landed — its cinemas are in the `movies` document's embedded `sourceData`,
 * a collection it was never written to. A FAILED read is not evidence of emptiness
 * either.
 */
class UnscreenedCleanupSpec extends AnyFlatSpec with Matchers {

  private val cinemaSlot = SourceData()

  private def mkRecord(imdbId: String, cinemas: Map[models.Cinema, SourceData]): MovieRecord =
    MovieRecord(
      imdbId = Some(imdbId),
      data   = cinemas.map { case (c, sd) => (c: Source) -> sd }
    )

  /** The `_id` the cleanup's delete would cascade against — the same formula
   *  `MovieCache.invalidate` → `MovieRepository.delete` keys the row by. */
  private def filmId(title: String, year: Option[Int]): String =
    StoredMovieRecord.idFor(title, year)

  /** Production's storage shape: showtimes in `screenings`, per-cinema slots in
   *  `movie_slots`. The corroborating read has to stitch both back, so a fake wired
   *  without them cannot express what the guard is actually deciding. */
  private def splitRepository(seed: Seq[(String, Option[Int], MovieRecord)] = Seq.empty) = {
    val slots = new InMemorySlotsRepository
    val repository = new InMemoryMovieRepository(seed,
      screenings = Some(new InMemoryScreeningsRepository), slots = Some(slots))
    (repository, slots)
  }

  /** A repository whose per-film read always FAILS — `findByIdChecked` reports
   *  `readOk = false`, the "I could not tell you" answer that must never be
   *  mistaken for "this film has no cinemas". */
  private class UnreadableMovieRepository(delegate: InMemoryMovieRepository) extends MovieRepository {
    override def findByIdChecked(id: String): (Option[StoredMovieRecord], Boolean) = (None, false)
    def enabled: Boolean                                              = true
    def findAll(): Seq[StoredMovieRecord]                             = delegate.findAll()
    def upsert(t: String, y: Option[Int], e: MovieRecord): Unit       = delegate.upsert(t, y, e)
    def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord): Boolean =
      delegate.updateIfPresent(t, y, before, after)
    def delete(t: String, y: Option[Int]): Unit                       = delegate.delete(t, y)
    def deleteById(id: String): Unit                                  = delegate.deleteById(id)
    def close(): Unit                                                 = ()
  }

  "removeUnscreened" should "delete rows whose cinemaShowings map is empty" in {
    val withCinema    = mkRecord("tt1", Map(Helios -> cinemaSlot))
    val withoutCinema = mkRecord("tt2", Map.empty)
    val (repository, _) = splitRepository(Seq(
      ("With",    Some(2026), withCinema),
      ("Without", Some(2025), withoutCinema)
    ))
    val cache = new CaffeineMovieCache(repository)

    val removed = new UnscreenedCleanup(cache, repository).removeUnscreened()

    removed                                              shouldBe 1
    cache.get(cache.keyOf("With",    Some(2026)))        should not be empty
    cache.get(cache.keyOf("Without", Some(2025)))        shouldBe None
    // The repository re-derives the display title on read (as Mongo does); these
    // title-less records collapse to their sanitized _id prefix ("without"),
    // which `displayTitle` re-cases to "Without". The cleanup deleting the
    // unscreened row is what this pins.
    repository.deletes                                         should contain (("Without", Some(2025)))
  }

  it should "be idempotent — no-op when every row has at least one cinema slot" in {
    val (repository, _) = splitRepository(Seq(
      ("Drama",   Some(2026), mkRecord("tt1", Map(Helios -> cinemaSlot))),
      ("Erupcja", Some(2026), mkRecord("tt2", Map(Helios -> cinemaSlot)))
    ))
    val cache   = new CaffeineMovieCache(repository)
    val cleanup = new UnscreenedCleanup(cache, repository)

    cleanup.removeUnscreened()                 shouldBe 0
    cleanup.removeUnscreened()                 shouldBe 0  // second pass: still no-op
    repository.deletes                               shouldBe empty
  }

  it should "count rows correctly when called on an empty cache" in {
    val (repository, _) = splitRepository()
    val cache = new CaffeineMovieCache(repository)
    new UnscreenedCleanup(cache, repository).removeUnscreened() shouldBe 0
  }

  it should "KEEP a MIGRATED row whose cinemas the durable store still holds in movie_slots" in {
    // The 2026-07-27 shape: the cache's view is empty (a pass that ran before hydration
    // finished), but the film's slots are sitting in `movie_slots` — so it is still
    // screening and must survive. The slot lands AFTER the cache hydrates, which is what
    // makes the two views disagree.
    val (repository, slots) = splitRepository(Seq(("Filipinana", Some(2026), mkRecord("tt9", Map.empty))))
    val cache = new CaffeineMovieCache(repository)
    slots.upsertSlot(filmId("Filipinana", Some(2026)), Helios.displayName, cinemaSlot)

    val removed = new UnscreenedCleanup(cache, repository).removeUnscreened()

    removed                                                    shouldBe 0
    cache.get(cache.keyOf("Filipinana", Some(2026)))     should not be empty
    repository.deletes                                         shouldBe empty
  }

  it should "KEEP an UNMIGRATED row whose cinemas are still embedded in the movies document" in {
    // Same disagreement, other store. This film has not been rewritten since the slots
    // split landed, so `movie_slots` holds NOTHING for it and its cinemas are still in the
    // `movies` document's own `sourceData`. A witness that asks only the slot store gets
    // an honest, successful, EMPTY answer and convicts a film that is playing tonight —
    // the guard has to read the union, which is what every serving reader already reads.
    val (repository, _) = splitRepository(Seq(("Clarissa", Some(2026), mkRecord("tt8", Map.empty))))
    val cache = new CaffeineMovieCache(repository)
    repository.putEmbeddedOutOfBand("Clarissa", Some(2026), mkRecord("tt8", Map(Helios -> cinemaSlot)))

    val removed = new UnscreenedCleanup(cache, repository).removeUnscreened()

    removed                                                  shouldBe 0
    cache.get(cache.keyOf("Clarissa", Some(2026)))     should not be empty
    repository.deletes                                       shouldBe empty
  }

  it should "KEEP a row when the durable read FAILED rather than came back empty" in {
    // A failed read is not data: it cannot distinguish "no cinemas" from
    // "Mongo did not answer", so it must never authorise a delete.
    val (delegate, _) = splitRepository(Seq(("Blogoslawieni", Some(2026), mkRecord("tt7", Map.empty))))
    val cache = new CaffeineMovieCache(delegate)

    val removed = new UnscreenedCleanup(cache, new UnreadableMovieRepository(delegate)).removeUnscreened()

    removed                                                      shouldBe 0
    cache.get(cache.keyOf("Blogoslawieni", Some(2026)))    should not be empty
    delegate.deletes                                             shouldBe empty
  }

  it should "still delete a row both the cache AND a healthy durable read call empty" in {
    // Genuine expiry — the one case that legitimately removes a row.
    val (repository, _) = splitRepository(Seq(("Dkfzakopeta", Some(2026), mkRecord("tt6", Map.empty))))
    val cache = new CaffeineMovieCache(repository)

    val removed = new UnscreenedCleanup(cache, repository).removeUnscreened()

    removed                                                     shouldBe 1
    cache.get(cache.keyOf("Dkfzakopeta", Some(2026)))     shouldBe None
  }
}
