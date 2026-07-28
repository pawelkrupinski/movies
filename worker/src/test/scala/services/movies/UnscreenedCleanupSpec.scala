package services.movies

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
 * out. The durable slot store is the corroborating witness; a FAILED read of
 * it is not evidence of emptiness either.
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

  /** A slot store whose reads always FAIL — `findForFilmChecked` reports
   *  `readOk = false`, the "I could not tell you" answer that must never be
   *  mistaken for "this film has no cinemas". */
  private class UnreadableSlotsRepository extends SlotsRepository {
    def findForFilmChecked(filmId: String): (Map[String, SourceData], Boolean)   = (Map.empty, false)
    def findAllChecked(): (Map[String, Map[String, SourceData]], Boolean)        = (Map.empty, false)
    def replaceFilm(filmId: String, slots: Map[String, SourceData]): Boolean     = false
    def upsertSlot(filmId: String, slotKey: String, slot: SourceData): Unit      = ()
    def deleteSlot(filmId: String, slotKey: String): Unit                        = ()
    def deleteFilm(filmId: String): Unit                                         = ()
  }

  "removeUnscreened" should "delete rows whose cinemaShowings map is empty" in {
    val withCinema    = mkRecord("tt1", Map(Helios -> cinemaSlot))
    val withoutCinema = mkRecord("tt2", Map.empty)
    val repository = new InMemoryMovieRepository(Seq(
      ("With",    Some(2026), withCinema),
      ("Without", Some(2025), withoutCinema)
    ))
    val cache = new CaffeineMovieCache(repository)
    val cleanup = new UnscreenedCleanup(cache, new InMemorySlotsRepository)

    val removed = cleanup.removeUnscreened()

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
    val repository = new InMemoryMovieRepository(Seq(
      ("Drama",  Some(2026), mkRecord("tt1", Map(Helios -> cinemaSlot))),
      ("Erupcja", Some(2026), mkRecord("tt2", Map(Helios -> cinemaSlot)))
    ))
    val cache = new CaffeineMovieCache(repository)
    val cleanup = new UnscreenedCleanup(cache, new InMemorySlotsRepository)

    cleanup.removeUnscreened()                 shouldBe 0
    cleanup.removeUnscreened()                 shouldBe 0  // second pass: still no-op
    repository.deletes                               shouldBe empty
  }

  it should "count rows correctly when called on an empty cache" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val cleanup = new UnscreenedCleanup(cache, new InMemorySlotsRepository)
    cleanup.removeUnscreened() shouldBe 0
  }

  it should "KEEP a row the durable slot store still holds cinemas for" in {
    // The 2026-07-27 shape: the cache's embedded view is empty (a pass that ran
    // before hydration finished), but `movie_slots` still holds this film's
    // cinemas — so it is still screening and must survive.
    val repository = new InMemoryMovieRepository(Seq(
      ("Filipinana", Some(2026), mkRecord("tt9", Map.empty))
    ))
    val slots = new InMemorySlotsRepository
    slots.upsertSlot(filmId("Filipinana", Some(2026)), "Kino Pod Baranami␟filipinana", cinemaSlot)
    val cache = new CaffeineMovieCache(repository)

    val removed = new UnscreenedCleanup(cache, slots).removeUnscreened()

    removed                                                    shouldBe 0
    cache.get(cache.keyOf("Filipinana", Some(2026)))     should not be empty
    repository.deletes                                         shouldBe empty
  }

  it should "KEEP a row when the slot-store read FAILED rather than came back empty" in {
    // A failed read is not data: it cannot distinguish "no cinemas" from
    // "Mongo did not answer", so it must never authorise a delete.
    val repository = new InMemoryMovieRepository(Seq(
      ("Clarissa", Some(2026), mkRecord("tt8", Map.empty))
    ))
    val cache = new CaffeineMovieCache(repository)

    val removed = new UnscreenedCleanup(cache, new UnreadableSlotsRepository).removeUnscreened()

    removed                                                  shouldBe 0
    cache.get(cache.keyOf("Clarissa", Some(2026)))     should not be empty
    repository.deletes                                       shouldBe empty
  }

  it should "still delete a row both the cache AND a healthy slot store call empty" in {
    // Genuine expiry — the one case that legitimately removes a row.
    val repository = new InMemoryMovieRepository(Seq(
      ("Dkfzakopeta", Some(2026), mkRecord("tt7", Map.empty))
    ))
    val cache = new CaffeineMovieCache(repository)

    val removed = new UnscreenedCleanup(cache, new InMemorySlotsRepository).removeUnscreened()

    removed                                                     shouldBe 1
    cache.get(cache.keyOf("Dkfzakopeta", Some(2026)))     shouldBe None
  }
}
