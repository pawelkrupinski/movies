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
 */
class UnscreenedCleanupSpec extends AnyFlatSpec with Matchers {

  private val cinemaSlot = SourceData()

  private def mkRecord(imdbId: String, cinemas: Map[models.Cinema, SourceData]): MovieRecord =
    MovieRecord(
      imdbId = Some(imdbId),
      data   = cinemas.map { case (c, sd) => (c: Source) -> sd }
    )

  "removeUnscreened" should "delete rows whose cinemaShowings map is empty" in {
    val withCinema    = mkRecord("tt1", Map(Helios -> cinemaSlot))
    val withoutCinema = mkRecord("tt2", Map.empty)
    val repository = new InMemoryMovieRepository(Seq(
      ("With",    Some(2026), withCinema),
      ("Without", Some(2025), withoutCinema)
    ))
    val cache = new CaffeineMovieCache(repository)
    val cleanup = new UnscreenedCleanup(cache)

    val removed = cleanup.removeUnscreened()

    removed                                              shouldBe 1
    cache.get(cache.keyOf("With",    Some(2026)))        should not be empty
    cache.get(cache.keyOf("Without", Some(2025)))        shouldBe None
    // The repository re-derives the display title on read (as Mongo does); these
    // title-less records collapse to their sanitized _id prefix, so the deleted
    // row is recorded as "without". The cleanup deleting the unscreened row is
    // what this pins.
    repository.deletes                                         should contain (("without", Some(2025)))
  }

  it should "be idempotent — no-op when every row has at least one cinema slot" in {
    val repository = new InMemoryMovieRepository(Seq(
      ("Drama",  Some(2026), mkRecord("tt1", Map(Helios -> cinemaSlot))),
      ("Erupcja", Some(2026), mkRecord("tt2", Map(Helios -> cinemaSlot)))
    ))
    val cache = new CaffeineMovieCache(repository)
    val cleanup = new UnscreenedCleanup(cache)

    cleanup.removeUnscreened()                 shouldBe 0
    cleanup.removeUnscreened()                 shouldBe 0  // second pass: still no-op
    repository.deletes                               shouldBe empty
  }

  it should "count rows correctly when called on an empty cache" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository())
    val cleanup = new UnscreenedCleanup(cache)
    cleanup.removeUnscreened() shouldBe 0
  }
}
