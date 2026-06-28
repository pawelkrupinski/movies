package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.InMemoryStagingRepository

/**
 * Reproduces the post-reboot corpus re-divert flap (panel-36 `kinowo_worker_corpus_movies`
 * swinging 812 → 670 → 814 on 2026-06-28, ~150 known films re-incubated into
 * `pending_movies` then folded back over ~15 min).
 *
 * The trigger is a scrape landing while the in-memory movies mirror is COLD. The
 * worker reboots alongside its Mongo; `bootHydrate`'s `findAll()` can return empty
 * (Mongo not ready yet — `findAll` swallows the failure to `Seq.empty`), leaving the
 * `positive` mirror empty even though the corpus is fully populated. The change
 * stream only delivers rows written AFTER boot (see `MovieCache.bootHydrate`), so a
 * pre-boot row never reaches the mirror until the next full `rehydrate`. A scrape that
 * lands in that window reads the empty mirror, finds NONE of its films "known", and
 * diverts every one into staging — the mass re-divert that drains `movies`.
 *
 * Seeded below: a film that IS persisted in `movies` (a pre-boot row) but is absent
 * from the cold mirror — so diverting it is unambiguously wrong (it's already known).
 * `BootBlackoutRepository` hides the seeded rows from the boot `findAll` (Mongo
 * blackout), then reveals them — exactly the prod race.
 */
class ColdMirrorReDivertSpec extends AnyFlatSpec with Matchers {

  private val cinema: Cinema = KinoMuza

  // A `findAll`-empty-at-boot repository: while `blackout` is true (Mongo not ready
  // during the worker's boot) `findAll` returns nothing, so the cache hydrates cold;
  // flipping it off reveals the pre-boot rows, which the cache's mirror has never seen.
  private class BootBlackoutRepository(seed: Seq[(String, Option[Int], MovieRecord)])
    extends InMemoryMovieRepository(seed) {
    @volatile var blackout: Boolean = true
    override def findAll(): Seq[StoredMovieRecord] = if (blackout) Seq.empty else super.findAll()
  }

  private def knownRow: MovieRecord =
    MovieRecord(tmdbId = Some(1084244),
      data = Map[Source, SourceData](
        (cinema: Source) -> SourceData(
          title = Some("Toy Story 5"), rawTitle = Some("Toy Story 5"), releaseYear = Some(2026))))

  private def scrape(title: String): CinemaMovie =
    CinemaMovie(Movie(title = title, releaseYear = Some(2026)),
      cinema, posterUrl = None, filmUrl = None, synopsis = None,
      cast = Nil, director = Nil, showtimes = Nil)

  "recordCinemaScrape" should
    "NOT re-divert a known film into staging when the movies mirror is still cold post-reboot (corpus boot flap)" in {
    val staging = new InMemoryStagingRepository
    val repo    = new BootBlackoutRepository(Seq(("Toy Story 5", Some(2026), knownRow)))
    // Boots cold: bootHydrate's findAll sees the blackout, so the mirror is empty
    // even though "Toy Story 5" is persisted in `movies`.
    val cache   = new CaffeineMovieCache(repo, staging = Some(staging))
    repo.blackout = false // Mongo recovered; the pre-boot row is now visible to findAll, but not to the cold mirror.

    val before   = staging.findAll().toSet
    cache.recordCinemaScrape(cinema, Seq(scrape("Toy Story 5")))
    val diverted = staging.findAll().toSet -- before

    withClue(
      "a KNOWN film (already persisted in `movies`) was re-incubated into staging because the mirror " +
        "was cold at scrape time — that mass re-divert on every reboot IS the corpus flap. Staging delta: " +
        s"$diverted\n") {
      diverted shouldBe empty
    }
  }
}
