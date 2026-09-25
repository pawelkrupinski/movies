package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.InMemoryStagingRepository
import services.movies.SingleCountryNormalizer.titleNormalizer

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
    extends InMemoryMovieRepository(seed, normalizer = titleNormalizer) {
    @volatile var blackout: Boolean = true
    val corpusReads = new java.util.concurrent.atomic.AtomicInteger(0)
    override def findAll(): Seq[StoredMovieRecord] = if (blackout) Seq.empty else super.findAll()
    // What `MongoMovieRepository` reports for the same blackout: nothing, and incomplete.
    override def findAllChecked(): (Seq[StoredMovieRecord], Boolean) = {
      corpusReads.incrementAndGet()
      if (blackout) (Seq.empty, false) else super.findAllChecked()
    }
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
    val cache   = new CaffeineMovieCache(repo, staging = Some(staging), normalizer = titleNormalizer)
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

  // The same cold mirror with Mongo STILL unreadable when the first scrape lands. The sync's
  // `findAll` came back empty, which read as "the corpus is genuinely empty": the one-shot
  // latch disarmed for good, and every known film the scrape carried was diverted.
  it should "neither divert nor disarm the cold-mirror sync when the corpus read FAILED, and sync on the first scrape after its backoff" in {
    val staging = new InMemoryStagingRepository
    val repo    = new BootBlackoutRepository(Seq(("Toy Story 5", Some(2026), knownRow)))
    val clock   = new tools.MutableClock(java.time.Instant.parse("2026-09-24T12:00:00Z"))
    val cache   = new CaffeineMovieCache(repo, staging = Some(staging), normalizer = titleNormalizer, clock = clock)

    cache.recordCinemaScrape(cinema, Seq(scrape("Toy Story 5")))   // Mongo still down
    withClue("the tick landed on an unreadable corpus and diverted a known film: ") {
      staging.findAll() shouldBe empty
    }

    repo.blackout = false
    clock.advanceSeconds(ScrapeLanding.ColdMirrorRetryMax.toSeconds)
    cache.recordCinemaScrape(cinema, Seq(scrape("Toy Story 5")))   // Mongo back: the sync must still fire
    withClue("the sync latch disarmed on the failed read, so the recovered corpus was never synced: ") {
      staging.findAll() shouldBe empty
    }
  }

  // THE STITCHED CORPUS READ IS THE MOST EXPENSIVE READ THE WORKER MAKES. While it keeps
  // failing, every venue's scrape asked for it again — a whole Poland tick of venues, each a
  // full keyset scan against a Mongo that is already not answering. Backed off instead: the
  // ticks inside the backoff are discarded unread, the first after it tries again.
  it should "back off the corpus read while it keeps failing, discarding the ticks in between" in {
    val staging = new InMemoryStagingRepository
    val repo    = new BootBlackoutRepository(Seq(("Toy Story 5", Some(2026), knownRow)))
    val clock   = new tools.MutableClock(java.time.Instant.parse("2026-09-24T12:00:00Z"))
    val cache   = new CaffeineMovieCache(repo, staging = Some(staging), normalizer = titleNormalizer, clock = clock)
    val atBoot  = repo.corpusReads.get

    (1 to 5).foreach(_ => cache.recordCinemaScrape(cinema, Seq(scrape("Toy Story 5"))))
    repo.corpusReads.get - atBoot shouldBe 1
    staging.findAll() shouldBe empty

    clock.advanceSeconds(ScrapeLanding.ColdMirrorRetryMax.toSeconds)
    repo.blackout = false
    cache.recordCinemaScrape(cinema, Seq(scrape("Toy Story 5")))
    repo.corpusReads.get - atBoot shouldBe 2
    staging.findAll() shouldBe empty
    cache.get(cache.keyOf("Toy Story 5", Some(2026))) should not be empty   // synced, landed
  }

  // Two venues' first scrapes landing together both saw the latch armed and the mirror cold,
  // and both read the whole corpus and rehydrated: the one-shot sync fired once per racer.
  it should "sync once when two venues' first scrapes land at the same time" in {
    val staging = new InMemoryStagingRepository
    val repo    = new BootBlackoutRepository(Seq(("Toy Story 5", Some(2026), knownRow))) {
      // Holds each corpus read until a second one arrives (or 500ms pass), so racers overlap.
      val bothReading = new java.util.concurrent.CountDownLatch(2)
      override def findAllChecked(): (Seq[StoredMovieRecord], Boolean) = {
        bothReading.countDown()
        bothReading.await(500, java.util.concurrent.TimeUnit.MILLISECONDS)
        super.findAllChecked()
      }
    }
    val cache   = new CaffeineMovieCache(repo, staging = Some(staging), normalizer = titleNormalizer)
    repo.blackout = false
    val atBoot  = repo.corpusReads.get

    val racers = Seq[Cinema](KinoMuza, Multikino).map { venue =>
      new Thread(() => { cache.recordCinemaScrape(venue, Seq(scrape("Toy Story 5").copy(cinema = venue))); () })
    }
    racers.foreach(_.start())
    racers.foreach(_.join(10000))
    repo.corpusReads.get - atBoot shouldBe 1
  }
}
