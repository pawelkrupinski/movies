package services.cinemas.common

import models.{Cinema, CinemaMovie, KinoMuza, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}
import services.scrapes.{InMemoryScrapeArchiveRepository, ScrapeOutcome}

import java.time.LocalDateTime

/**
 * `CinemaScrapeRunner` is the one place both scrape paths converge — a
 * non-chunked client's live `fetch()`, and a chunked client's already-reduced
 * chunks arriving wrapped in a `PreScrapedCinemaScraper`. Archiving there is
 * what makes "every cinema's last listing" true for the whole corpus rather than
 * just the simple half of it.
 */
class CinemaScrapeArchiveSpec extends AnyFlatSpec with Matchers {

  private class FakeScraper(
    val cinema: Cinema,
    result:     => Seq[CinemaMovie],
    complete:   Boolean = true
  ) extends CinemaScraper {
    def scrapeHosts: Set[String]            = Set.empty
    def fetch(): Seq[CinemaMovie]           = result
    override def listingIsComplete: Boolean = complete
  }

  private def film(cinema: Cinema, title: String, showtimes: Int = 1) =
    CinemaMovie(Movie(title), cinema, Some("https://poster"), Some("https://film"), Some("A blurb"),
      Seq("Actor"), Seq("Director"),
      (0 until showtimes).map(i => Showtime(LocalDateTime.of(2026, 8, 1, 18, 0).plusHours(i), Some("https://book"))),
      ageRating = Some("15"))

  private def runnerWith(archive: InMemoryScrapeArchiveRepository) =
    new CinemaScrapeRunner(
      new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus()),
      new InProcessEventBus(),
      deferredCinemas = Set.empty,
      scrapeArchive   = archive
    )

  "CinemaScrapeRunner" should "archive a cinema's listing as the client produced it" in {
    val archive = new InMemoryScrapeArchiveRepository
    runnerWith(archive).run(new FakeScraper(Multikino, Seq(film(Multikino, "Dune", showtimes = 3), film(Multikino, "Alien"))))

    val stored = archive.find(Multikino).getOrElse(fail("nothing archived"))
    stored.films.map(_.movie.title) should contain theSameElementsAs Seq("Dune", "Alien")
    stored.lastSuccess.map(_.showtimeCount) shouldBe Some(4)
    stored.city                             shouldBe Cinema.cityOf(Multikino)
    stored.outcome                          shouldBe ScrapeOutcome.Ok
    // The whole payload, not just the showtimes — a replay has to reproduce the
    // detail fields the enrichment pipeline reads off a scrape.
    stored.films.head.synopsis  shouldBe Some("A blurb")
    stored.films.head.ageRating shouldBe Some("15")
    stored.films.head.director  shouldBe Seq("Director")
  }

  it should "archive the reduced result of a chunked scrape too" in {
    val archive = new InMemoryScrapeArchiveRepository
    val reduced = Seq(film(KinoMuza, "Nosferatu"), film(KinoMuza, "Anora"))
    // How ScrapeChunkReduceHandler hands its reduced chunks back to the runner.
    runnerWith(archive).run(new PreScrapedCinemaScraper(KinoMuza, Set.empty, isChain = false, () => reduced, listingComplete = true))

    archive.find(KinoMuza).getOrElse(fail("nothing archived")).films.map(_.movie.title) should
      contain theSameElementsAs Seq("Nosferatu", "Anora")
  }

  it should "mark a partial listing so a replay knows not to trust it as complete" in {
    val archive = new InMemoryScrapeArchiveRepository
    runnerWith(archive).run(new FakeScraper(Multikino, Seq(film(Multikino, "Dune")), complete = false))

    archive.find(Multikino).getOrElse(fail("nothing archived"))
      .lastSuccess.map(_.listingComplete) shouldBe Some(false)
  }

  it should "keep the last listing and flag it white when a scrape comes back empty" in {
    val archive = new InMemoryScrapeArchiveRepository
    val runner  = runnerWith(archive)
    runner.run(new FakeScraper(Multikino, Seq(film(Multikino, "Dune"))))
    runner.run(new FakeScraper(Multikino, Seq.empty))

    val stored = archive.find(Multikino).getOrElse(fail("nothing archived"))
    stored.films.map(_.movie.title) shouldBe Seq("Dune")
    stored.outcome                  shouldBe ScrapeOutcome.Empty
    stored.current                  shouldBe false
  }

  it should "keep the last listing and flag it red when a scrape throws" in {
    val archive = new InMemoryScrapeArchiveRepository
    val runner  = runnerWith(archive)
    runner.run(new FakeScraper(Multikino, Seq(film(Multikino, "Dune"))))

    a[RuntimeException] should be thrownBy
      runner.run(new FakeScraper(Multikino, throw new RuntimeException("503 from multikino.pl")))

    val stored = archive.find(Multikino).getOrElse(fail("nothing archived"))
    stored.films.map(_.movie.title)      shouldBe Seq("Dune")
    stored.outcome                       shouldBe ScrapeOutcome.Failed
    stored.lastBarren.flatMap(_.error)   shouldBe Some("503 from multikino.pl")
  }

  // Callers (ScrapeCinemaHandler → the reaper's retry) decide what a failure
  // means; archiving it must not swallow the throw.
  it should "rethrow a scrape failure after recording it" in {
    val archive = new InMemoryScrapeArchiveRepository
    val boom    = new IllegalStateException("layout changed")

    val thrown = the[IllegalStateException] thrownBy
      runnerWith(archive).run(new FakeScraper(Multikino, throw boom))

    thrown should be theSameInstanceAs boom
    archive.find(Multikino).getOrElse(fail("nothing archived")).outcome shouldBe ScrapeOutcome.Failed
  }

  it should "still scrape normally when no archive is wired" in {
    val runner = new CinemaScrapeRunner(
      new CaffeineMovieCache(new InMemoryMovieRepository(), new InProcessEventBus()),
      new InProcessEventBus(),
      deferredCinemas = Set.empty
    )
    runner.run(new FakeScraper(Multikino, Seq(film(Multikino, "Dune")))) should have size 1
  }
}
