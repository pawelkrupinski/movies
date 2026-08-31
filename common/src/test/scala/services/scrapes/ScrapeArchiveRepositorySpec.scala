package services.scrapes

import models.{Cinema, CinemaMovie, KinoMuza, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime}

/**
 * The archive's contract, exercised against the in-memory store. The rules that
 * matter — content is never replaced by nothing, a barren attempt rides on top
 * of the listing it failed to refresh, a stale one is ignored — live in the
 * trait, so a Mongo-backed store inherits exactly what is asserted here.
 */
class ScrapeArchiveRepositorySpec extends AnyFlatSpec with Matchers {

  private val Morning = Instant.parse("2026-07-28T09:00:00Z")
  private val Noon    = Instant.parse("2026-07-28T12:00:00Z")
  private val Evening = Instant.parse("2026-07-28T18:00:00Z")

  private def film(title: String, showtimes: Int = 1) =
    CinemaMovie(Movie(title), Multikino, None, None, None, Seq.empty, Seq.empty,
      (0 until showtimes).map(i => Showtime(LocalDateTime.of(2026, 8, 1, 18, 0).plusHours(i), Some("https://book"))))

  private def scraped(cinema: Cinema, at: Instant, films: Seq[CinemaMovie], complete: Boolean = true) =
    ScrapeAttempt(cinema, Cinema.cityOf(cinema), at, complete, films)

  private def blank(cinema: Cinema, at: Instant) =
    ScrapeAttempt(cinema, Cinema.cityOf(cinema), at, listingComplete = true, films = Seq.empty)

  private def threw(cinema: Cinema, at: Instant, error: String) =
    ScrapeAttempt(cinema, Cinema.cityOf(cinema), at, listingComplete = true, films = Seq.empty, error = Some(error))

  "ScrapeArchiveRepository" should "store a cinema's scrape and read it back whole" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Noon, Seq(film("Dune", showtimes = 3), film("Alien"))))

    val stored = repository.find(Multikino).value
    stored.films.map(_.movie.title) should contain theSameElementsAs Seq("Dune", "Alien")
    stored.lastSuccess.value.showtimeCount   shouldBe 4
    stored.lastSuccess.value.listingComplete shouldBe true
    stored.contentAt shouldBe Some(Noon)
    stored.city      shouldBe Cinema.cityOf(Multikino)
    stored.outcome   shouldBe ScrapeOutcome.Ok
    stored.current   shouldBe true
  }

  it should "replace the previous listing rather than accumulate" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Morning, Seq(film("Dune"), film("Alien"))))
    repository.record(scraped(Multikino, Noon, Seq(film("Nosferatu"))))

    repository.find(Multikino).value.films.map(_.movie.title) shouldBe Seq("Nosferatu")
    repository.findAll() should have size 1
  }

  // The whole point of the collection: whatever else happens, the last listing
  // we actually saw survives.
  it should "keep the last successful listing when a later scrape comes back empty" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Morning, Seq(film("Dune"))))
    repository.record(blank(Multikino, Noon))

    val stored = repository.find(Multikino).value
    stored.films.map(_.movie.title) shouldBe Seq("Dune")
    stored.contentAt                shouldBe Some(Morning)
    stored.outcome                  shouldBe ScrapeOutcome.Empty
    stored.lastBarren.value.at      shouldBe Noon
    stored.lastBarren.value.error   shouldBe None
    stored.current                  shouldBe false
  }

  it should "keep the last successful listing when a later scrape throws" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Morning, Seq(film("Dune"))))
    repository.record(threw(Multikino, Noon, "503 from multikino.pl"))

    val stored = repository.find(Multikino).value
    stored.films.map(_.movie.title) shouldBe Seq("Dune")
    stored.outcome                  shouldBe ScrapeOutcome.Failed
    stored.lastBarren.value.error   shouldBe Some("503 from multikino.pl")
  }

  it should "let the newest barren attempt supersede an older one" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Morning, Seq(film("Dune"))))
    repository.record(blank(Multikino, Noon))
    repository.record(threw(Multikino, Evening, "connection reset"))

    val stored = repository.find(Multikino).value
    stored.lastBarren.value.at      shouldBe Evening
    stored.lastBarren.value.outcome shouldBe ScrapeOutcome.Failed
    stored.films.map(_.movie.title) shouldBe Seq("Dune")
  }

  // How long a cinema has been failing is what separates a blip from a venue that
  // has been deleted upstream (`GoneUpstream`), and the newest attempt cannot say:
  // a page 404ing for a week, scraped a minute ago, has `at` a minute old. So the
  // run's START rides along, carried from the marker each attempt replaces.
  it should "carry the start of a barren run forward across attempts" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(threw(Multikino, Morning, "HTTP 404 for GET https://x/"))
    repository.record(threw(Multikino, Noon,    "HTTP 404 for GET https://x/"))
    repository.record(threw(Multikino, Evening, "HTTP 404 for GET https://x/"))

    val stored = repository.find(Multikino).value
    stored.lastBarren.value.at           shouldBe Evening   // the newest attempt is what is stored
    stored.lastBarren.value.runStartedAt shouldBe Morning   // …and it has been failing since morning
  }

  it should "start a new run after a success, not resume the old one" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(threw(Multikino, Morning, "HTTP 404 for GET https://x/"))
    repository.record(scraped(Multikino, Noon, Seq(film("Dune"))))
    repository.record(threw(Multikino, Evening, "HTTP 404 for GET https://x/"))

    repository.find(Multikino).value.lastBarren.value.runStartedAt shouldBe Evening
  }

  it should "clear the barren marker once the cinema scrapes successfully again" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Morning, Seq(film("Dune"))))
    repository.record(threw(Multikino, Noon, "503"))
    repository.record(scraped(Multikino, Evening, Seq(film("Anora"))))

    val stored = repository.find(Multikino).value
    stored.lastBarren               shouldBe None
    stored.outcome                  shouldBe ScrapeOutcome.Ok
    stored.current                  shouldBe true
    stored.films.map(_.movie.title) shouldBe Seq("Anora")
  }

  // An attempt that predates the stored listing says nothing about it — letting
  // it through would mark a perfectly current row as stale.
  it should "ignore a barren attempt older than the stored listing" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Noon, Seq(film("Dune"))))
    repository.record(blank(Multikino, Morning))

    val stored = repository.find(Multikino).value
    stored.lastBarren shouldBe None
    stored.current    shouldBe true
  }

  it should "record a cinema that has only ever failed, with no listing" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(threw(Multikino, Noon, "DNS failure"))

    val stored = repository.find(Multikino).value
    stored.lastSuccess shouldBe None
    stored.films       shouldBe empty
    stored.outcome     shouldBe ScrapeOutcome.Failed
    stored.contentAt   shouldBe None
  }

  it should "keep one row per cinema" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Noon, Seq(film("Dune"))))
    repository.record(scraped(KinoMuza, Noon, Seq(film("Alien"))))

    repository.findAll().map(_.cinema) should contain theSameElementsAs Seq(Multikino, KinoMuza)
  }

  it should "carry the partial-listing flag through, so a replay can tell" in {
    val repository = new InMemoryScrapeArchiveRepository
    repository.record(scraped(Multikino, Noon, Seq(film("Dune")), complete = false))

    repository.find(Multikino).value.lastSuccess.value.listingComplete shouldBe false
  }

  "ScrapeAttempt" should "classify itself by what it carries" in {
    scraped(Multikino, Noon, Seq(film("Dune"))).outcome shouldBe ScrapeOutcome.Ok
    blank(Multikino, Noon).outcome                      shouldBe ScrapeOutcome.Empty
    threw(Multikino, Noon, "boom").outcome              shouldBe ScrapeOutcome.Failed
    // A failure that still returned partial films is a failure — the listing is
    // not trustworthy as the cinema's full repertoire.
    scraped(Multikino, Noon, Seq(film("Dune"))).copy(error = Some("boom")).outcome shouldBe ScrapeOutcome.Failed
  }

  "the no-op archive" should "accept writes and stay empty" in {
    ScrapeArchiveRepository.empty.record(scraped(Multikino, Noon, Seq(film("Dune"))))

    ScrapeArchiveRepository.empty.enabled         shouldBe false
    ScrapeArchiveRepository.empty.find(Multikino) shouldBe None
    ScrapeArchiveRepository.empty.findAll()       shouldBe empty
  }

  private implicit class OptionValue[A](option: Option[A]) {
    def value: A = option.getOrElse(fail("expected a value, found none"))
  }
}
