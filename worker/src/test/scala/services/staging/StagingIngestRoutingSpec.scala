package services.staging

import models.{CinemaMovie, Helios, Movie, Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.InProcessEventBus
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}

import java.time.LocalDateTime

/** Phase 3: with a staging sink wired, a genuinely-new film is diverted to
 *  `pending_movies` (one row per cinema|title|year) instead of `movies`; a film
 *  already known to `movies` keeps the direct path; and a newcomer a cinema
 *  stops listing is pruned from staging. */
class StagingIngestRoutingSpec extends AnyFlatSpec with Matchers {

  private val When = LocalDateTime.of(2026, 6, 14, 18, 0)

  private def scrape(title: String, year: Option[Int]): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = year),
      cinema    = Helios,
      posterUrl = None, filmUrl = Some(s"https://example/$title"), synopsis = None,
      cast = Nil, director = Nil, showtimes = Seq(Showtime(When, bookingUrl = None)))

  private def cacheWithStaging(repo: InMemoryMovieRepo, staging: InMemoryStagingRepo): CaffeineMovieCache =
    new CaffeineMovieCache(repo, new InProcessEventBus, staging = Some(staging))

  "a genuinely-new film" should "be diverted to staging, not movies" in {
    val staging = new InMemoryStagingRepo
    val cache   = cacheWithStaging(new InMemoryMovieRepo, staging)

    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))

    cache.entries shouldBe empty
    staging.findAll().map(r => (r.cinema, r.title, r.year)) shouldBe Seq((Helios, "Brand New Film", Some(2026)))
  }

  "a film already known to movies" should "stay on the direct path, not divert" in {
    val staging = new InMemoryStagingRepo
    val cache   = cacheWithStaging(new InMemoryMovieRepo, staging)
    // Seed a known film (any same-sanitize row in movies).
    cache.put(cache.keyOf("Kumotry", Some(2026)),
      MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    cache.recordCinemaScrape(Helios, Seq(scrape("Kumotry", Some(2026))))

    staging.findAll() shouldBe empty
    cache.get(cache.keyOf("Kumotry", Some(2026))).map(_.cinemaData.keySet) shouldBe Some(Set(Multikino, Helios))
  }

  "a newcomer a cinema stops listing" should "be pruned from staging" in {
    val staging = new InMemoryStagingRepo
    val cache   = cacheWithStaging(new InMemoryMovieRepo, staging)
    cache.recordCinemaScrape(Helios, Seq(scrape("Film A", Some(2026))))
    staging.findAll().map(_.title) should contain("Film A")

    // Next tick: Helios lists a different newcomer; "Film A" is no longer reported.
    cache.recordCinemaScrape(Helios, Seq(scrape("Film B", Some(2026))))
    staging.findAll().map(_.title) shouldBe Seq("Film B")
  }

  "no staging sink (default)" should "leave every scrape landing in movies" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo, new InProcessEventBus) // staging = None
    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))
    cache.entries should have size 1
  }
}
