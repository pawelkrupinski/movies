package services.staging

import models.{CinemaMovie, Helios, Movie, Multikino, MovieRecord, Showtime, Source, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.events.{InProcessEventBus, StagingNewcomerDiverted}
import services.movies.{CaffeineMovieCache, InMemoryMovieRepository}

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

  private def cacheWithStaging(repository: InMemoryMovieRepository, staging: InMemoryStagingRepository): CaffeineMovieCache =
    new CaffeineMovieCache(repository, new InProcessEventBus, staging = Some(staging))

  "a genuinely-new film" should "be diverted to staging, not movies" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)

    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))

    cache.entries shouldBe empty
    staging.findAll().map(r => (r.cinema, r.title, r.year)) shouldBe Seq((Helios, "Brand New Film", Some(2026)))
  }

  "a first-time diverted newcomer" should "publish StagingNewcomerDiverted so the reaper kicks its chain off an event, but not republish on re-scrape" in {
    val staging = new InMemoryStagingRepository
    val bus     = new InProcessEventBus
    val kicked  = scala.collection.mutable.ArrayBuffer.empty[String]
    bus.subscribe { case StagingNewcomerDiverted(title) => kicked += title }
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, bus, staging = Some(staging))

    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))
    kicked.toSeq shouldBe Seq("Brand New Film")                 // first divert → one event

    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))
    kicked.toSeq shouldBe Seq("Brand New Film")                 // still incubating → no republish
  }

  "a film already known to movies" should "stay on the direct path, not divert nor kick a newcomer event" in {
    val staging = new InMemoryStagingRepository
    val bus     = new InProcessEventBus
    val kicked  = scala.collection.mutable.ArrayBuffer.empty[String]
    bus.subscribe { case StagingNewcomerDiverted(title) => kicked += title }
    val cache   = new CaffeineMovieCache(new InMemoryMovieRepository, bus, staging = Some(staging))
    cache.put(cache.keyOf("Kumotry", Some(2026)),
      MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    cache.recordCinemaScrape(Helios, Seq(scrape("Kumotry", Some(2026))))

    kicked shouldBe empty
  }

  "a film already known to movies" should "stay on the direct path, not divert" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    // Seed a known film (any same-sanitize row in movies).
    cache.put(cache.keyOf("Kumotry", Some(2026)),
      MovieRecord(tmdbId = Some(1454157), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Kumotry"), releaseYear = Some(2026)))))

    cache.recordCinemaScrape(Helios, Seq(scrape("Kumotry", Some(2026))))

    staging.findAll() shouldBe empty
    cache.get(cache.keyOf("Kumotry", Some(2026))).map(_.cinemaData.keySet) shouldBe Some(Set(Multikino, Helios))
  }

  "a known film listed under another language" should "land on the existing resolved row, not incubate as a newcomer" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    // A CONCLUDED row for Tangled, keyed under its Polish title. Its TMDB aliases
    // include the original "Tangled", so a cinema listing it in English is a known
    // film — it must NOT be diverted as a brand-new newcomer.
    cache.put(cache.keyOf("Zaplątani", Some(2010)),
      MovieRecord(tmdbId = Some(38757), data = Map[Source, SourceData](
        Tmdb      -> SourceData(title = Some("Zaplątani"), originalTitle = Some("Tangled"), releaseYear = Some(2010)),
        Multikino -> SourceData(title = Some("Zaplątani"), releaseYear = Some(2010)))))

    cache.recordCinemaScrape(Helios, Seq(scrape("Tangled", Some(2010))))

    staging.findAll() shouldBe empty   // not incubated as a newcomer
    cache.get(cache.keyOf("Zaplątani", Some(2010))).map(_.cinemaData.keySet) shouldBe Some(Set(Multikino, Helios))
  }

  "a newcomer a cinema stops listing" should "be pruned from staging" in {
    val staging = new InMemoryStagingRepository
    val cache   = cacheWithStaging(new InMemoryMovieRepository, staging)
    cache.recordCinemaScrape(Helios, Seq(scrape("Film A", Some(2026))))
    staging.findAll().map(_.title) should contain("Film A")

    // Next tick: Helios lists a different newcomer; "Film A" is no longer reported.
    cache.recordCinemaScrape(Helios, Seq(scrape("Film B", Some(2026))))
    staging.findAll().map(_.title) shouldBe Seq("Film B")
  }

  "no staging sink (default)" should "leave every scrape landing in movies" in {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepository, new InProcessEventBus) // staging = None
    cache.recordCinemaScrape(Helios, Seq(scrape("Brand New Film", Some(2026))))
    cache.entries should have size 1
  }
}
