package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.staging.InMemoryStagingRepository

/**
 * A listing whose title runs along a known film's ("It Ends with Us" around "It Ends") is a
 * decorated screening of it only if nothing the venue published says otherwise.
 *
 * US hard clusters, 2026-09-25: Colonial Theatre Phoenixville lists "It Ends with Us" (130 min,
 * Justin Baldoni). Arriving after Bay Theater's "It Ends" (89 min, Alexander Ullom) had
 * resolved, the landing read it as a decorated "It Ends" and put it on that film; arriving
 * together, both resolved on their own. Its director is not the film's and its runtime is
 * forty minutes off — a different film, which must incubate and resolve on its own.
 */
class DecorationVetoSpec extends AnyFlatSpec with Matchers {

  "a listing that decorates a known title" should "not land on it when its own director and runtime deny the film" in {
    val repository = new InMemoryMovieRepository(normalizer = titleNormalizer)
    val staging    = new InMemoryStagingRepository(normalizer = titleNormalizer)
    val cache      = new CaffeineMovieCache(repository, staging = Some(staging), normalizer = titleNormalizer,
      clock = java.time.Clock.fixed(java.time.Instant.parse("2026-09-25T12:00:00Z"), java.time.ZoneOffset.UTC))
    cache.put(CacheKey("It Ends", Some(2026), titleNormalizer), MovieRecord(tmdbId = Some(1422011), data = Map[Source, SourceData](
      Tmdb   -> SourceData(title = Some("It Ends"), releaseYear = Some(2026), runtimeMinutes = Some(89), director = Seq("Alexander Ullom")),
      Helios -> SourceData(title = Some("It Ends"), runtimeMinutes = Some(89), director = Seq("Alexander Ullom")))))

    cache.recordCinemaScrape(Multikino, Seq(CinemaMovie(Movie("It Ends with Us", runtimeMinutes = Some(130)), Multikino,
      None, None, None, Nil, Seq("Justin Baldoni"), Seq(Showtime(java.time.LocalDateTime.of(2026, 10, 1, 19, 0), None)))))

    val onItEnds = cache.snapshot().filter(r => r.record.cinemaSlots.exists { case (s, _) => Source.cinemaOf(s).contains(Multikino) })
    withClue("'It Ends with Us' landed on 'It Ends': ")(onItEnds.map(_.title) shouldBe empty)
    staging.findAll().map(_.title) shouldBe Seq("It Ends with Us")
  }
}
