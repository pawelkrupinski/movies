package services.movies

import models._
import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/**
 * A repository write that THROWS must be counted, and must not leave the cache holding a
 * row the store never took.
 *
 * 2026-09-24: a codec bug made `MovieRepository.upsert`, `SlotsRepository.replaceFilm` and
 * `upsertSlot` throw for ~6h (34 failures, 29 films). The repositories logged a WARN and
 * returned `Unit`; `MovieCache.persist` had already cached the row, so every later
 * identical scrape diffed as a no-op and the write was never retried. Two new films never
 * reached the site until a restart, and no metric moved.
 */
class RepositoryWriteFailureSpec extends AnyFlatSpec with Matchers with LoneElement {

  private class RecordingWriteMetrics extends RepositoryWriteMetrics {
    @volatile var failures: Vector[(String, String, String)] = Vector.empty
    def recordWriteFailed(collection: String, op: String, exception: String): Unit =
      failures :+= ((collection, op, exception))
  }

  // The caches run at a fixed instant and the showtime sits a day after it.
  private val specClock = java.time.Clock.fixed(java.time.Instant.parse("2026-06-01T10:00:00Z"), java.time.ZoneOffset.UTC)
  private val showtime  = Showtime(LocalDateTime.now(specClock).plusDays(1).withHour(20), bookingUrl = None)

  private def listing(title: String) = CinemaMovie(
    movie = Movie(title, releaseYear = Some(2026)), cinema = Multikino,
    posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = Seq(showtime))

  "a new film whose upsert throws" should "be counted, rolled out of the cache, and written by the next identical scrape" in {
    val metrics    = new RecordingWriteMetrics
    val repository = new ThrowingUpsertMovieRepository(metrics)
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    val key        = cache.keyOf("Nowy Film", Some(2026))

    cache.recordCinemaScrape(Multikino, Seq(listing("Nowy Film")))

    metrics.failures shouldBe Vector((MovieRepository.Collection, "upsert", "CodecConfigurationException"))
    withClue("a row the store never took must not stay resident — the next scrape would diff it as a no-op: ")(
      cache.get(key) shouldBe None)
    repository.findAll() shouldBe empty

    repository.failing = false
    cache.recordCinemaScrape(Multikino, Seq(listing("Nowy Film")))

    withClue("the identical re-scrape must retry the write, not skip it: ")(
      repository.findAll().map(_.title) shouldBe Seq("Nowy Film"))
    cache.get(key) should not be empty
  }

  "an existing film whose new slot's write throws" should "keep the pre-update row, and land the slot on the next identical scrape" in {
    val metrics    = new RecordingWriteMetrics
    val slots      = new ThrowingSlotsRepository(metrics)
    slots.failing  = false
    val repository = new InMemoryMovieRepository(
      screenings = Some(new InMemoryScreeningsRepository), slots = Some(slots))
    val cache      = new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = specClock)
    cache.recordCinemaScrape(Multikino, Seq(listing("Stary Film")))
    val key        = cache.keyOf("Stary Film", Some(2026))
    val id         = repository.findAll().map(_.id).loneElement
    def heliosSlots = slots.findForFilm(id.value).keySet.filter(_.startsWith(Helios.displayName))

    slots.failing = true
    cache.recordCinemaScrape(Helios, Seq(listing("Stary Film")))

    metrics.failures.map(_._1) should contain only SlotsRepository.Collection
    heliosSlots shouldBe empty
    withClue("the cache must not hold the Helios slot the store never took: ")(
      cache.get(key).toSeq.flatMap(_.data.keys).flatMap(Source.cinemaOf) should not contain Helios)

    slots.failing = false
    cache.recordCinemaScrape(Helios, Seq(listing("Stary Film")))

    withClue("the identical re-scrape must retry the slot write, not skip it: ")(
      heliosSlots should not be empty)
  }
}
