package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.LocalDateTime

/** The write-time identity gate when the store cannot be read: a write on a cold key is
 *  deferred rather than minting a second document for it once the store recovers. */
class FilmIdentityWritesSpec extends AnyFlatSpec with Matchers {

  private def slot(cinema: Cinema, title: String) =
    Map[Source, SourceData]((cinema: Source) -> SourceData(title = Some(title), releaseYear = Some(2026),
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 6, 12, 20, 0), None))))

  "a write on a key the store cannot be asked about" should "be deferred, not written under a second id" in {
    val repository = new UnreadableByIdMovieRepository(titleNormalizer = titleNormalizer)
    repository.failing = false
    val cache = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val key   = CacheKey("Beta", Some(2026), titleNormalizer)
    repository.upsert("Beta", Some(2026), MovieRecord(imdbRating = Some(6.5), data = slot(KinoMuza, "Beta")))
    val stored = repository.findAll().map(_.id)

    repository.failing = true
    cache.put(key, MovieRecord(data = slot(Helios, "Beta")))      // a cold key, and the store is unreadable

    cache.skippedUnreadable.get() shouldBe 1
    cache.get(key) shouldBe empty
    repository.failing = false
    repository.findAll().map(_.id) shouldBe stored                 // still one document, the original
  }
}
