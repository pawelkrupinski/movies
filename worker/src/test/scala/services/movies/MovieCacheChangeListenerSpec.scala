package services.movies

import models.{CinemaShowing, Helios, MovieRecord, Multikino, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/** [[MovieCache.onChanged]] tells the identity projection of a film another writer moved — the projection has no period to
 *  find it on — and never of the projection's own writes or their echo, or every projection would set off another. */
class MovieCacheChangeListenerSpec extends AnyFlatSpec with Matchers {
  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val id         = FilmId("flistener")
  private val filmKey    = CacheKey.stored("Listener", "listener|2026")
  private val at         = LocalDateTime.of(2036, 10, 7, 18, 0)
  private def slot(cinema: models.Cinema, hours: Int*) = CinemaShowing.keyFor(cinema, "Listener", normalizer) ->
    SourceData(title = Some("Listener"), releaseYear = Some(2026), showtimes = hours.map(h => Showtime(at.plusHours(h.toLong), None)))
  private val film = MovieRecord(data = Map(slot(Multikino, 0, 2), slot(Helios, 3)))

  private final class World {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository), slots = Some(new InMemorySlotsRepository),
      normalizer = normalizer)
    val cache   = new CaffeineMovieCache(repository, normalizer = normalizer, clock = _root_.tools.SpecClock.Pinned)
    val changed = scala.collection.mutable.ListBuffer.empty[FilmId]
    cache.onChanged(changed += _)
    cache.writeProjected(id, filmKey, film)
    def resident: MovieRecord = cache.snapshot().find(_.id == id).get.record
  }

  "the movie cache" should "tell of a film another writer moved through it, and not of a write that moved nothing" in {
    val w = new World
    w.cache.putIfPresent(filmKey, identity)
    w.changed shouldBe empty
    w.cache.putIfPresent(filmKey, _.copy(imdbRating = Some(7.1)))
    w.changed.toSeq shouldBe Seq(id)
  }

  it should "not tell of the identity projection's own writes, nor of their echo from the change stream" in {
    val w = new World
    w.changed shouldBe empty                                       // writeProjected, in the world's set-up
    val before = w.resident
    w.cache.patchProjected(id, filmKey, before, before.copy(data = before.data + slot(Multikino, 0, 5))) shouldBe WriteOutcome.Written
    w.cache.applyUpsert(w.repository.findByIdChecked(id).answered.get, FilmWriteFence.Unfenced)
    w.changed shouldBe empty
  }

  it should "tell of another process's change the change stream brings" in {
    val w = new World
    val stored = w.repository.findByIdChecked(id).answered.get
    w.cache.applyUpsert(stored.copy(record = stored.record.copy(metascore = Some(61))), FilmWriteFence.Unfenced)
    w.changed.toSeq shouldBe Seq(id)
  }
}
