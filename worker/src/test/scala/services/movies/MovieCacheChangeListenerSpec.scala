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

  // The echo of a write is read back as new objects of the same content. Stored, it replaced the record the projection
  // left (its lean slots, which the next projection compares by `eq`) with a copy every reader then compared slot by slot,
  // and each copy lived until the film's next write: on worker-us, minutes — long enough to be promoted, and die in the old
  // generation.
  "a change-stream read of what the cache holds already" should "leave the held record as it is, the very object" in {
    val w = new World
    val before = w.resident
    w.cache.patchProjected(id, filmKey, before, before.copy(data = before.data + slot(Multikino, 0, 5))) shouldBe WriteOutcome.Written
    val held = w.cache.get(filmKey).get
    w.cache.applyUpsert(w.repository.findByIdChecked(id).answered.get, FilmWriteFence.Unfenced)
    w.cache.get(filmKey).get should be theSameInstanceAs held
  }

  it should "leave it as it is when the read is of some venues alone" in {
    val w = new World
    val before = w.resident
    w.cache.patchProjected(id, filmKey, before, before.copy(data = before.data + slot(Multikino, 0, 5))) shouldBe WriteOutcome.Written
    val held   = w.cache.get(filmKey).get
    val stored = w.repository.findByIdChecked(id).answered.get.record
    val venues = VenueSlots(id, Map(Multikino -> stored.data.toSeq.collect { case (s @ CinemaShowing(Multikino, _), sd) => s -> sd }))
    w.cache.applyVenueSlots(venues, FilmWriteFence.Unfenced) shouldBe VenueVerdict.Applied
    w.cache.get(filmKey).get should be theSameInstanceAs held
    w.changed shouldBe empty
  }

  it should "keep, of a film another writer moved, every slot it did not move as the held object" in {
    val w = new World
    val held   = w.cache.get(filmKey).get
    val stored = w.repository.findByIdChecked(id).answered.get
    w.cache.applyUpsert(stored.copy(record = stored.record.copy(metascore = Some(61))), FilmWriteFence.Unfenced)
    val now = w.cache.get(filmKey).get
    now.metascore shouldBe Some(61)
    now.data.keySet shouldBe held.data.keySet
    now.data.foreach { case (source, sd) => sd should be theSameInstanceAs held.data(source) }
    w.changed.toSeq shouldBe Seq(id)
  }

  it should "take a slot whose cast another writer only reordered: the read's order is what the store holds" in {
    val w = new World
    val (source, sd) = slot(Helios, 3)
    val cast = sd.copy(cast = Seq("Ann Lee", "Bo Chan"))
    w.cache.patchProjected(id, filmKey, w.resident, w.resident.copy(data = w.resident.data + (source -> cast))) shouldBe WriteOutcome.Written
    val stored = w.repository.findByIdChecked(id).answered.get
    val reordered = stored.record.data(source).copy(cast = Seq("Bo Chan", "Ann Lee"))
    w.cache.applyUpsert(stored.copy(record = stored.record.copy(data = stored.record.data + (source -> reordered))), FilmWriteFence.Unfenced)
    w.cache.get(filmKey).get.data(source).cast shouldBe Seq("Bo Chan", "Ann Lee")
  }

  "a projection's patch of a film whose slots the cache holds" should "keep the record's map, not a copy of it" in {
    val w = new World
    val before = w.resident
    w.cache.patchProjected(id, filmKey, before, before.copy(metascore = Some(70))) shouldBe WriteOutcome.Written
    assert(w.cache.get(filmKey).get.data eq before.data, "the map of held slots was copied")
  }

  it should "tell of another process's change the change stream brings" in {
    val w = new World
    val stored = w.repository.findByIdChecked(id).answered.get
    w.cache.applyUpsert(stored.copy(record = stored.record.copy(metascore = Some(61))), FilmWriteFence.Unfenced)
    w.changed.toSeq shouldBe Seq(id)
  }

  // The corpus census keeps each film's part from what the cache holds, so it must hear of EVERY change — the identity
  // projection's included, which `onChanged` leaves out — and start from every film already held.
  "its resident listeners" should "hear of every film held, then of every change by any path, and of a film's going" in {
    val w     = new World
    val heard = scala.collection.mutable.ListBuffer.empty[(CacheKey, Option[Option[Int]])]
    w.cache.onResident((key, film) => heard += key -> film.map(_.record.metascore))
    heard.toSeq shouldBe Seq(filmKey -> Some(None))                // the film held when it registered
    heard.clear()

    w.cache.putIfPresent(filmKey, identity)                         // moved nothing
    heard shouldBe empty
    val before = w.resident
    w.cache.patchProjected(id, filmKey, before, before.copy(metascore = Some(70))) shouldBe WriteOutcome.Written
    val stored = w.repository.findByIdChecked(id).answered.get
    w.cache.applyUpsert(stored.copy(record = stored.record.copy(metascore = Some(61))), FilmWriteFence.Unfenced)
    w.cache.putIfPresent(filmKey, _.copy(metascore = Some(62)))
    w.cache.retireProjected(id) shouldBe WriteOutcome.Written
    heard.toSeq shouldBe Seq(filmKey -> Some(Some(70)), filmKey -> Some(Some(61)), filmKey -> Some(Some(62)), filmKey -> None)
  }

  it should "hear a write the store refused put back as it was" in {
    val repository = new InMemoryMovieRepository(screenings = Some(new InMemoryScreeningsRepository),
      slots = Some(new UnwritableSlotsRepository), normalizer = normalizer)
    val cache = new CaffeineMovieCache(repository, normalizer = normalizer, clock = _root_.tools.SpecClock.Pinned)
    val heard = scala.collection.mutable.ListBuffer.empty[Option[MovieRecord]]
    cache.onResident((_, film) => heard += film.map(_.record))
    cache.put(filmKey, film).failed shouldBe true
    cache.get(filmKey) shouldBe None                                  // rolled back
    heard.toSeq.map(_.isDefined) shouldBe Seq(true, false)            // held, then gone again
  }

  // A listener runs inside the write's own lock: one that throws must not fail a write already made.
  it should "keep a write a resident listener fails on" in {
    val w = new World
    w.cache.onResident((_, _) => throw new IllegalStateException("listener bug"))
    w.cache.putIfPresent(filmKey, _.copy(metascore = Some(55))) shouldBe true
    w.resident.metascore shouldBe Some(55)
  }
}
