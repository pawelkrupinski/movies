package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `putSlotIfPresent` is `putIfPresent` of a one-slot update, made cheap: it compares,
 * strips, re-indexes and diffs only the slot it writes (see its doc comment). Cheap must
 * not mean different, so this drives the same sequence of slot writes through both and
 * asserts the store, its screenings and the cache end up identical after every step.
 */
class PutSlotIfPresentSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val filmKey    = CacheKey("Slot Film", Some(2026), normalizer)
  private def at(day: Int) = DepthGuardTime.showtimes(12 * day).last

  private final class Side {
    val repository = new InMemoryMovieRepository(normalizer = normalizer, screenings = Some(new InMemoryScreeningsRepository))
    val cache      = new CaffeineMovieCache(repository, normalizer = normalizer, clock = DepthGuardTime.clock)
    /** What a reader can see: each stored row with its per-slot showtimes, and the cache's row. */
    def state: (Seq[(CacheKey, MovieRecord, Map[Source, Seq[Showtime]])], Option[(MovieRecord, Map[Source, Int])]) =
      (repository.findAll().map(r => (r.cacheKey(normalizer), r.record, r.record.data.view.mapValues(_.showtimes).toMap)),
       cache.get(filmKey).map(r => (r, r.data.view.mapValues(ShowtimesDigest.slotDigest).toMap)))
  }

  "putSlotIfPresent" should "leave the store and the cache exactly where putIfPresent of the same slot does" in {
    val (slotWise, recordWise) = (new Side, new Side)
    Seq(slotWise, recordWise).foreach(_.cache.put(filmKey, MovieRecord(imdbRating = Some(7.5), data = Map[Source, SourceData](
      (KinoMuza: Source) -> SourceData(title = Some("Slot Film"), showtimes = Seq(at(1)))))))
    val writes: Seq[(Source, SourceData)] = Seq(
      CinemaShowing.keyFor(Multikino, "Slot Film", normalizer) -> SourceData(title = Some("Slot Film"), showtimes = Seq(at(2))),
      CinemaShowing.keyFor(Multikino, "Slot Film", normalizer) -> SourceData(title = Some("Slot Film"), showtimes = Seq(at(2))),
      CinemaShowing.keyFor(Multikino, "Slot Film", normalizer) -> SourceData(title = Some("Slot Film"), showtimes = Seq(at(2), at(3))),
      (KinoMuza: Source) -> SourceData(title = Some("Slot Film"), synopsis = Some("new"), showtimes = Seq(at(1))),
      (KinoMuza: Source) -> SourceData(title = Some("Slot Film"), synopsis = Some("new"), showtimes = Nil))
    writes.zipWithIndex.foreach { case ((source, slot), step) =>
      slotWise.cache.putSlotIfPresent(filmKey, source, slot)
        .shouldBe(recordWise.cache.putIfPresent(filmKey, r => r.copy(data = r.data + (source -> slot))))
      withClue(s"after write $step: ")(slotWise.state shouldBe recordWise.state)
    }
  }
}
