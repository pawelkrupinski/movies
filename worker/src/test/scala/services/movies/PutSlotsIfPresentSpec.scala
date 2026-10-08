package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `putSlotsIfPresent` is `putIfPresent` of an update to some of a row's slots, made cheap: it compares,
 * strips and diffs only the slots it writes (see its doc comment). Cheap must
 * not mean different, so this drives the same sequence of slot writes through both and
 * asserts the store, its screenings, the cache and the films announced changed end up identical after every step.
 */
class PutSlotsIfPresentSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val filmKey    = CacheKey("Slot Film", Some(2026), normalizer)
  private def at(day: Int) = DepthGuardTime.showtimes(12 * day).last

  private final class Side {
    val repository = new InMemoryMovieRepository(normalizer = normalizer, screenings = Some(new InMemoryScreeningsRepository))
    val cache      = new CaffeineMovieCache(repository, normalizer = normalizer, clock = _root_.tools.SpecClock.Pinned)
    val announced  = new java.util.concurrent.atomic.AtomicInteger(0)
    cache.onChanged(_ => { announced.incrementAndGet(); () })
    /** What a reader can see: each stored row with its per-slot showtimes, and the cache's row. */
    def state: (Seq[(CacheKey, MovieRecord, Map[Source, Seq[Showtime]])], Option[(MovieRecord, Map[Source, Int])]) =
      (repository.findAll().map(r => (r.cacheKey(normalizer), r.record, r.record.data.view.mapValues(_.showtimes).toMap)),
       cache.get(filmKey).map(r => (r, r.data.view.mapValues(ShowtimesDigest.slotDigest).toMap)))
  }

  "putSlotsIfPresent" should "leave the store and the cache exactly where putIfPresent of the same slot does" in {
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
      slotWise.cache.putSlotsIfPresent(filmKey, Seq(source))((_, _) => slot)
        .shouldBe(recordWise.cache.putIfPresent(filmKey, r => r.copy(data = r.data + (source -> slot))))
      withClue(s"after write $step: ")(slotWise.state shouldBe recordWise.state)
      withClue(s"announced after write $step: ")(slotWise.announced.get shouldBe recordWise.announced.get)
    }
  }

  // A venue page read lands on every slot of the row it names, each merged from what that slot held — the detail
  // handler's write, which took the whole row's strip and diff under the title lock: 40% of a US detail
  // drain's CPU, with the drain's other claimants queued behind the lock (JFR, run 37622335270 and after).
  it should "write several slots at once, each from what it held, exactly as putIfPresent of the same update does" in {
    val (slotWise, recordWise) = (new Side, new Side)
    val multikino = CinemaShowing.keyFor(Multikino, "Slot Film", normalizer)
    Seq(slotWise, recordWise).foreach(_.cache.put(filmKey, MovieRecord(imdbRating = Some(7.5), data = Map[Source, SourceData](
      (KinoMuza: Source) -> SourceData(title = Some("Slot Film"), showtimes = Seq(at(1))),
      multikino -> SourceData(title = Some("Slot Film"), showtimes = Seq(at(2))),
      (Helios: Source) -> SourceData(title = Some("Slot Film"), synopsis = Some("untouched"), showtimes = Seq(at(3)))))))
    def merged(synopsis: String)(held: Option[SourceData]): SourceData =
      held.getOrElse(SourceData(title = Some("Slot Film"))).copy(synopsis = Some(synopsis), runtimeMinutes = Some(100))
    val writes: Seq[(Seq[Source], String)] = Seq(
      Seq(KinoMuza, multikino) -> "page",
      Seq(KinoMuza, multikino) -> "page",                       // the same read again: nothing moves
      Seq(multikino, CinemaShowing.keyFor(KinoPalacowe, "Slot Film", normalizer)) -> "page, read again",
      Seq(KinoMuza) -> "page")
    writes.zipWithIndex.foreach { case ((sources, synopsis), step) =>
      slotWise.cache.putSlotsIfPresent(filmKey, sources)((_, held) => merged(synopsis)(held))
        .shouldBe(recordWise.cache.putIfPresent(filmKey, r => r.copy(data = sources.foldLeft(r.data)((d, s) => d + (s -> merged(synopsis)(d.get(s)))))))
      withClue(s"after write $step: ")(slotWise.state shouldBe recordWise.state)
      withClue(s"announced after write $step: ")(slotWise.announced.get shouldBe recordWise.announced.get)
    }
    slotWise.announced.get should be > 0
  }
}
