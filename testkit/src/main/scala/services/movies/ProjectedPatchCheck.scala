package services.movies

import models.{CinemaShowing, Helios, KinoMuranow, KinoMuza, MovieRecord, Multikino, Showtime, Source, SourceData}

import java.time.LocalDateTime

/**
 * What `MovieCache.patchProjected` must do, over any store with the showtimes and slots split: leave the store exactly as
 * writing the film whole would — every screening, slot row and field — while reading neither the film's screenings nor
 * its slots back. One check, run over the in-memory store (`ProjectedPatchSpec`) and over Mongo
 * (`ProjectedPatchIntegrationSpec`), so the two cannot be held to different rules.
 *
 * `world()` makes a fresh cache over a fresh store, with the counting decorators between them.
 */
object ProjectedPatchCheck {

  final case class World(cache: CaffeineMovieCache, repository: MovieRepository,
                         screenings: CountingScreeningsRepository, slots: CountingSlotsRepository)

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val id         = FilmId("fprojectedpatch")
  private val key        = CacheKey.stored("Projected Patch", "projectedpatch|2026")
  /** Showtimes well ahead of any clock a store runs on: none is past, so none is pruned or stripped as one. */
  private val at         = LocalDateTime.of(2036, 10, 7, 18, 0)

  private def slot(cinema: models.Cinema, hours: Int*): (Source, SourceData) =
    CinemaShowing.keyFor(cinema, "Projected Patch", normalizer) ->
      SourceData(title = Some("Projected Patch"), releaseYear = Some(2026), showtimes = hours.map(h => Showtime(at.plusHours(h.toLong), None)))

  private val first = MovieRecord(data = Map(slot(Multikino, 0, 2), slot(KinoMuranow, 1), slot(Helios, 3)))

  /** The film as the projection would next write it, over the record `cache` holds: Multikino's showtimes moved,
   *  Kino Muranow gone, Kino Muza new, Helios as held (lean). */
  private def next(resident: MovieRecord): MovieRecord = resident.copy(imdbRating = Some(7.5), data =
    resident.data - slot(KinoMuranow)._1 + slot(Multikino, 0, 5) + slot(KinoMuza, 4))

  /** Everything the store holds for the film: its record (read back stitched), and every slot's showtimes. */
  private def held(w: World): (Option[MovieRecord], Map[String, Seq[LocalDateTime]]) = {
    val stored = w.repository.findByIdChecked(id).answered
    (stored.map(_.record.copy(data = Map.empty)),
      stored.fold(Map.empty[String, Seq[LocalDateTime]])(_.record.data.collect {
        case (source: CinemaShowing, sd) => source.displayName -> sd.showtimes.map(_.dateTime)
      }))
  }

  /** The patch against the whole write; `Left` names the first difference. */
  def patchesAsWrittenWhole(world: () => World): Either[String, Unit] = {
    val (patched, whole) = (world(), world())
    Seq(patched, whole).foreach(_.cache.writeProjected(id, key, first))
    def resident(w: World) = w.cache.snapshot().find(_.id == id).map(_.record).get
    val before = resident(patched)
    patched.screenings.reset(); patched.slots.reset()
    val outcome = patched.cache.patchProjected(id, key, before, next(before))
    whole.cache.writeProjected(id, key, next(resident(whole)))
    if (outcome != WriteOutcome.Written) Left(s"the patch reported $outcome")
    else if (patched.screenings.filmReadCalls.get() + patched.slots.filmReadCalls.get() != 0)
      Left(s"the patch read the film back: ${patched.screenings.filmReadCalls.get()} screenings read(s), " +
        s"${patched.slots.filmReadCalls.get()} slots read(s)")
    else if (held(patched) != held(whole)) Left(s"the store differs —\n  patched: ${held(patched)}\n  whole:   ${held(whole)}")
    else if (!ShowtimesDigest.leanEqual(resident(patched), resident(whole))) Left("the cache's rows differ")
    else Right(())
  }

  /** A patch from a record the cache no longer holds is written whole — the patch would be from the wrong state. */
  def writesWholeWhenResidentMoved(world: () => World): Either[String, Unit] = {
    val w = world()
    w.cache.writeProjected(id, key, first)
    val stale = w.cache.snapshot().find(_.id == id).map(_.record).get
    w.cache.putIfPresent(key, _.copy(metascore = Some(61)))
    w.screenings.reset(); w.slots.reset()
    val outcome = w.cache.patchProjected(id, key, stale, next(stale))
    val stored  = held(w)
    if (outcome != WriteOutcome.Written) Left(s"the write reported $outcome")
    else if (w.screenings.filmReadCalls.get() == 0) Left("the moved film was patched from a record the cache no longer held")
    else if (stored._2.keySet != next(stale).data.keySet.collect { case s: CinemaShowing => s.displayName })
      Left(s"the whole write left the wrong venues: ${stored._2.keySet}")
    else Right(())
  }
}
