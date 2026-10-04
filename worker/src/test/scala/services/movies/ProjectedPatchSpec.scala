package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** [[ProjectedPatchCheck]] over the in-memory store, as production stores a film: showtimes in `screenings`, slots in
 *  `movie_slots`. Its Mongo twin is `ProjectedPatchIntegrationSpec`. */
class ProjectedPatchSpec extends AnyFlatSpec with Matchers {

  private def world(): ProjectedPatchCheck.World = {
    val screenings = new CountingScreeningsRepository(new InMemoryScreeningsRepository)
    val slots      = new CountingSlotsRepository(new InMemorySlotsRepository)
    val repository = new InMemoryMovieRepository(screenings = Some(screenings), slots = Some(slots), normalizer = SingleCountryNormalizer.titleNormalizer)
    ProjectedPatchCheck.World(new CaffeineMovieCache(repository, normalizer = SingleCountryNormalizer.titleNormalizer,
      clock = _root_.tools.SpecClock.Pinned), repository, screenings, slots)
  }

  "A projected film's patch" should "store exactly what writing it whole stores, reading neither its screenings nor its slots" in {
    ProjectedPatchCheck.patchesAsWrittenWhole(() => world()) shouldBe Right(())
  }

  it should "write the film whole when the cache no longer holds the record it was patched from" in {
    ProjectedPatchCheck.writesWholeWhenResidentMoved(() => world()) shouldBe Right(())
  }
}
