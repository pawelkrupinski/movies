package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{CaffeineMovieCache, CountingScreeningsRepository, CountingSlotsRepository, MongoMovieRepository,
  MongoScreeningsRepository, MongoSlotsRepository, ProjectedPatchCheck}

/** [[ProjectedPatchCheck]] over Mongo: a projected film's patch lands every screening, slot row and field a whole write
 *  would, without reading the film back. Requires MONGODB_URI; skips otherwise. */
class ProjectedPatchIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  /** `check` over worlds each on a database of its own — the patched film and the film written whole share no row —
   *  dropped once it is done. */
  private def checked(check: (() => ProjectedPatchCheck.World) => Either[String, Unit]): Unit =
    tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "projected-patch-a") { a =>
      tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "projected-patch-b") { b =>
        val databases = scala.collection.mutable.Queue(a, b)
        val opened    = scala.collection.mutable.ListBuffer.empty[MongoMovieRepository]
        def world(): ProjectedPatchCheck.World = {
          val db         = databases.dequeue()
          val screenings = new CountingScreeningsRepository(new MongoScreeningsRepository(Some(db)))
          val slots      = new CountingSlotsRepository(new MongoSlotsRepository(Some(db)))
          val repository = new MongoMovieRepository(Some(db), _root_.tools.SpecClock.Pinned, screenings = Some(screenings),
            slots = Some(slots), normalizer = titleNormalizer)
          opened += repository
          ProjectedPatchCheck.World(new CaffeineMovieCache(repository, normalizer = titleNormalizer, clock = _root_.tools.SpecClock.Pinned),
            repository, screenings, slots)
        }
        try check(() => world()) shouldBe Right(())
        finally opened.foreach(_.close())
      }
    }

  "A projected film's patch" should "store exactly what writing it whole stores, reading neither its screenings nor its slots" in
    checked(ProjectedPatchCheck.patchesAsWrittenWhole)

  it should "write the film whole when the cache no longer holds the record it was patched from" in
    checked(ProjectedPatchCheck.writesWholeWhenResidentMoved)

  it should "write no slot row for a patch that moves only a venue's showtimes" in {
    checked(ProjectedPatchCheck.movesShowtimesWithoutSlotWrites)
  }
}
