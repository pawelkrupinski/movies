package services.readmodel

import models.{Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer
import services.movies.{InMemoryMovieRepository, MovieRepository, StoredMovieRecord}
import tools.costs.{CostScaling, Work}

/**
 * Projecting ONE changed film costs the same whatever the corpus holds.
 *
 * The change stream calls `onMovieUpsert` once per changed row, all day; a projection that
 * read the corpus — or rewrote cards it did not change — per event is the quadratic the
 * `ProjectionWorkDisproportionate` alert watches for in production. This pins it before it
 * ships: rows read from the source and the read model, plus documents written, for one
 * film's rating change against a corpus of N and of 4N.
 */
class ReadModelProjectionCostSpec extends AnyFlatSpec with Matchers {

  private def film(n: Int, rating: Double): (String, Option[Int], MovieRecord) = {
    val title = s"Projected Film $n"
    (title, Some(2026), MovieRecord(imdbRating = Some(rating), tmdbId = Some(n),
      data = Map[Source, SourceData](Helios -> SourceData(title = Some(title), releaseYear = Some(2026), filmUrl = Some(s"https://h/$n")))))
  }

  private def oneFilmsProjection(corpus: Int): Long = {
    val repository = new InMemoryMovieRepository((1 to corpus).map(film(_, 6.0)), normalizer = titleNormalizer)
    val readModel  = new InMemoryReadModelRepository
    val work       = new Work
    val projector  = new ReadModelProjector(
      Work.counting(classOf[MovieRepository], repository, work),
      Work.counting(classOf[ReadModelWriter], readModel, work),
      Work.counting(classOf[ReadModelReader], readModel, work), clock = tools.SpecClock.Pinned)
    projector.reconcile()
    readModel.findAllMovies() should have size corpus.toLong
    work.reset()

    val (title, year, changed) = film(1, 7.5)
    repository.upsert(title, year, changed)
    projector.onMovieUpsert(StoredMovieRecord.synthesised(title, year, changed, services.movies.SingleCountryNormalizer.titleNormalizer))
    readModel.findAllMovies().find(_.title == title).flatMap(_.ratings.imdb) shouldBe Some(7.5)
    work.total
  }

  "onMovieUpsert" should "cost the same for one film whatever the corpus holds" in
    CostScaling.assertIndependent("rows read + documents written projecting one film's rating change", n = 25)(oneFilmsProjection)
}
