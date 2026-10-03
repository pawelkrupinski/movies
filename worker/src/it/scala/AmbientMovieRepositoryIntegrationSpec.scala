package scripts

import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{FilmId, MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository}
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The read-and-patch scripts' store reads a film as the worker wrote it. Under the read/write split
 * a film's `movies` document holds neither its title nor its slots (both live in `movie_slots`, the
 * showtimes in `screenings`), so a store without the side collections reads a title derived from the
 * `_id` and no venue at all — and a backfill patching that row writes it back.
 */
class AmbientMovieRepositoryIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val when = java.time.LocalDateTime.parse("2031-06-12T18:00")

  "AmbientMovieRepository" should "read a film's title, slots and showtimes as the worker's split store wrote them" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "ambient-movie-repository") { db =>
      val worker = new MongoMovieRepository(Some(db), screenings = Some(new MongoScreeningsRepository(Some(db))),
        slots = Some(new MongoSlotsRepository(Some(db))), normalizer = titleNormalizer)
      worker.upsert("Anora", Some(2024), MovieRecord(tmdbId = Some(1064213), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Anora"), filmUrl = Some("https://mk/anora"), showtimes = Seq(Showtime(when, None))))))
      val id: FilmId = worker.findAll().head.id

      val script = AmbientMovieRepository.over(Some(db), titleNormalizer)
      val read   = script.findById(id).getOrElse(fail(s"the script's store does not find $id"))
      read.title shouldBe "Anora"
      read.record.data.get(Multikino).flatMap(_.filmUrl) shouldBe Some("https://mk/anora")
      read.record.data(Multikino).showtimes.map(_.dateTime) shouldBe Seq(when)
    }
  }
}
