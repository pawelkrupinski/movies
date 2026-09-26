package services.movies

import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * The by-id read a cards-only derivation pass makes: the film with its cinema slots stitched from
 * `movie_slots`, and WITHOUT the `screenings` read — which is most of what a whole-row read pulls
 * (US: 260 of 341 MB, 2026-09-26) and nothing a card reads. Against a real split store, because
 * the in-memory one has no side collections to skip.
 */
class FindByIdWithSlotsIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val when = java.time.LocalDateTime.parse("2031-06-12T18:00")

  "findByIdWithSlotsChecked" should "return the film with its slots and no showtimes, where findByIdChecked has both" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "find-by-id-with-slots") { db =>
      val repository = new MongoMovieRepository(Some(db), screenings = Some(new MongoScreeningsRepository(Some(db))),
        slots = Some(new MongoSlotsRepository(Some(db))), normalizer = titleNormalizer)
      repository.upsert("Anora", Some(2024), MovieRecord(tmdbId = Some(1064213), data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some("Anora"), filmUrl = Some("https://mk/anora"), showtimes = Seq(Showtime(when, None))))))
      val id = repository.findAll().head.id

      val (whole, wholeRead)         = repository.findByIdChecked(id)
      val (slotsOnly, slotsOnlyRead) = repository.findByIdWithSlotsChecked(id)

      wholeRead shouldBe true
      slotsOnlyRead shouldBe true
      withClue("the whole read stitches the showtime back from `screenings`: ") {
        whole.get.record.data(Multikino).showtimes.map(_.dateTime) shouldBe Seq(when)
      }
      withClue("the slots-only read has the slot, from `movie_slots`: ") {
        slotsOnly.get.record.data(Multikino).filmUrl shouldBe Some("https://mk/anora")
      }
      withClue("…and skipped `screenings`: ") {
        slotsOnly.get.record.data(Multikino).showtimes shouldBe empty
      }
      slotsOnly.get.title shouldBe whole.get.title
    }
  }
}
