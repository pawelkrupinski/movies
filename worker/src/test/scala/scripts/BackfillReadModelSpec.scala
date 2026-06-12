package scripts

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepo, StoredMovieRecord}
import services.readmodel.{InMemoryReadModelRepo, ReadModelProjection}

import java.time.LocalDateTime

class BackfillReadModelSpec extends AnyFlatSpec with Matchers {

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = Some("https://book"))

  private def record(title: String, year: Int): MovieRecord =
    MovieRecord(
      imdbRating = Some(8.0),
      data = Map[Source, SourceData](
        Multikino -> SourceData(title = Some(title), releaseYear = Some(year), filmUrl = Some("https://mk"), showtimes = Seq(at("2026-06-12T20:00")))
      )
    )

  private def filmId(title: String, year: Int): String =
    ReadModelProjection.filmId(StoredMovieRecord(title, Some(year), record(title, year)))

  // Seed the read model with a film that no longer exists in `movies` — the
  // backfill must prune it.
  private def seedStale(readModel: InMemoryReadModelRepo): Unit = {
    val stale = StoredMovieRecord("Stale", Some(2000), record("Stale", 2000))
    readModel.upsertMovie(ReadModelProjection.resolve(stale))
    ReadModelProjection.screenings(stale).foreach(readModel.upsertScreening)
  }

  "BackfillReadModel.run" should "populate the read model from movies and prune stale derived docs" in {
    val movieRepo = new InMemoryMovieRepo(Seq(("Foo", Some(2024), record("Foo", 2024))))
    val readModel = new InMemoryReadModelRepo()
    seedStale(readModel)

    val (movies, screenings, prunedM, prunedS) = BackfillReadModel.run(movieRepo, readModel)

    movies     shouldBe 1
    screenings shouldBe 1
    prunedM    shouldBe 1
    prunedS    shouldBe 1

    readModel.findAllMovies().map(_._id)     should contain only filmId("Foo", 2024)
    readModel.findAllScreenings().map(_.filmId) should contain only filmId("Foo", 2024)
  }

  it should "be idempotent — a second run writes the same docs and prunes nothing" in {
    val movieRepo = new InMemoryMovieRepo(Seq(("Foo", Some(2024), record("Foo", 2024))))
    val readModel = new InMemoryReadModelRepo()

    BackfillReadModel.run(movieRepo, readModel)
    val (movies, screenings, prunedM, prunedS) = BackfillReadModel.run(movieRepo, readModel)

    movies     shouldBe 1
    screenings shouldBe 1
    prunedM    shouldBe 0
    prunedS    shouldBe 0
    readModel.findAllMovies()     should have size 1
    readModel.findAllScreenings() should have size 1
  }
}
