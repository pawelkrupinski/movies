package scripts

import services.movies.SingleCountryNormalizer.titleNormalizer

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepository, StoredMovieRecord}
import services.readmodel.{InMemoryReadModelRepository, ReadModelProjection}

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
    ReadModelProjection.filmId(StoredMovieRecord(title, Some(year), record(title, year)), titleNormalizer)

  // Seed the read model with a film that no longer exists in `movies` — the
  // backfill must prune it.
  private def seedStale(readModel: InMemoryReadModelRepository): Unit = {
    val stale = StoredMovieRecord("Stale", Some(2000), record("Stale", 2000))
    readModel.upsertMovie(ReadModelProjection.resolve(stale, titleNormalizer))
    ReadModelProjection.screenings(stale, titleNormalizer).foreach(readModel.upsertScreening)
  }

  "BackfillReadModel.run" should "populate the read model from movies and prune stale derived documents" in {
    val movieRepository = new InMemoryMovieRepository(Seq(("Foo", Some(2024), record("Foo", 2024))))
    val readModel = new InMemoryReadModelRepository()
    seedStale(readModel)

    val (movies, screenings, prunedM, prunedS) = BackfillReadModel.run(movieRepository, readModel)

    movies     shouldBe 1
    screenings shouldBe 1
    prunedM    shouldBe 1
    prunedS    shouldBe 1

    readModel.findAllMovies().map(_._id)     should contain only filmId("Foo", 2024)
    readModel.findAllScreenings().map(_.filmId) should contain only filmId("Foo", 2024)
  }

  it should "be idempotent — a second run writes the same documents and prunes nothing" in {
    val movieRepository = new InMemoryMovieRepository(Seq(("Foo", Some(2024), record("Foo", 2024))))
    val readModel = new InMemoryReadModelRepository()

    BackfillReadModel.run(movieRepository, readModel)
    val (movies, screenings, prunedM, prunedS) = BackfillReadModel.run(movieRepository, readModel)

    movies     shouldBe 1
    screenings shouldBe 1
    prunedM    shouldBe 0
    prunedS    shouldBe 0
    readModel.findAllMovies()     should have size 1
    readModel.findAllScreenings() should have size 1
  }

  // 2026-08-10: run against prod with `MongoMovieRepository`'s `screenings`/`slots`
  // side repositories UNWIRED. Showtimes live in the `screenings` collection and are
  // stitched back only when that repository is passed, so every row came back with no
  // showtimes, the projection produced no screenings, and the prune deleted the live
  // `web_screenings` — Poznań fell from 209 films to 28. Projecting nothing is evidence
  // the reader is wrong, never that the corpus emptied.
  it should "never prune screenings when it projected none (a showtime-less read)" in {
    // A record whose slot carries metadata but NO showtimes — exactly the shape an
    // unstitched read returns.
    val showtimeless = MovieRecord(data = Map[Source, SourceData](
      Multikino -> SourceData(title = Some("Foo"), releaseYear = Some(2024), showtimes = Seq.empty)
    ))
    val movieRepository = new InMemoryMovieRepository(Seq(("Foo", Some(2024), showtimeless)))
    val readModel = new InMemoryReadModelRepository()
    // The live read model already holds this film's screenings, written by the projector.
    val live = StoredMovieRecord("Foo", Some(2024), record("Foo", 2024))
    ReadModelProjection.screenings(live, titleNormalizer).foreach(readModel.upsertScreening)
    readModel.findAllScreenings() should have size 1

    val (_, screenings, _, prunedS) = BackfillReadModel.run(movieRepository, readModel)

    screenings shouldBe 0
    prunedS    shouldBe 0
    readModel.findAllScreenings() should have size 1   // the live screening survives
  }

  it should "never prune films when it projected none (an empty corpus read)" in {
    val readModel = new InMemoryReadModelRepository()
    seedStale(readModel)
    readModel.findAllMovies() should have size 1

    val (movies, _, prunedM, _) = BackfillReadModel.run(new InMemoryMovieRepository(Seq.empty), readModel)

    movies  shouldBe 0
    prunedM shouldBe 0
    readModel.findAllMovies() should have size 1
  }
}
