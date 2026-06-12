package services.readmodel

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryMovieRepo, StoredMovieRecord, TitleNormalizer}

import java.time.LocalDateTime

/**
 * The projector's minimal-write diff: the whole point of the read-model split
 * is that a showtime-only edit moves one screening doc and a metadata-only edit
 * moves one movie doc. Drives `onMovieUpsert` / `reconcile` directly against the
 * in-memory repos and asserts exactly which docs were written.
 */
class ReadModelProjectorSpec extends AnyFlatSpec with Matchers {

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = Some("https://book"))

  private val fid = s"${TitleNormalizer.sanitize("Foo")}|2024"

  private def slot(showtimes: Seq[Showtime]) =
    SourceData(title = Some("Foo"), releaseYear = Some(2024), filmUrl = Some("https://mk/foo"), showtimes = showtimes)

  private def record(rating: Option[Double], showtimes: Seq[Showtime]): MovieRecord =
    MovieRecord(imdbRating = rating, data = Map[Source, SourceData](Multikino -> slot(showtimes)))

  private def stored(rec: MovieRecord): StoredMovieRecord = StoredMovieRecord("Foo", Some(2024), rec)

  private def fixture(): (ReadModelProjector, InMemoryMovieRepo, InMemoryReadModelRepo) = {
    val repo = new InMemoryMovieRepo()
    val rm   = new InMemoryReadModelRepo()
    (new ReadModelProjector(repo, rm, rm), repo, rm)
  }

  "the first projection of a row" should "write the movie doc before its screenings" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))

    rm.movieUpserts should have size 1
    rm.screeningUpserts should have size 1
    rm.writeOrder.head        should startWith("movie:")
    rm.writeOrder(1)          should startWith("screening:")
    rm.screeningUpserts.head._id shouldBe s"$fid|poznan|Multikino Stary Browar"
  }

  "a showtime-only change" should "move only the one screening doc" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00"), at("2026-06-13T18:00")))))

    rm.movieUpserts     should have size 1  // metadata unchanged → not rewritten
    rm.screeningUpserts should have size 2  // the one changed screening doc, again
  }

  "a rating-only change" should "move only the movie doc" in {
    val (projector, _, rm) = fixture()
    val shows = Seq(at("2026-06-12T20:00"))
    projector.onMovieUpsert(stored(record(Some(8.0), shows)))
    projector.onMovieUpsert(stored(record(Some(9.1), shows)))

    rm.movieUpserts     should have size 2  // rating changed → movie doc rewritten
    rm.screeningUpserts should have size 1  // showtimes unchanged → no screening write
  }

  "a film leaving a cinema" should "delete that cinema's screening doc" in {
    val (projector, _, rm) = fixture()
    projector.onMovieUpsert(stored(record(Some(8.0), Seq(at("2026-06-12T20:00")))))
    projector.onMovieUpsert(stored(record(Some(8.0), Seq.empty)))  // cinema slot now has no showtimes

    rm.screeningDeletes should contain(s"$fid|poznan|Multikino Stary Browar")
  }

  // Boot-cost guard: the full reconcile is a `findAll()` + project-every-row scan.
  // Running it synchronously at `start()` stacked a second full scan onto the cache
  // hydrate + first scrape on a cold JVM (the boot CPU-credit drain). `start()` now
  // seeds state + installs the watch but defers the reconcile to the first scheduled
  // tick — so no source row is projected synchronously. (Before, the boot reconcile
  // projected "Foo" at start(), making movieUpserts size 1 and failing this.)
  "start" should "not reconcile synchronously (defer the full scan off the boot path)" in {
    val (projector, repo, rm) = fixture()
    repo.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.start()
    rm.movieUpserts     shouldBe empty
    rm.screeningUpserts shouldBe empty
    projector.stop()
  }

  "reconcile" should "prune derived docs whose source film vanished" in {
    val (projector, repo, rm) = fixture()
    repo.upsert("Foo", Some(2024), record(Some(8.0), Seq(at("2026-06-12T20:00"))))
    projector.reconcile()
    rm.movieUpserts should have size 1

    repo.delete("Foo", Some(2024))
    projector.reconcile()
    rm.movieDeletes     should contain(fid)
    rm.screeningDeletes should contain(s"$fid|poznan|Multikino Stary Browar")
  }
}
