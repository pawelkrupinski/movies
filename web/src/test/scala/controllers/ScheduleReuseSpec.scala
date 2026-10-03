package controllers

import models.{Helios, KinoApollo, MovieRecord, Poznan, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.{TestReadModel, WebReadModel}

import java.time.LocalDateTime

/**
 * `toSchedules` reuses a film's schedule while nothing it is built from has changed —
 * and only then: a reused schedule must be exactly the one a rebuild produces, so
 * every input that can move (a showtime lapsing, a row rewritten, the day turning)
 * must make it build afresh.
 */
class ScheduleReuseSpec extends AnyFlatSpec with Matchers {

  private val day = LocalDateTime.of(2026, 6, 8, 0, 0)

  private def record(hours: Int*): MovieRecord = MovieRecord(data = Map[Source, SourceData](
    Helios     -> SourceData(title = Some("Milcząca przyjaciółka"), showtimes = hours.map(h => Showtime(day.withHour(h), bookingUrl = None))),
    KinoApollo -> SourceData(title = Some("Milcząca przyjaciółka"), showtimes = Seq(Showtime(day.withHour(21), bookingUrl = None)))))

  private def fixture(hours: Int*) = {
    val repository = TestReadModel.store(Seq(("Milcząca przyjaciółka", Some(2026), record(hours*))))
    val readModel  = new WebReadModel(repository, clock = _root_.tools.SpecClock.Pinned)
    readModel.reload()
    (repository, readModel, new MovieControllerService(readModel, clock = TestMovieController.clock))
  }

  /** What a service that has never built anything answers for the same read model. */
  private def fresh(readModel: WebReadModel, now: LocalDateTime) =
    new MovieControllerService(readModel, clock = TestMovieController.clock).toSchedules(Poznan, now)

  "toSchedules" should "hand back the same schedule while nothing it was built from has moved" in {
    val (_, _, service) = fixture(15, 18)
    val first  = service.toSchedules(Poznan, day.withHour(9))
    val second = service.toSchedules(Poznan, day.withHour(10))
    second should have size 1
    second.head should be theSameInstanceAs first.head
  }

  it should "build afresh once a showtime has lapsed" in {
    val (_, readModel, service) = fixture(15, 18)
    service.toSchedules(Poznan, day.withHour(9))
    val later = day.withHour(16)                     // 15:00 + the 30-min grace has passed
    val after = service.toSchedules(Poznan, later)
    after shouldBe fresh(readModel, later)
    after.head.showings.head._2.flatMap(_.showtimes).map(_.dateTime.getHour) should not contain 15
  }

  it should "build afresh once the film's rows are rewritten" in {
    val (repository, readModel, service) = fixture(15, 18)
    val before = service.toSchedules(Poznan, day.withHour(9))
    TestReadModel.store(Seq(("Milcząca przyjaciółka", Some(2026), record(15, 19)))).findAllScreenings()
      .foreach(repository.upsertScreening)
    readModel.reload()
    val after = service.toSchedules(Poznan, day.withHour(9))
    after.head should not be theSameInstanceAs (before.head)
    after shouldBe fresh(readModel, day.withHour(9))
  }

  it should "build afresh when the day turns, whose date labels count from it" in {
    val (_, readModel, service) = fixture(15, 18)
    val before = service.toSchedules(Poznan, day.minusHours(1))      // the evening before
    val after  = service.toSchedules(Poznan, day.withHour(9))
    after.head should not be theSameInstanceAs (before.head)
    after shouldBe fresh(readModel, day.withHour(9))
  }
}
