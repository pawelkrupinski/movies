package controllers

import models.{Helios, KinoApollo, MovieRecord, Poznan, Rialto, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

import java.time.LocalDateTime

/** When one film shows at several of a city's cinemas at the SAME earliest
 *  time, the per-cinema ordering must be deterministic. Ordering by time
 *  alone leaves tied cinemas in the upstream map's iteration order, which in
 *  production tracks scrape-merge thread timing — non-deterministic across
 *  JVM boots and the source of the "Kino Malta vs Kino Meduza" whole-corpus
 *  snapshot flake. The fix adds `cinema.displayName` as the sort tiebreaker.
 */
class ScheduleOrderingSpec extends AnyFlatSpec with Matchers {

  private val showAt = LocalDateTime.of(2026, 6, 8, 18, 0)
  private val now    = LocalDateTime.of(2026, 6, 8, 0, 0)

  private def slot(): SourceData =
    SourceData(title = Some("Milcząca przyjaciółka"),
               showtimes = Seq(Showtime(showAt, bookingUrl = None)))

  "toSchedules" should "order tied-time cinemas by displayName regardless of map insertion order" in {
    // Inserted in REVERSE displayName order ("Kino Rialto", "Kino Apollo",
    // "Helios Posnania"). Before the fix the output followed this insertion
    // order — exactly the iteration-order dependence that flips between runs;
    // after the fix it is always displayName-sorted.
    val record = MovieRecord(data = Map[Source, SourceData](
      Rialto     -> slot(),
      KinoApollo -> slot(),
      Helios     -> slot(),
    ))
    val service = new MovieControllerService(
      TestReadModel.fromRecords(Seq(("Milcząca przyjaciółka", Some(2026), record))))

    val schedules = service.toSchedules(Poznan, now)
    schedules should have size 1
    val firstDayCinemas = schedules.head.showings.head._2.map(_.cinema.displayName)
    firstDayCinemas shouldBe Seq("Helios Posnania", "Kino Apollo", "Kino Rialto")
  }
}
