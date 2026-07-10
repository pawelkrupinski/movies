package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/** `MovieRecord.firstScreeningDateOf` — the min-ever premiere rule that
 *  `MovieCache.persist` applies at the write boundary. */
class MovieRecordFirstScreeningDateSpec extends AnyFlatSpec with Matchers {

  private def at(d: String): Showtime = Showtime(LocalDateTime.parse(d), bookingUrl = None)
  private def withShowtimes(dts: String*): MovieRecord =
    MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(showtimes = dts.map(at))))

  "firstScreeningDateOf" should "set the earliest showtime when a record first gains showtimes" in {
    val r = withShowtimes("2026-08-10T18:00", "2026-08-12T20:00", "2026-08-11T15:00")
    MovieRecord.firstScreeningDateOf(prior = None, r) shouldBe Some(LocalDateTime.parse("2026-08-10T18:00"))
  }

  it should "NOT move later when only a later showtime is now listed" in {
    val prior = Some(LocalDateTime.parse("2026-08-10T18:00"))
    val r     = withShowtimes("2026-08-20T21:00")
    MovieRecord.firstScreeningDateOf(prior, r) shouldBe Some(LocalDateTime.parse("2026-08-10T18:00"))
  }

  it should "move EARLIER when an earlier showtime arrives" in {
    val prior = Some(LocalDateTime.parse("2026-08-10T18:00"))
    val r     = withShowtimes("2026-08-05T12:00", "2026-08-15T20:00")
    MovieRecord.firstScreeningDateOf(prior, r) shouldBe Some(LocalDateTime.parse("2026-08-05T12:00"))
  }

  it should "keep the historical minimum when the earliest showtime is no longer listed" in {
    // The premiere showtime has aged out of `data`; only later ones remain — the
    // remembered `firstScreeningDate` must survive so the true premiere isn't lost.
    val prior = Some(LocalDateTime.parse("2026-08-01T10:00"))
    val r     = withShowtimes("2026-08-25T20:00", "2026-08-26T20:00")
    MovieRecord.firstScreeningDateOf(prior, r) shouldBe Some(LocalDateTime.parse("2026-08-01T10:00"))
  }

  it should "be None for a record with no showtimes and no prior date" in {
    MovieRecord.firstScreeningDateOf(prior = None, MovieRecord()) shouldBe None
  }
}
