package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/** `MovieRecord.firstScreeningDate` — the earliest showtime currently listed for
 *  the film, a PURE function of the record's live `data`. Drives the premiere
 *  window in `PremiereResolveReaper`; deliberately NOT a stored field (a stored,
 *  persist-time-only value varied by write/arrival order — see
 *  `StagingOrderDeterminismSpec`), so it's derived fresh from the showtimes,
 *  which converge regardless of arrival order. */
class MovieRecordFirstScreeningDateSpec extends AnyFlatSpec with Matchers {

  private def at(y: Int, mo: Int, d: Int, h: Int): Showtime =
    Showtime(LocalDateTime.of(y, mo, d, h, 0), None)

  private def sourceWith(showtimes: Showtime*): SourceData = SourceData(showtimes = showtimes.toSeq)

  "firstScreeningDate" should "return the earliest showtime across all cinemas" in {
    val record = MovieRecord(data = Map(
      KinoApollo -> sourceWith(at(2026, 6, 10, 20), at(2026, 6, 8, 18)),
      Multikino  -> sourceWith(at(2026, 6, 9, 12), at(2026, 6, 12, 21))
    ))
    record.firstScreeningDate shouldBe Some(LocalDateTime.of(2026, 6, 8, 18, 0))
  }

  it should "return that single showtime when there is only one" in {
    val record = MovieRecord(data = Map(KinoApollo -> sourceWith(at(2026, 6, 8, 18))))
    record.firstScreeningDate shouldBe Some(LocalDateTime.of(2026, 6, 8, 18, 0))
  }

  it should "be None when the record has no showtimes" in {
    MovieRecord(data = Map(KinoApollo -> sourceWith())).firstScreeningDate shouldBe None
    MovieRecord().firstScreeningDate shouldBe None
  }
}
