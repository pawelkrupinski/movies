package services.movies

import models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * `slotOps` turns a before/after pair into the `screenings` writes that carry it, and one of
 * those writes is a DELETE. It decides to delete from `after.showtimes.isEmpty` — but under
 * the read-split an empty list does not mean "no showtimes". A record resident in the cache
 * has been through `ShowtimesDigest.stripForCache`, which drops every list and keeps only a
 * digest; "stripped" and "genuinely empty" look identical to `isEmpty`.
 *
 * That matters because `MovieCache.putIfPresent` — the path every rating refresh and every
 * per-slot scrape update takes — hands `updateIfPresent` exactly those cache records. So an
 * update whose digest moved for any reason would delete the film's screenings for that
 * cinema, having been handed a record that never carried them in the first place.
 *
 * The digest is the field that tells the two apart, and `slotOps` already computes it.
 */
class SlotOpsStrippedRecordSpec extends AnyFlatSpec with Matchers {

  private val showtimes = Seq(
    Showtime(LocalDateTime.of(2026, 8, 1, 18, 0), None),
    Showtime(LocalDateTime.of(2026, 8, 1, 20, 30), None))

  private def slot(times: Seq[Showtime]) = SourceData(title = Some("Sirat"), showtimes = times)
  private def record(sd: SourceData)     = MovieRecord(data = Map[Source, SourceData](Multikino -> sd))

  "slotOps" should "not delete a cinema's screenings because the record was stripped for the cache" in {
    val before  = record(slot(showtimes))
    // The same slot as the cache holds it: no resident list, digest intact.
    val after   = ShowtimesDigest.stripForCache(record(slot(showtimes.tail)))

    val ops = ScreeningsRepository.slotOps(before.data, after.data)

    // Nothing to write, and above all nothing to delete: the digest moved, but a stripped
    // record cannot say what to. The whole-record path carries it.
    withClue(s"ops=$ops — `None` is a DELETE of every showtime this cinema has: ")(
      ops shouldBe empty)
  }

  it should "still delete when the slot genuinely has no showtimes left" in {
    val before = record(slot(showtimes))
    val after  = record(slot(Seq.empty))       // scraped, present, and screening nothing

    ScreeningsRepository.slotOps(before.data, after.data) shouldBe
      Map(Multikino.displayName -> None)
  }

  it should "write the showtimes when the record actually carries them" in {
    val before = record(slot(showtimes.tail))
    val after  = record(slot(showtimes))

    ScreeningsRepository.slotOps(before.data, after.data) shouldBe
      Map(Multikino.displayName -> Some(showtimes))
  }

  it should "stay silent when a stripped record's digest matches — the common case" in {
    val before = record(slot(showtimes))
    val after  = ShowtimesDigest.stripForCache(record(slot(showtimes)))

    ScreeningsRepository.slotOps(before.data, after.data) shouldBe empty
  }
}
