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

    val ops = ScreeningsSplit.slotOps(before.data, after.data)

    // Nothing to write, and above all nothing to delete: the digest moved, but a stripped
    // record cannot say what to. The whole-record path carries it.
    withClue(s"ops=$ops — `None` is a DELETE of every showtime this cinema has: ")(
      ops shouldBe empty)
  }

  it should "still delete when the slot genuinely has no showtimes left" in {
    val before = record(slot(showtimes))
    val after  = record(slot(Seq.empty))       // scraped, present, and screening nothing

    ScreeningsSplit.slotOps(before.data, after.data) shouldBe
      Map(Multikino.displayName -> None)
  }

  it should "write the showtimes when the record actually carries them" in {
    val before = record(slot(showtimes.tail))
    val after  = record(slot(showtimes))

    ScreeningsSplit.slotOps(before.data, after.data) shouldBe
      Map(Multikino.displayName -> Some(ListedShowtimes(showtimes, Some(ListingKey.Published(Multikino.displayName, "Sirat", None, Nil)))))
  }

  it should "stay silent when a stripped record's digest matches — the common case" in {
    val before = record(slot(showtimes))
    val after  = ShowtimesDigest.stripForCache(record(slot(showtimes)))

    ScreeningsSplit.slotOps(before.data, after.data) shouldBe empty
  }
}

/**
 * `slotOps` writes a slot only when its showtimes digest moved, so a venue whose SOURCE data
 * changed under unchanged showtimes — moved from Filmweb to its own-site scraper ("Flavia de Luce
 * - KNT" became "Flavia de Luce"), or a client that stopped emitting a trailing ")" — would keep
 * the `screenings` row's old `listingKey` forever: `movie_slots` is rewritten every scrape, the
 * screenings row never, and the shadow read counts the pair as `screenings_disagree`.
 *
 * `writesFor` carries those as RESTAMPS — the new key alone, never showtimes — because the
 * record handed in may be stripped for the cache and so cannot say what the showtimes are.
 */
class ScreeningsRestampSpec extends AnyFlatSpec with Matchers {

  private val showtimes = Seq(
    Showtime(LocalDateTime.of(2026, 8, 1, 18, 0), None),
    Showtime(LocalDateTime.of(2026, 8, 1, 20, 30), None))

  private def slot(raw: String, times: Seq[Showtime] = showtimes) =
    SourceData(title = Some("Sirat"), rawTitle = Some(raw), showtimes = times)
  private def record(sd: SourceData) = MovieRecord(data = Map[Source, SourceData](Multikino -> sd))
  private def keyOf(raw: String)     = ListingKey.Published(Multikino.displayName, raw, None, Nil)

  "writesFor" should "restamp a slot whose listing key moved while its showtimes did not" in {
    val writes = ScreeningsSplit.writesFor(record(slot("Sirat - KNT")).data, record(slot("Sirat")).data)
    writes.rows shouldBe empty
    writes.restamps shouldBe Map(Multikino.displayName -> keyOf("Sirat"))
  }

  it should "restamp nothing when the key did not move" in {
    val writes = ScreeningsSplit.writesFor(record(slot("Sirat")).data, record(slot("Sirat").copy(runtimeMinutes = Some(120))).data)
    writes.isEmpty shouldBe true
  }

  it should "restamp a STRIPPED record's slot without writing any showtimes" in {
    val before = ShowtimesDigest.stripForCache(record(slot("Sirat - KNT")))
    val after  = ShowtimesDigest.stripForCache(record(slot("Sirat")))
    val writes = ScreeningsSplit.writesFor(before.data, after.data)
    withClue("a row write from a stripped record carries NO showtimes and would wipe the cinema's screenings: ")(
      writes.rows shouldBe empty)
    writes.restamps shouldBe Map(Multikino.displayName -> keyOf("Sirat"))
  }

  it should "restamp a stripped slot whose showtimes also moved, since the record cannot say to what" in {
    val before = record(slot("Sirat - KNT"))
    val after  = ShowtimesDigest.stripForCache(record(slot("Sirat", showtimes.tail)))
    val writes = ScreeningsSplit.writesFor(before.data, after.data)
    writes.rows shouldBe empty
    writes.restamps shouldBe Map(Multikino.displayName -> keyOf("Sirat"))
  }

  it should "leave the key to the row write when the showtimes are written anyway" in {
    val writes = ScreeningsSplit.writesFor(record(slot("Sirat - KNT", showtimes.tail)).data, record(slot("Sirat")).data)
    writes.rows shouldBe Map(Multikino.displayName -> Some(ListedShowtimes(showtimes, Some(keyOf("Sirat")))))
    writes.restamps shouldBe empty
  }

  it should "not restamp a slot that screens nothing, which has no screenings row to restamp" in {
    val writes = ScreeningsSplit.writesFor(record(slot("Sirat - KNT", Nil)).data, record(slot("Sirat", Nil)).data)
    writes.isEmpty shouldBe true
  }
}
