package services.movies

import models.{CinemaShowing, Kinoteka, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A venue re-billing the SAME cast is not a change. Kinoteka's page for "Rozważna i
 * romantyczna" printed "Fiona Shaw, Daisy Edgar-Jones, Esme Creed-Miles" one day and
 * "Daisy Edgar-Jones, Fiona Shaw, …" the next; every "did this record change" check read
 * that as a rewrite. A cast that GAINS or LOSES a name still is one. One case per site that
 * decides a write, all of which read `SourceData` equality.
 */
class CastChangeDetectionSpec extends AnyFlatSpec with Matchers {

  private val billed    = Seq("Fiona Shaw", "Daisy Edgar-Jones", "Esme Creed-Miles")
  private val rebilled  = Seq("Daisy Edgar-Jones", "Fiona Shaw", "Esme Creed-Miles")
  private val grown     = billed :+ "George MacKay"
  private val slot: Source = CinemaShowing(Kinoteka, "rozwaznairomantycznakinoprzyherbatce")

  private def sd(cast: Seq[String]) = SourceData(title = Some("Rozważna i romantyczna | Kino przy herbatce"), cast = cast)
  private def record(cast: Seq[String]) = MovieRecord(data = Map(slot -> sd(cast)))

  "SourceData equality" should "ignore the order a cast is billed in, and hash alike" in {
    sd(billed) shouldBe sd(rebilled)
    sd(billed).hashCode shouldBe sd(rebilled).hashCode
  }

  it should "still see a cast that gained a name" in {
    sd(billed) should not be sd(grown)
  }

  "the Mongo patch" should "set nothing for a re-billed cast, and the slot for a grown one" in {
    MovieRecordPatch.diff(record(billed), record(rebilled)).isEmpty shouldBe true
    MovieRecordPatch.diff(record(billed), record(grown)).isEmpty shouldBe false
  }

  "the cache write guard" should "skip a re-billed cast, and write a grown one" in {
    ShowtimesDigest.leanEqual(record(billed), record(rebilled)) shouldBe true
    ShowtimesDigest.leanEqual(record(billed), record(grown)) shouldBe false
  }

  "the slot writes" should "emit nothing for a re-billed cast, and an upsert for a grown one" in {
    SlotsRepository.slotOps(record(billed).data, record(rebilled).data) shouldBe empty
    SlotsRepository.slotOps(record(billed).data, record(grown).data).keySet shouldBe Set(slot.displayName)
  }
}
