package scripts

import models.{CinemaShowing, KinoMuranow, Kinoteka, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.ListingKeyBackfill._
import services.movies.{ListingKey, SlotKeyed}

/**
 * The backfill's plan, pinned apart from the Mongo scan around it: which rows get which
 * `listingKey`, which cannot get one, and which slots claim the same listing.
 */
class ListingKeyBackfillSpec extends AnyFlatSpec with Matchers {

  private val belle    = CinemaShowing(KinoMuranow, "belle").displayName
  private val kinoteka = CinemaShowing(Kinoteka, "belle").displayName
  private val slot     = SourceData(title = Some("Belle"), rawTitle = Some("Belle (2013)"), releaseYear = Some(2013))
  private val belleKey = ListingKey.serialised(ListingKey.Published(KinoMuranow.displayName, "Belle (2013)", Some(2013), Nil))

  private def slotRow(film: String, slotKey: String, sd: SourceData = slot, stored: Option[String] = None) =
    SlotRow(SlotKeyed.idOf(film, slotKey), film, slotKey, sd, stored)
  private def screeningRow(film: String, slotKey: String, stored: Option[String] = None) =
    ScreeningRow(SlotKeyed.idOf(film, slotKey), film, slotKey, stored)

  "plan" should "stamp a slot and its screenings row with the key the slot derives" in {
    val p = plan(Seq(slotRow("belle|2013", belle)), Seq(screeningRow("belle|2013", belle)))
    p.slotUpdates shouldBe Seq(Update(SlotKeyed.idOf("belle|2013", belle), None, belleKey))
    p.screeningUpdates shouldBe Seq(Update(SlotKeyed.idOf("belle|2013", belle), None, belleKey))
  }

  it should "leave rows that already carry the right key alone, and correct one that carries a stale key" in {
    val p = plan(Seq(slotRow("a", belle, stored = Some(belleKey)), slotRow("b", belle, stored = Some("stale"))),
                 Seq(screeningRow("a", belle, stored = Some(belleKey)), screeningRow("b", belle, stored = Some("stale"))))
    p.slotUpdates.map(_.id) shouldBe Seq(SlotKeyed.idOf("b", belle))
    p.screeningUpdates shouldBe Seq(Update(SlotKeyed.idOf("b", belle), Some("stale"), belleKey))
  }

  it should "count, not stamp, what has no venue listing: an enrichment slot, a retired venue, a screening without its slot" in {
    val p = plan(Seq(slotRow("a", Tmdb.displayName), slotRow("a", "Kino Gone␟belle")),
                 Seq(screeningRow("a", "Kino Gone␟belle"), screeningRow("z", belle)))
    p.slotUpdates shouldBe empty
    p.screeningUpdates shouldBe empty
    p.slotsWithoutListing shouldBe 2
    p.screeningsWithoutListing shouldBe Seq(SlotKeyed.idOf("a", "Kino Gone␟belle"), SlotKeyed.idOf("z", belle))
  }

  "collisions" should "name every listing key more than one slot row claims, across films and within one" in {
    val other = slot.copy(title = Some("Belle"))           // same listing fields, another film's slot
    val p = plan(Seq(slotRow("belle|2013", belle), slotRow("belle|2021", belle, other), slotRow("x", kinoteka)), Nil)
    p.collisions shouldBe Seq(Collision(belleKey, Seq("belle|2013" -> belle, "belle|2021" -> belle)))
    p.collisions.head.acrossFilms shouldBe true
  }

  "filmListings" should "be each film's set of listing keys, the previous assignment ID seeding starts from" in {
    val p = plan(Seq(slotRow("f1", belle), slotRow("f1", Tmdb.displayName), slotRow("f2", kinoteka)), Nil)
    p.filmListings shouldBe Map(
      "f1" -> Set(belleKey),
      "f2" -> Set(ListingKey.serialised(ListingKey.Published(Kinoteka.displayName, "Belle (2013)", Some(2013), Nil))))
  }
}
