package services.movies

import models.{CinemaShowing, Country, KinoEtiuda, KinoMiescisko, KinoOOK, KinoStarowka, KinoWawrzyn, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

import java.time.LocalDateTime

/** The Mongo half of the retired-venue sweep: the `_id`-projected id read and the `$in`
 *  delete against the real `screenings` / `movie_slots` collections, on a database of its
 *  own that is dropped afterwards. The rule itself is `RetiredVenueRowsSpec`'s; this proves
 *  the stores answer it — the retired venue's rows go, a live venue's rows (including the
 *  prefix-sharing "Kino Etiuda") stay, and a second sweep is a no-op. */
class RetiredVenueRowsIntegrationSpec extends AnyFlatSpec with Matchers {
  private val uri = Env.get("MONGODB_URI").get

  private val tomorrow = Seq(Showtime(LocalDateTime.now.plusDays(1).withNano(0), bookingUrl = None))
  private val Retired  = "Kino Etiuda OBK"
  private val live     = Seq(KinoEtiuda, KinoOOK, KinoStarowka, KinoWawrzyn, KinoMiescisko)

  "RetiredVenueRows.sweep" should "remove exactly the rows of a venue the roster no longer lists" in
    tools.IntegrationCorpusDatabase.withDatabase(uri, "retired-venue-rows") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db))
      def seed(filmId: String, slotKey: String): Unit = {
        screenings.upsertSlot(filmId, slotKey, tomorrow)
        slots.upsertSlot(filmId, slotKey, SourceData(title = Some(filmId)))
      }
      try {
        live.foreach(c => seed("live|2026", CinemaShowing(c, "live").displayName))
        seed("live|2026", s"$Retired${CinemaShowing.Separator}live")
        seed("other|2025", s"$Retired${CinemaShowing.Separator}other")
        seed("other|2025", CinemaShowing(KinoEtiuda, "other").displayName)

        val (screeningIdsBefore, _) = screenings.rowIdsChecked()
        val (slotIdsBefore, _)      = slots.rowIdsChecked()
        screeningIdsBefore should have size 8

        RetiredVenueRows.sweep(Some(screenings), Some(slots), RetiredVenueRows.rosterOf(Country.Poland)) shouldBe
          RetiredVenueRows(screenings = 2, slots = 2, venues = Map(Retired -> 4L))

        screenings.rowIdsChecked() shouldBe ((screeningIdsBefore.filterNot(RetiredVenueRows.venueOf(_) == Retired), true))
        slots.rowIdsChecked()      shouldBe ((slotIdsBefore.filterNot(RetiredVenueRows.venueOf(_) == Retired), true))
        screenings.findForFilmChecked("other|2025") shouldBe
          ((Map(CinemaShowing(KinoEtiuda, "other").displayName -> tomorrow), true))

        RetiredVenueRows.sweep(Some(screenings), Some(slots), RetiredVenueRows.rosterOf(Country.Poland)) shouldBe
          RetiredVenueRows.none
      } finally { screenings.close(); slots.close() }
    }
}
