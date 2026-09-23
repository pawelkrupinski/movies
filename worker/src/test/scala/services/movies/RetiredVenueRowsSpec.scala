package services.movies

import ch.qos.logback.classic.Level
import models.{Cinema, CinemaCityChain, CinemaShowing, Country, KinoEtiuda, KinoMiescisko, KinoOOK, KinoStarowka, KinoWawrzyn, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.LogCapture

import java.time.LocalDateTime

/**
 * [[RetiredVenueRows.sweep]] removes the `screenings` / `movie_slots` rows of a venue the
 * country's roster no longer lists — "Kino Etiuda OBK", dropped in 39e04cdd2, left 3 films
 * and 8 future showtimes in prod PL that nothing ever cleared — and ONLY those rows.
 *
 * The rule is pure over [[SlotKeyedRows]], so the in-memory stores here run the exact
 * code production runs; the Mongo half is `RetiredVenueRowsIntegrationSpec`'s.
 */
class RetiredVenueRowsSpec extends AnyFlatSpec with Matchers {

  private val tomorrow = Seq(Showtime(LocalDateTime.now.plusDays(1), bookingUrl = None))
  private val roster   = RetiredVenueRows.rosterOf(Country.Poland)

  /** The retired venue's name. A PREFIX-sharing sibling ("Kino Etiuda") is still on the
   *  roster, so a rule matching on `startsWith` — or reading the whole slot key as the
   *  venue — gets one of the two wrong. */
  private val Retired = "Kino Etiuda OBK"
  private val live    = Seq(KinoEtiuda, KinoOOK, KinoStarowka, KinoWawrzyn, KinoMiescisko)

  private def seed(screenings: ScreeningsRepository, slots: SlotsRepository, filmId: String, slotKey: String): Unit = {
    screenings.upsertSlot(filmId, slotKey, tomorrow)
    slots.upsertSlot(filmId, slotKey, SourceData(title = Some(filmId)))
  }

  /** Every live venue holds a slot on `live|2026`; the retired one holds per-title slots
   *  on two films plus one legacy bare-cinema key. */
  private def corpus(screenings: ScreeningsRepository = new InMemoryScreeningsRepository,
                     slots: SlotsRepository = new InMemorySlotsRepository) = {
    live.foreach(c => seed(screenings, slots, "live|2026", CinemaShowing(c, "live").displayName))
    seed(screenings, slots, "live|2026", s"$Retired${CinemaShowing.Separator}live")
    seed(screenings, slots, "other|2025", s"$Retired${CinemaShowing.Separator}other")
    seed(screenings, slots, "other|2025", CinemaShowing(KinoEtiuda, "other").displayName)
    slots.upsertSlot("legacy|2020", Retired, SourceData(title = Some("legacy")))
    // Not venues at all: the chain-detail slot and an enrichment slot must survive.
    slots.upsertSlot("live|2026", CinemaCityChain.displayName, SourceData(title = Some("live")))
    slots.upsertSlot("live|2026", Tmdb.displayName, SourceData(title = Some("live")))
    (screenings, slots)
  }

  private def venuesOf(store: SlotKeyedRows): Set[String] = store.rowIdsChecked()._1.map(RetiredVenueRows.venueOf)

  "sweep" should "remove every row of a venue the roster no longer lists, and nothing else" in {
    val (screenings, slots) = corpus()
    val liveScreenings = screenings.rowIdsChecked()._1.filterNot(RetiredVenueRows.venueOf(_) == Retired)
    val liveSlots      = slots.rowIdsChecked()._1.filterNot(RetiredVenueRows.venueOf(_) == Retired)

    RetiredVenueRows.sweep(Some(screenings), Some(slots), roster) shouldBe
      RetiredVenueRows(screenings = 2, slots = 3, venues = Map(Retired -> 5L))

    screenings.rowIdsChecked() shouldBe ((liveScreenings, true))
    slots.rowIdsChecked()      shouldBe ((liveSlots, true))
    venuesOf(slots) should contain allOf (KinoEtiuda.displayName, CinemaCityChain.displayName, Tmdb.displayName)
  }

  it should "be idempotent — a second sweep finds nothing" in {
    val (screenings, slots) = corpus()
    RetiredVenueRows.sweep(Some(screenings), Some(slots), roster).rows shouldBe 5
    RetiredVenueRows.sweep(Some(screenings), Some(slots), roster) shouldBe RetiredVenueRows.none
  }

  it should "remove nothing when the roster is EMPTY" in {
    val (screenings, slots) = corpus()
    RetiredVenueRows.sweep(Some(screenings), Some(slots), roster = Set.empty) shouldBe RetiredVenueRows.none
    venuesOf(screenings) should contain (Retired)
    venuesOf(slots)      should contain (Retired)
  }

  it should "remove nothing when the roster would retire too large a share of the stored venues" in {
    val (screenings, slots) = corpus()
    // A roster that has lost most of its venues — a half-loaded data file, a wrong country.
    val partial = roster -- live.drop(2).map(_.displayName)
    RetiredVenueRows.sweep(Some(screenings), Some(slots), partial) shouldBe RetiredVenueRows.none
    venuesOf(slots) should contain allOf (Retired, KinoWawrzyn.displayName)
  }

  it should "skip a side store whose id read failed, while still sweeping the other" in {
    val screeningsStore = new InMemoryScreeningsRepository
    val (_, slots) = corpus(screenings = screeningsStore)
    RetiredVenueRows.sweep(Some(new UnreadableScreeningsRepository(screeningsStore)), Some(slots), roster) shouldBe
      RetiredVenueRows(screenings = 0, slots = 3, venues = Map(Retired -> 3L))
    venuesOf(screeningsStore) should contain (Retired)
    venuesOf(slots) should not contain Retired
  }

  it should "put the removal on the removal-audit log with the venue" in {
    val (screenings, slots) = corpus()
    val events = LogCapture.thisThread(RemovalAudit.LoggerName, Some(Level.INFO))(
      RetiredVenueRows.sweep(Some(screenings), Some(slots), roster))
    val lines = events.map(_.getFormattedMessage).filter(_.contains("reason=retired-venue"))
    withClue(s"audit lines: $lines\n") {
      lines should have size 1
      lines.head should include (s"$Retired=5")
      lines.head should include ("2 screenings row(s) + 3 slot row(s)")
    }
  }

  "rosterOf" should "be the country's own venues plus the sources that are not venues" in {
    roster should contain allOf (KinoEtiuda.displayName, CinemaCityChain.displayName, Tmdb.displayName)
    roster should not contain Retired
    // Per-country: a UK venue is not on Poland's roster, nor a Polish one on the UK's.
    val ukVenue = Country.UnitedKingdom.cities.flatMap(_.cinemas).head
    roster should not contain ukVenue.displayName
    RetiredVenueRows.rosterOf(Country.UnitedKingdom) should not contain KinoEtiuda.displayName
  }

  it should "cover every known venue under SOME country, so no live venue is ever retired" in {
    val covered = Country.all.flatMap(RetiredVenueRows.rosterOf).toSet
    Cinema.all.map(_.displayName).filterNot(covered) shouldBe empty
  }
}
