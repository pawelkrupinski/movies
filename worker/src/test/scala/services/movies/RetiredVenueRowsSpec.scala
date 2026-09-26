package services.movies

import ch.qos.logback.classic.Level
import models.{Cinema, CinemaCityChain, CinemaShowing, Country, KinoEtiuda, KinoMiescisko, KinoOOK, KinoStarowka, KinoWawrzyn, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.LogCapture

import java.time.{Instant, LocalDateTime}

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

  /** The stores' clock: every row is written at `T0`, and every sweep runs past the grace
   *  period unless a case says otherwise. */
  private val T0 = Instant.parse("2026-09-20T12:00:00Z")
  private var clockNow = T0
  private val clock: () => Instant = () => clockNow
  private val later = T0.plusSeconds(RetiredVenueRows.Grace.toSeconds + 3600)
  private def sweep(screenings: Option[SlotKeyedRows], slots: Option[SlotKeyedRows], roster: Set[String]) =
    RetiredVenueRows.sweep(screenings, slots, roster, now = later)
  private val roster   = VenueRoster.venuesOf(Country.Poland)

  /** The retired venue's name. A PREFIX-sharing sibling ("Kino Etiuda") is still on the
   *  roster, so a rule matching on `startsWith` — or reading the whole slot key as the
   *  venue — gets one of the two wrong. */
  private val Retired = "Kino Etiuda OBK"
  private val live    = Seq(KinoEtiuda, KinoOOK, KinoStarowka, KinoWawrzyn, KinoMiescisko)

  private def seed(screenings: ScreeningsRepository, slots: SlotsRepository, filmId: String, slotKey: String): Unit = {
    screenings.upsertSlot(filmId, slotKey, ListedShowtimes(tomorrow, None))
    slots.upsertSlot(filmId, slotKey, SourceData(title = Some(filmId)))
  }

  /** Every live venue holds a slot on `live|2026`; the retired one holds per-title slots
   *  on two films plus one legacy bare-cinema key. */
  private def corpus(screenings: ScreeningsRepository = new InMemoryScreeningsRepository(clock),
                     slots: SlotsRepository = new InMemorySlotsRepository(clock)) = {
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

    sweep(Some(screenings), Some(slots), roster) shouldBe
      RetiredVenueRows(screenings = 2, slots = 3, venues = Map(Retired -> 5L))

    screenings.rowIdsChecked() shouldBe ((liveScreenings, true))
    slots.rowIdsChecked()      shouldBe ((liveSlots, true))
    venuesOf(slots) should contain allOf (KinoEtiuda.displayName, CinemaCityChain.displayName, Tmdb.displayName)
  }

  it should "leave a retired venue's row alone while it is younger than the grace period" in {
    // A rolling deploy: the NEW pod knows a venue the old pod's roster lacks, and has just
    // written its rows. The old pod's sweep must not delete them.
    val (screenings, slots) = corpus()
    clockNow = later.minusSeconds(3600)   // written an hour before the sweep
    seed(screenings, slots, "fresh|2026", s"$Retired${CinemaShowing.Separator}fresh")
    try {
      sweep(Some(screenings), Some(slots), roster) shouldBe
        RetiredVenueRows(screenings = 2, slots = 3, venues = Map(Retired -> 5L))
      screenings.findForFilm("fresh|2026") should not be empty
      slots.findForFilm("fresh|2026")      should not be empty
    } finally clockNow = T0
  }

  it should "never split a screenings/slot twin pair: one twin written inside the grace spares both" in {
    // Prod US 2026-09-23: Pickwick Theatre Syracuse's movie_slots rows were older than the
    // grace but 8 of their screenings twins had been rewritten since, so the sweep deleted
    // the slots and left 8 twinless screenings rows (200 future showtimes) for the stranded
    // sweep to find a day later. A row is as fresh as its freshest twin.
    val (screenings, slots) = corpus()
    clockNow = later.minusSeconds(3600)   // only the screenings twin is rewritten recently
    screenings.upsertSlot("other|2025", s"$Retired${CinemaShowing.Separator}other", ListedShowtimes(tomorrow ++ tomorrow, None))
    try {
      sweep(Some(screenings), Some(slots), roster) shouldBe
        RetiredVenueRows(screenings = 1, slots = 2, venues = Map(Retired -> 3L))
      screenings.findForFilm("other|2025").keySet should contain (s"$Retired${CinemaShowing.Separator}other")
      slots.findForFilm("other|2025").keySet     should contain (s"$Retired${CinemaShowing.Separator}other")
    } finally clockNow = T0
  }

  it should "be idempotent — a second sweep finds nothing" in {
    val (screenings, slots) = corpus()
    sweep(Some(screenings), Some(slots), roster).rows shouldBe 5
    sweep(Some(screenings), Some(slots), roster) shouldBe RetiredVenueRows.none
  }

  it should "remove nothing when the roster is EMPTY" in {
    val (screenings, slots) = corpus()
    sweep(Some(screenings), Some(slots), roster = Set.empty) shouldBe RetiredVenueRows.none
    venuesOf(screenings) should contain (Retired)
    venuesOf(slots)      should contain (Retired)
  }

  it should "remove nothing when the roster would retire too large a share of the stored venues" in {
    val (screenings, slots) = corpus()
    // A roster that has lost most of its venues — a half-loaded data file, a wrong country.
    val partial = roster -- live.drop(2).map(_.displayName)
    sweep(Some(screenings), Some(slots), partial) shouldBe RetiredVenueRows.none
    venuesOf(slots) should contain allOf (Retired, KinoWawrzyn.displayName)
  }

  it should "remove nothing from EITHER store when one store's id read failed" in {
    // Sweeping the readable store alone would judge the grace without the other twin's stamp
    // and could delete one half of a pair — the split the freshest-twin rule exists to stop.
    val screeningsStore = new InMemoryScreeningsRepository(clock)
    val (_, slots) = corpus(screenings = screeningsStore)
    sweep(Some(new UnreadableScreeningsRepository(screeningsStore)), Some(slots), roster) shouldBe RetiredVenueRows.none
    venuesOf(screeningsStore) should contain (Retired)
    venuesOf(slots)           should contain (Retired)
  }

  it should "put the removal on the removal-audit log with the venue" in {
    val (screenings, slots) = corpus()
    val events = LogCapture.thisThread(RemovalAudit.LoggerName, Some(Level.INFO))(
      sweep(Some(screenings), Some(slots), roster))
    val lines = events.map(_.getFormattedMessage).filter(_.contains("reason=retired-venue"))
    withClue(s"audit lines: $lines\n") {
      lines should have size 1
      lines.head should include (s"$Retired=5")
      lines.head should include ("2 screenings row(s) + 3 slot row(s)")
    }
  }

  "VenueRoster.venuesOf" should "be the country's own venues plus the sources that are not venues" in {
    roster should contain allOf (KinoEtiuda.displayName, CinemaCityChain.displayName, Tmdb.displayName)
    roster should not contain Retired
    // Per-country: a UK venue is not on Poland's roster, nor a Polish one on the UK's.
    val ukVenue = Country.UnitedKingdom.cities.flatMap(_.cinemas).head
    roster should not contain ukVenue.displayName
    VenueRoster.venuesOf(Country.UnitedKingdom) should not contain KinoEtiuda.displayName
  }

  it should "cover every known venue under SOME country, so no live venue is ever retired" in {
    val covered = Country.all.flatMap(VenueRoster.venuesOf).toSet
    Cinema.all.map(_.displayName).filterNot(covered) shouldBe empty
  }
}
