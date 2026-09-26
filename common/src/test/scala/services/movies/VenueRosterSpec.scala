package services.movies

import models.{CinemaCityChain, CinemaShowing, CineworldFeltham, Country, KinoEtiuda, Showtime, SourceData, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/**
 * A side-collection store scoped to a country's [[VenueRoster]] never WRITES a row under a
 * venue outside it — prod PL held 60 `movie_slots` rows under 14 German/UK venues, written
 * by per-country pipelines run against the Polish database — and never DELETES one because
 * of it either (a rolling deploy's old pod must not prune a venue the new pod just added).
 *
 * Runs against the in-memory stores, which call the same [[VenueRoster.writable]] /
 * [[VenueRoster.admitsWrite]] the Mongo stores do; the Mongo half is
 * `VenueRosterIntegrationSpec`'s.
 */
class VenueRosterSpec extends AnyFlatSpec with Matchers {

  private val roster   = VenueRoster.of(Country.Poland)
  private val polish   = CinemaShowing(KinoEtiuda, "odyseja").displayName
  private val foreign  = CinemaShowing(CineworldFeltham, "odyseja").displayName
  private val tomorrow = Seq(Showtime(LocalDateTime.now.plusDays(1).withNano(0), bookingUrl = None))
  private val film     = "odyseja|2026"

  "VenueRoster.of" should "admit the country's own venues and the non-venue sources, and nothing foreign" in {
    roster.admits(polish) shouldBe true
    roster.admits(KinoEtiuda.displayName) shouldBe true   // a legacy bare-cinema key
    roster.admits(CinemaCityChain.displayName) shouldBe true
    roster.admits(Tmdb.displayName) shouldBe true
    roster.admits(foreign) shouldBe false
    roster.admits(CineworldFeltham.displayName) shouldBe false
    VenueRoster.of(Country.UnitedKingdom).admits(foreign) shouldBe true
  }

  "A roster-scoped slots store" should "not write a slot under a foreign venue" in {
    val slots = new InMemorySlotsRepository(roster = roster)
    slots.replaceFilm(film, Map(polish -> SourceData(title = Some("Odyseja")), foreign -> SourceData(title = Some("The Odyssey"))))
    slots.upsertSlot(film, CinemaShowing(CineworldFeltham, "other").displayName, SourceData(title = Some("Other")))
    slots.findForFilm(film).keySet shouldBe Set(polish)
  }

  "VenueRoster.writable" should "keep, untouched, a foreign row already stored rather than prune or rewrite it" in {
    val stale  = SourceData(title = Some("The Odyssey"))
    val stored = Map(polish -> SourceData(title = Some("Odyseja")), foreign -> stale)
    val fresh  = Map(polish -> SourceData(title = Some("Odyseja 2")), foreign -> SourceData(title = Some("rewritten")))
    roster.writable("movie_slots", film, stored, fresh) shouldBe Map(polish -> SourceData(title = Some("Odyseja 2")), foreign -> stale)
    // A refused row the payload no longer names is dropped from the payload — the store's
    // own stale-slot prune, not the guard, decides that.
    roster.writable("movie_slots", film, stored, Map(polish -> stale)) shouldBe Map(polish -> stale)
  }

  "A roster-scoped screenings store" should "not write showtimes under a foreign venue" in {
    val screenings = new InMemoryScreeningsRepository(roster = roster)
    screenings.replaceFilm(film, Map(polish -> ListedShowtimes(tomorrow, None), foreign -> ListedShowtimes(tomorrow, None)))
    screenings.upsertSlot(film, CinemaShowing(CineworldFeltham, "other").displayName, ListedShowtimes(tomorrow, None))
    screenings.findForFilm(film).keySet shouldBe Set(polish)
  }

  "An unrestricted store" should "write every venue, as before" in {
    val slots = new InMemorySlotsRepository()
    slots.replaceFilm(film, Map(polish -> SourceData(title = Some("Odyseja")), foreign -> SourceData(title = Some("The Odyssey"))))
    slots.findForFilm(film).keySet shouldBe Set(polish, foreign)
  }
}
