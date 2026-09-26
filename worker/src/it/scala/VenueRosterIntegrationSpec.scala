package services.movies

import models.{CinemaShowing, CineworldFeltham, Country, KinoEtiuda, Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/** The Mongo half of `VenueRosterSpec`: `screenings` / `movie_slots` stores scoped to
 *  Poland's roster, on a database of their own that is dropped afterwards, refuse to write
 *  a row under a UK venue through either write path (`replaceFilm`, `upsertSlot`), and a
 *  foreign row already on disk survives a `replaceFilm` that names it. */
class VenueRosterIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {


  private val tomorrow = Seq(Showtime(LocalDateTime.now.plusDays(1).withNano(0), bookingUrl = None))
  private val film     = "odyseja|2026"
  private val polish   = CinemaShowing(KinoEtiuda, "odyseja").displayName
  private val foreign  = CinemaShowing(CineworldFeltham, "odyseja").displayName
  private val other    = CinemaShowing(CineworldFeltham, "other").displayName

  "A roster-scoped Mongo side store" should "never write a row under a venue outside the country" in
    tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "venue-roster") { db =>
      val roster     = VenueRoster.of(Country.Poland)
      val screenings = new MongoScreeningsRepository(Some(db), roster = roster)
      val slots      = new MongoSlotsRepository(Some(db), roster = roster)
      try {
        screenings.replaceFilm(film, Map(polish -> ListedShowtimes(tomorrow, None), foreign -> ListedShowtimes(tomorrow, None)))
        screenings.upsertSlot(film, other, ListedShowtimes(tomorrow, None))
        slots.replaceFilm(film, Map(polish -> SourceData(title = Some("Odyseja")), foreign -> SourceData(title = Some("The Odyssey"))))
        slots.upsertSlot(film, other, SourceData(title = Some("Other")))

        screenings.findForFilmChecked(film) shouldBe ((Map(polish -> tomorrow), true))
        slots.findForFilmChecked(film)._1.keySet shouldBe Set(polish)
      } finally { screenings.close(); slots.close() }
    }

  it should "keep a foreign row already on disk as it is, rather than prune or rewrite it" in
    tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "venue-roster-kept") { db =>
      val stale      = SourceData(title = Some("The Odyssey"))
      val unscoped   = new MongoSlotsRepository(Some(db))
      val slots      = new MongoSlotsRepository(Some(db), roster = VenueRoster.of(Country.Poland))
      try {
        unscoped.replaceFilm(film, Map(polish -> SourceData(title = Some("Odyseja")), foreign -> stale))
        slots.replaceFilm(film, Map(polish -> SourceData(title = Some("Odyseja 2")), foreign -> SourceData(title = Some("rewritten"))))
        slots.findForFilmChecked(film) shouldBe ((Map(polish -> SourceData(title = Some("Odyseja 2")), foreign -> stale), true))
      } finally { unscoped.close(); slots.close() }
    }
}
