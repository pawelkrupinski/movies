package services.movies

import models.Showtime
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The read of one film's rows at a few venues ([[SlotKeyed.atCinemasFilter]]), against a real store:
 * Mongo orders strings by their UTF-8 bytes, so the `_id` range per venue has to hold every title key
 * a venue's rows can carry — an astral one (an emoji, 4 UTF-8 bytes from F0) included — and asking
 * for no venues at all is an empty, complete answer, not a query the server refuses.
 */
class FindAtCinemasIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val listed = ListedShowtimes(Seq(Showtime(java.time.LocalDateTime.parse("2031-06-12T18:00"), None)), None)
  private val venue  = s"Kino Muza${models.CinemaShowing.Separator}"

  "findAtCinemasChecked" should "read every row at a venue, an astral title key's included, and answer no venues as nothing" in {
    tools.IsolatedMongoDatabase.withDatabase(mongoTarget, "find-at-cinemas") { db =>
      val screenings = new MongoScreeningsRepository(Some(db))
      Seq("anora", "🎬 kino", "￿z").foreach(title => screenings.upsertSlot("film", s"$venue$title", listed))
      screenings.upsertSlot("film", s"Rialto${models.CinemaShowing.Separator}anora", listed)

      val rows = screenings.findAtCinemasChecked("film", Set("Kino Muza")).required
      rows.keySet shouldBe Set(s"${venue}anora", s"$venue🎬 kino", s"$venue￿z")

      screenings.findAtCinemasChecked("film", Set.empty) shouldBe tools.ReadOutcome.Answered(Map.empty)
      new MongoSlotsRepository(Some(db)).findAtCinemasChecked("film", Set.empty) shouldBe tools.ReadOutcome.Answered(Map.empty)
    }
  }
}
