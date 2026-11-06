package services.identity

import models.{CinemaMovie, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.{HistoryEntry, InMemoryIdentityHistory, InMemoryTmdbDocuments}
import services.movies.ListingKey

import java.time.Instant

/** The mirror's world at a moment (fix 3, one replay world): its listings as the history says they stood then, and its
 *  clock AT that moment -- a replay of a past moment never dated by the day it runs. */
class MirrorWorldSpec extends AnyFlatSpec with Matchers {
  private val titles = services.movies.SingleCountryNormalizer.titleNormalizer
  private def billed(year: Int) = ModelRead.corpus(Seq(Multikino -> Seq(CinemaMovie(Movie("Lalka", releaseYear = Some(year)), Multikino, None,
    Some("Multikino/Lalka"), None, Nil, Nil, Seq(Showtime(java.time.LocalDateTime.of(2026, 10, 1, 18, 0), None))))), titles)

  "the mirror's world at a moment" should "read the listings as they stood then, by a clock at that moment" in {
    val history = new InMemoryIdentityHistory
    val was     = billed(2024).head
    val now     = billed(2025).head
    val moved   = Instant.parse("2026-11-03T12:00:00Z")
    // the listing re-published at `moved`: 2024 before it, 2025 since
    history.append(Seq(HistoryEntry(HistoryEntry.Listing, ListingKey.serialised(now.key), moved.toEpochMilli,
      Some(ListingBson.doc(was)), Some(ListingBson.doc(now)), "b1")))
    val before = MirrorWorld.of(history, Seq(now), Nil, new InMemoryTmdbDocuments, moved.minusSeconds(3600))
    before.clock.instant() shouldBe moved.minusSeconds(3600)
    before.listings.map(_.year) shouldBe Seq(Some(2024))
    val after = MirrorWorld.of(history, Seq(now), Nil, new InMemoryTmdbDocuments, moved.plusSeconds(3600))
    after.clock.instant() shouldBe moved.plusSeconds(3600)
    after.listings.map(_.year) shouldBe Seq(Some(2025))
  }
}
