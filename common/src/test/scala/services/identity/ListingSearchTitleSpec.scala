package services.identity

import models.{CinemaMovie, KinoApollo, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer

import java.time.LocalDateTime

/** A listing searches the title the venue's rules ask external lookups for (`TitleNormalizer.apiQuery`) —
 *  what the old pipeline searched — while what relates it to other listings stays the venue's own title. */
class ListingSearchTitleSpec extends AnyFlatSpec with Matchers {

  private def listing(title: String): Listing =
    Listing.of(KinoApollo, CinemaMovie(Movie(title), KinoApollo, None, None, None, Nil, Nil,
      Seq(Showtime(LocalDateTime.of(2099, 3, 1, 18, 0), None))), SingleCountryNormalizer.titleNormalizer)

  "A programme's listing" should "search its film's title without the programme and the accessibility tags" in {
    // Kino Pałacowe's "Kino bez barier: Lalka (AD + CC + PJM) + spotkanie z twórcami": the old pipeline asked
    // TMDB for "Lalka"; the resolver's own shapes never cut "+ spotkanie…" off "Lalka (AD + CC + PJM)".
    val l = listing("Kino bez barier: Lalka (AD + CC + PJM) + spotkanie z twórcami")
    l.searchTitle shouldBe Some("Lalka (AD + CC + PJM)")
    Evidence.of(l, None).measured.shapes should contain ("Lalka")
    // Relating it to other listings reads the venue's title alone: the card stays the programme's own.
    Evidence.of(l, None).published.searchTitles shouldBe empty
  }

  "A plain title" should "have no search title of its own" in {
    listing("Lalka").searchTitle shouldBe None
  }
}
