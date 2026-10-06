package services.identity

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The one reading of a listing's shape every rule shares ([[ListingShape]]): the predicates the agreement, posters,
 *  catalogue take, broadcast join and fill each used to define for themselves. */
class ListingShapeSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  "a listing's facts" should "be its venue's own unless a feed states them or it links a listings site's catalogue entry" in {
    val own = listing(KinoMuza, "NT Live: All My Sons", year = Some(2026))
    ListingShape.venueStated(own) shouldBe true
    // Flicks' film page linked as its page: the entry's claim, read onto it or not
    ListingShape.venueStated(own.copy(page = Some("https://www.flicks.us/movie/national-theatre-live-all-my-sons/"))) shouldBe false
    ListingShape.venueStated(own.copy(page = Some("https://www.flicks.us/movie/national-theatre-live-all-my-sons/"), pageFacts = true)) shouldBe false
    // a showtimes feed's catalogue id
    ListingShape.venueStated(own.copy(catalogueIds = Seq(CatalogueId(CatalogueSources.Webedia.source, "227420")))) shouldBe false
    // the venue's own page
    ListingShape.venueStated(own.copy(page = Some("https://kinomuza.pl/film/all-my-sons"))) shouldBe true
  }

  "a relay" should "be told by its stage work, its house or season, or a concert film's venue" in {
    ListingShape.stagesAWork(listing(KinoMuza, "OPERA-COSI FAN TUTTE")) shouldBe true
    ListingShape.billsAHouse(listing(KinoMuza, "OPERA-MAKBET - retransmisja")) shouldBe true
    ListingShape.billsAHouse(listing(KinoMuza, "Klondike")) shouldBe false
    val concert = listing(KinoMuza, "Hauser symfonicznie z Royal Albert Hall")
    ListingShape.relays(concert, concert.rawTitle.toLowerCase(java.util.Locale.ROOT)) shouldBe true
    val film = listing(KinoMuza, "Klondike")
    ListingShape.relays(film, film.rawTitle.toLowerCase(java.util.Locale.ROOT)) shouldBe false
  }
}
