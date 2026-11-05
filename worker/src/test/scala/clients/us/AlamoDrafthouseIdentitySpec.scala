package clients.us

import clients.tools.FakeHttpFetch
import models.{CinemaMovie, UsRoster}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.us.AlamoDrafthouseClient
import services.identity.{CatalogueId, Listing}
import services.identity.adapters.{CatalogueLinkReader, TmdbIdentityLookups, VenueDetails}
import services.movies.TitleNormalizer

import java.time.LocalDate

/** What identity reads off Alamo Drafthouse's recorded show pages ([[AlamoDrafthouseClientSpec]] replays the
 *  scrape itself): the venue's stated facts through [[VenueDetails]], and its catalogue links through
 *  [[CatalogueLinkReader]]. */
class AlamoDrafthouseIdentitySpec extends AnyFlatSpec with Matchers with OptionValues {

  private val Today    = LocalDate.of(2026, 8, 30)
  private val Lakeline = UsRoster.byDisplayName("Alamo Drafthouse Lakeline")
  private val ShowPage = "https://drafthouse.com/show/"

  private def client = {
    val fixtures = new FakeHttpFetch("alamo-drafthouse")
    new AlamoDrafthouseClient(fixtures, fixtures, "lakeline", Lakeline, today = Today)
  }

  private def listing(title: String, slug: String): Listing = Listing.of(Lakeline,
    CinemaMovie(models.Movie(title = title), Lakeline, None, Some(ShowPage + slug), None, Nil, Nil, Nil),
    TitleNormalizer.forCountry(models.Country.UnitedStates))

  "a re-release's venue facts" should "state no year, so it can never deny the film it re-releases" in {
    new VenueDetails(Seq(client), TmdbIdentityLookups.NoGaps).detail(listing("Avengers Endgame: Encore", "avengers-endgame-encore"))
      .answered.flatten.value.year shouldBe None
  }

  "a show's detail" should "be the VENUE's stated facts, read by identity through its own detail page" in {
    val facts = new VenueDetails(Seq(client), TmdbIdentityLookups.NoGaps).detail(listing("Last Shot", "last-shot"))
    facts.answered.flatten.map(f => (f.year, f.runtime)) shouldBe Some((Some(2026), Some(105)))
  }

  "a show's IMDb id" should "be the venue's own catalogue link to the film" in {
    val reader = new CatalogueLinkReader(Seq(AlamoDrafthouseClient.CatalogueLinkPages -> new FakeHttpFetch("alamo-drafthouse")))
    reader.links(ShowPage + "spy-kids") shouldBe Seq(CatalogueId("imdb", "tt0227538"))
    reader.links(ShowPage + "last-shot") shouldBe Nil
  }
}
