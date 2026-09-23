package services.cinemas.roster

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.roster.RosterAuditFixtures._
import tools.FetchedPage

class VenueSourcePageSpec extends AnyFlatSpec with Matchers {
  import VenueSourcePage._

  "a bilety24 organiser page" should "publish the organiser's street and town" in {
    Bilety24Organiser.read(page(Sroda477)) shouldBe Some(PublishedVenue("Środa Wielkopolska", Some("ul. Dąbrowskiego 19")))
    Bilety24Organiser.read(page(Ostrowiec1389)) shouldBe Some(PublishedVenue("Ostrowiec Świętokrzyski", Some("Siennieńska 54")))
    Bilety24Organiser.read(page(Konin1626)) shouldBe Some(PublishedVenue("Konin", Some("Plac Niepodległości 1")))
  }

  // GOK Kino Mewa in Budzyń types its postcode "64 840".
  it should "read a postcode typed with a space" in {
    val header = """<section class="cinema-view"><div class="cinema-view-info"><div class="header"><h1>GOK Kino Mewa - Budzyń</h1><p>Łokietka 31, 64 840 Budzyń</p></div></div></section>"""
    Bilety24Organiser.read(header) shouldBe Some(PublishedVenue("Budzyń", Some("Łokietka 31")))
  }

  it should "name no town on a page that is not an organiser's" in {
    Bilety24Organiser.read("<html><body><h1>Bilety24</h1></body></html>") shouldBe None
  }

  it should "tell an organiser page from the home page an unknown id redirects to" in {
    Bilety24Organiser.dropped(FetchedPage("https://www.bilety24.pl/kino/organizator/kino-baszta-477", "")) shouldBe false
    Bilety24Organiser.dropped(FetchedPage("https://www.bilety24.pl/", "")) shouldBe true
  }

  "a Filmweb cinema" should "be read off the cinema-info API, not the page that redirects over http" in {
    Filmweb.pageUrl("https://www.filmweb.pl/cinema/-1526") shouldBe filmwebInfo(1526)
  }

  it should "publish the cinema's town and street" in {
    Filmweb.read(page(Kolo1526)) shouldBe Some(PublishedVenue("Koło", Some("Słowackiego 5")))
    Filmweb.read(page(EtiudaObk3024)) shouldBe Some(PublishedVenue("Ostrowiec Świętokrzyski", Some("Siennieńska 54")))
    Filmweb.read(page(Braniewo2352)) shouldBe Some(PublishedVenue("Braniewo", Some("Katedralna 9")))
  }

  it should "count the empty 204 Filmweb answers an unknown id with as dropped" in {
    Filmweb.dropped(FetchedPage(filmwebInfo(999999), "")) shouldBe true
    Filmweb.dropped(FetchedPage(filmwebInfo(1526), page(Kolo1526))) shouldBe false
  }

  "VenueSourcePage.forUrl" should "pick the reader by the source URL" in {
    forUrl("https://www.bilety24.pl/kino/organizator/kino-baszta-477") shouldBe Some(Bilety24Organiser)
    forUrl("https://www.filmweb.pl/cinema/-1526") shouldBe Some(Filmweb)
    forUrl("https://kinoluna.bilety24.pl") shouldBe None
  }
}
