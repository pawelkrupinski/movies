package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The US roster's shape: the addressable place is the METRO, and the state is
 * only how a visitor finds one. `/los-angeles/` is a page; `/california/` is
 * nothing at all. These lock the cut (which states split, which stay whole),
 * the slugs (448 metros joining a global namespace that already holds a UK
 * Birmingham, and two states each holding a "Philadelphia"), and the fact
 * that re-keying the roster moved no cinema.
 */
class UsRosterSpec extends AnyFlatSpec with Matchers {

  private def city(slug: String): City =
    City.usCities.find(_.slug == slug).getOrElse(fail(s"no US city '$slug'"))

  private def group(label: String): CityGroup =
    City.usStates.find(_.label == label).getOrElse(fail(s"no state '$label'"))

  "The US roster" should "be one city per metro, plus the states with no metros to speak of" in {
    // 448 metros over the 46 states worth splitting + the nine that stay whole.
    City.usCities should have size 457
    City.usStates should have size 55
  }

  it should "hold every venue exactly once — a re-key, not a re-harvest" in {
    val venues = City.usCities.flatMap(_.cinemas)
    venues should have size 5031
    venues.distinct should have size 5031
  }

  "A metro" should "be the city, named and placed as itself" in {
    val la = city("los-angeles")
    la.labels.nominative shouldBe "Los Angeles"
    la.cinemas should have size 133
    // Its own centroid, not California's (35.2, -119.3) — the landing's
    // nearest-place geolocation is only as good as this.
    la.lat shouldBe 34.06 +- 0.4
    la.lon shouldBe (-118.28) +- 0.4
    la.zoneId.getId shouldBe "America/Los_Angeles"
  }

  it should "carry only its own venues, never the rest of the state's" in {
    val la = city("los-angeles").cinemaDisplayNames
    la should contain("Chinese Theatre Hollywood")
    la should not contain "Castro Theatre"          // San Francisco
    city("san-francisco-bay-area").cinemaDisplayNames should contain("Castro Theatre")
  }

  "A state" should "be a grouping over its metros, not a city" in {
    City.bySlug("california") shouldBe None
    City.bySlug("texas") shouldBe None
    val california = group("California")
    california.cities.map(_.labels.nominative).take(2) shouldBe Seq("Los Angeles", "San Francisco Bay Area")
    california.cities should have size 21
    california.cities.flatMap(_.cinemas) should have size 486
    // Biggest metro first — the one most of the state's visitors want.
    val sizes = california.cities.map(_.cinemas.size)
    sizes shouldBe sizes.sorted.reverse
  }

  "A metro named for a city it is really the region around" should "be called the region, and answer at the region's own URL" in {
    // The Bay is the one such metro (`UsRoster.MetroDisplayNames`): its regions
    // are the East Bay, the South Bay, the North Bay, the Peninsula and a San
    // Francisco that is the city proper, so a metro also called "San Francisco"
    // both shadowed one of its own regions and filed San Jose and Oakland under
    // a city they are not in. The URL follows the name: the metro answers at
    // `/san-francisco-bay-area/`, and its share card is named to match.
    val bay = city("san-francisco-bay-area")
    bay.labels.nominative shouldBe "San Francisco Bay Area"
    bay.slug shouldBe "san-francisco-bay-area"
    bay.areas.map(_.area.label) should contain allOf ("East Bay", "South Bay", "San Francisco")
  }

  it should "partition the country's cities, with none dropped or shared" in {
    City.usStates.flatMap(_.cities) should contain theSameElementsAs City.usCities
    City.usStates.flatMap(_.cities).distinct should have size City.usCities.size
  }

  "A state with too few venues to be worth splitting" should "be the city itself, at the slug it always had" in {
    Seq("alaska", "hawaii", "district-of-columbia", "delaware", "rhode-island",
        "vermont", "guam", "american-samoa", "virgin-islands").foreach { slug =>
      withClue(s"$slug: ") {
        val flat = city(slug)
        flat.cinemas should not be empty
        // It is its own state's whole group — nothing was cut out of it.
        City.usStates.find(_.cities.contains(flat)).map(_.cities) shouldBe Some(Seq(flat))
      }
    }
    city("alaska").cinemas should have size 18
  }

  "Metro slugs" should "give the bare one to the biggest claimant and qualify the rest" in {
    // Clusters never cross a state line, so one travel-shed arrives as one metro
    // per state it reaches. The side a visitor typing the name means — the
    // bigger one — keeps the bare slug.
    City.bySlug("new-york").map(_.cinemas.size) shouldBe Some(102)
    City.bySlug("new-york-new-jersey").map(_.labels.nominative) shouldBe Some("New York")
    City.bySlug("philadelphia").map(_.cinemas.size) shouldBe Some(35)
    City.bySlug("philadelphia-new-jersey").map(_.labels.nominative) shouldBe Some("Philadelphia")
  }

  it should "never take a name another country already serves" in {
    // `/birmingham/` and `/glasgow/` are live UK pages; a US metro may not move
    // either, however big it is.
    City.bySlug("birmingham") shouldBe Some(Birmingham)
    City.bySlug("glasgow")    shouldBe Some(Glasgow)
    City.bySlug("birmingham-alabama").map(_.labels.nominative) shouldBe Some("Birmingham")
    City.bySlug("glasgow-montana").map(_.labels.nominative)    shouldBe Some("Glasgow")
  }

  it should "leave a name nobody else wants exactly as it reads" in {
    Seq("los-angeles", "san-francisco-bay-area", "chicago", "seattle", "denver", "new-york")
      .foreach(s => withClue(s"$s: ")(City.bySlug(s) should not be None))
  }

  /** The metro city a named venue was filed under. */
  private def metroOf(displayName: String): City =
    City.usCities.find(_.cinemaDisplayNames.contains(displayName))
      .getOrElse(fail(s"no US city holds '$displayName'"))

  "Flicks metros inside one travel-shed" should "cluster into a single city" in {
    // `dallas` and `fort-worth` are separate Flicks metros and one metroplex.
    val metroplex = metroOf("AMC North Park 15 Dallas")
    metroplex.labels.nominative shouldBe "Dallas Fort Worth"
    metroplex.cinemaDisplayNames should contain("AMC Hulen 10")          // Fort Worth
    // Greater Cleveland clusters on Akron — whose own cinemas outnumber
    // Cleveland's, those being spread over a dozen separately-named suburbs —
    // but keeps the name a visitor is looking for.
    val cleveland = metroOf("Capitol Theatre Cleveland")
    cleveland.labels.nominative shouldBe "Cleveland"
    cleveland.cinemaDisplayNames should contain("Highland Theater Akron")
  }

  "A venue Flicks files under no metro" should "join the metro nearest it" in {
    // No `region_slug`, so the raw grouping dumped it in the catch-all.
    metroOf("Alabama Theatre Birmingham").labels.nominative shouldBe "Birmingham"
    City.usCities.map(_.labels.nominative) should not contain CinemaArea.Other.label
  }

  it should "be big enough to be worth a page of its own" in {
    // Only a venue with no neighbouring metro within 150 km keeps a place of its
    // own; everything else is folded into the metro nearest it. The handful that
    // do, plus Guam, American Samoa and the Virgin Islands, which are whole
    // territories of one or two cinemas.
    City.usCities.count(_.cinemas.sizeIs <= 2) should be <= 25
  }

  "A big metro" should "be split into the areas a local names" in {
    city("los-angeles").areas.map(_.area.label).take(3) shouldBe
      Seq("Los Angeles", "San Fernando Valley", "San Gabriel Valley")
    city("new-york").areas.map(_.area.label) should contain allOf ("Manhattan", "Brooklyn")
    // Chicago is not region-folded, so its areas stay the towns they cluster on.
    city("chicago").areas.map(_.area.label).head shouldBe "Chicago"
  }

  "Every other metro" should "stay one flat list" in {
    City.usCities.filter(_.isSplit).map(_.slug) should contain theSameElementsAs
      Seq("los-angeles", "new-york", "san-francisco-bay-area", "dallas-fort-worth", "chicago")
  }
}
