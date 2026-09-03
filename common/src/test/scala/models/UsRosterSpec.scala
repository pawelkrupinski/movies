package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The US roster's shape: the addressable place is the METRO, and the state is
 * only how a visitor finds one. `/los-angeles/` is a page; `/california/` is
 * nothing at all. These lock the cut (which states split, which stay whole),
 * the slugs (460 metros joining a global namespace that already holds a UK
 * Birmingham, and two states each holding a "Philadelphia"), the clock each
 * metro keeps, and the fact that re-keying the roster moved no cinema.
 */
class UsRosterSpec extends AnyFlatSpec with Matchers {

  private def city(slug: String): City =
    City.usCities.find(_.slug == slug).getOrElse(fail(s"no US city '$slug'"))

  private def group(label: String): CityGroup =
    City.usStates.find(_.label == label).getOrElse(fail(s"no state '$label'"))

  /** The metro city a named venue was filed under. */
  private def metroOf(displayName: String): City =
    City.usCities.find(_.cinemaDisplayNames.contains(displayName))
      .getOrElse(fail(s"no US city holds '$displayName'"))

  "The US roster" should "be one city per metro, plus the states with no metros to speak of" in {
    // 460 metros over the 48 states worth splitting + the seven that stay whole.
    City.usCities should have size 467
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

  it should "come in state order, each state's metros together and biggest first" in {
    // The order IS the mobile picker's list and the catalog seed's — the apps
    // read `/api/catalog` top to bottom. A generator regression scrambled it to
    // American Samoa, Alaska, Alabama, Arizona, Arkansas, Colorado… and every
    // suite stayed green, because nothing asserted it: the seed spec only
    // rewrites the file, and `usStates` groups CONSECUTIVE places, which a
    // reshuffle of whole state blocks leaves intact.
    val states = City.usStates.map(_.slug)
    states shouldBe states.sorted
    City.usCities.map(_.slug).take(2) shouldBe
      City.usStates.head.cities.map(_.slug).take(2)   // Alabama's, not another state's
    City.usStates.head.label shouldBe "Alabama"
  }

  it should "partition the country's cities, with none dropped or shared" in {
    City.usStates.flatMap(_.cities) should contain theSameElementsAs City.usCities
    City.usStates.flatMap(_.cities).distinct should have size City.usCities.size
  }

  "A state with too few venues to be worth splitting" should "be the city itself, at the slug it always had" in {
    Seq("district-of-columbia", "delaware", "rhode-island",
        "vermont", "guam", "american-samoa", "virgin-islands").foreach { slug =>
      withClue(s"$slug: ") {
        val flat = city(slug)
        flat.cinemas should not be empty
        // It is its own state's whole group — nothing was cut out of it.
        City.usStates.find(_.cities.contains(flat)).map(_.cities) shouldBe Some(Seq(flat))
      }
    }
    // Vermont is the widest that stays whole: 142 km between its two metros, a
    // long drive but one drive. Delaware is 108 km.
    city("vermont").cinemas should have size 23
  }

  "A state whose few venues are nowhere near each other" should "be split anyway" in {
    // The count gate alone made these one city each. Alaska's 18 venues reach
    // 1,900 km and most of them have no road between them at all; Hawaii's 21
    // lie across four islands. `UsRoster.MaxSpanToStayWholeKm` is the term that
    // sees that, and the metros to show instead were already in the roster.
    City.bySlug("alaska") shouldBe None
    City.bySlug("hawaii") shouldBe None
    city("anchorage").cinemas should have size 5
    city("juneau").cinemas should have size 2
    city("oahu").cinemas should have size 11
    city("maui").cinemas should have size 4
    // Each holds only its own island/road network — the whole point of the cut.
    city("anchorage").cinemaDisplayNames should not contain "Gold Town Theater"  // Juneau
    city("juneau").cinemaDisplayNames should contain("Gold Town Theater")
    City.usStates.find(_.label == "Alaska").map(_.cities.size) shouldBe Some(9)
    City.usStates.find(_.label == "Hawaii").map(_.cities.size) shouldBe Some(4)
    // Still every venue, none dropped by the split.
    City.usStates.find(_.label == "Alaska").toSeq.flatMap(_.cities.flatMap(_.cinemas)) should have size 18
  }

  "A city that was split out of a state" should "keep answering for the state's published URL and its projected rows" in {
    // `/alaska/` is in the sitemap and in visitors' city cookies, so it 301s to
    // the biggest metro cut out of it rather than 404ing.
    City.renamedSlugs.get("alaska") shouldBe Some("anchorage")
    City.renamedSlugs.get("hawaii") shouldBe Some("oahu")
    // And EVERY metro reads the rows still projected under the old slug, or the
    // split would blank the state until the next projection — 14 h in the US.
    City.usStates.find(_.label == "Alaska").toSeq.flatMap(_.cities).foreach { metro =>
      withClue(s"${metro.slug}: ")(City.formerSlugs(metro.slug) should contain("alaska"))
    }
    City.formerSlugs("oahu") should contain("hawaii")
    // Derived from the roster, never listed by hand: the successors ARE the
    // state's cities, so a re-harvest that moves a metro cannot leave this stale.
    City.slugSuccession("alaska") should contain theSameElementsAs
      City.usStates.find(_.label == "Alaska").toSeq.flatMap(_.cities.map(_.slug))
  }

  "Every retired slug" should "be retired, and name successors that exist" in {
    // A typo in `slugSuccession` fails silently in the direction that costs
    // most: the redirect 404s and the rows projected under the old slug are
    // read by nobody, so the city serves a near-empty page for a whole scrape
    // cadence and nothing says why.
    City.slugSuccession.foreach { case (former, successors) =>
      withClue(s"$former: ") {
        City.bySlug(former) shouldBe None
        successors should not be empty
        successors.foreach(slug => withClue(s"successor '$slug': ")(City.bySlug(slug) should not be None))
      }
    }
  }

  "A metro" should "keep its OWN clock, not its state's predominant one" in {
    // Fifteen states straddle a zone boundary, and every City cut out of one
    // used to inherit the state's zone. That is not cosmetic: `City.zoneId`
    // decides when the day rolls over, when a showtime counts as started, and
    // the UTC offset the schema.org ScreeningEvents carry.
    city("knoxville").zoneId.getId shouldBe "America/New_York"    // Tennessee is Central
    city("chattanooga").zoneId.getId shouldBe "America/New_York"
    city("el-paso").zoneId.getId shouldBe "America/Denver"        // Texas is Central
    city("pensacola").zoneId.getId shouldBe "America/Chicago"     // Florida is Eastern
    city("coeur-dalene").zoneId.getId shouldBe "America/Los_Angeles" // Idaho is Mountain
    city("scottsbluff").zoneId.getId shouldBe "America/Denver"    // Nebraska is Central
    // …while a metro in its state's majority zone is unmoved.
    city("nashville").zoneId.getId shouldBe "America/Chicago"
    city("los-angeles").zoneId.getId shouldBe "America/Los_Angeles"
  }

  "A venue whose coordinates were wrong" should "be filed where its address says it is" in {
    // Four records disagreed with their own postcode, and each one had made or
    // corrupted a city. The generator now refuses them
    // (`cluster_metros.check_coordinates`); these lock the outcome.
    // 50220 is Perry, Iowa — a flipped longitude sign had it in Mongolia, and
    // alone out there it became a one-venue metro of its own.
    metroOf("Grand Theatre Perry").labels.nominative shouldBe "Des Moines"
    // 97365 is Newport, OREGON, not Newport News, Virginia.
    metroOf("Newport Performing Arts").labels.nominative shouldBe "Willamette Valley"
    City.usCities.map(_.labels.nominative) should not contain "Newport News"
    // Largo (33771) is on Tampa Bay; filed as "Key Largo" it had dragged the
    // real Key Largo cinema 380 km with it, into Naples.
    metroOf("Regal Largo Mall").labels.nominative shouldBe "Tampa Bay"
    metroOf("Ocean Reef Theater").labels.nominative shouldBe "Miami"
    // 53566 is Monroe, WI; filed as "Tomah" it had put the real Tomah cinema in
    // Madison, 142 km away.
    metroOf("Sky Vu Drive In Monroe").labels.nominative shouldBe "Madison"
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
    // territories of one or two cinemas — and Alaska's and Hawaii's six, which
    // are single-cinema towns with no road or no bridge to anywhere else
    // (Bethel, Kodiak, Nome, Old Valdez, Fairbanks, and Waimea on Kauai).
    City.usCities.count(_.cinemas.sizeIs <= 2) should be <= 30
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
