package models

import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The US roster after the move from 75 km metros to Poland's shape: a major city
 * (a place of 100,000+) lists only its own venues, and every venue outside one is
 * on a town page or a cluster of nearby small towns — see
 * `data/us/scripts/build_pages.py` and [[UsRoster]]. The state is only how a
 * visitor finds a page: `/california/` is nothing at all. These lock the cut, the
 * slugs (2,001 pages joining a global namespace that already holds a UK Birmingham
 * and a German Potsdam), where each retired metro's URL goes, the clock each page
 * keeps, and that re-cutting the roster moved no cinema off it.
 */
class UsRosterSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def page(slug: String): City = City.bySlug(slug).value
  private def venue(name: String): Cinema = UsRoster.byDisplayName.get(name).value
  private def pageOf(name: String): City = City.forCinema(venue(name)).value
  private def state(label: String): CityGroup = City.usStates.find(_.label == label).value

  "The US roster" should "be one city per town or cluster page, grouped by the 55 states and territories" in {
    City.usCities should have size 2001
    City.usStates should have size 55
  }

  it should "hold every venue exactly once — a re-cut, not a re-harvest" in {
    val venues = City.usCities.flatMap(_.cinemas)
    venues should have size 5009
    venues.distinct should have size 5009
    venues.toSet shouldBe UsRoster.byDisplayName.values.toSet
  }

  // Flicks' only feed for the Pickwick in Syracuse IN is Park Ridge IL's Veezi site (216/216
  // showtimes identical), and we have no other source for it — so it would show Chicago's
  // Pickwick programme in South Bend. Park Ridge itself stays.
  it should "leave out a venue whose only feed is another venue's programme" in {
    val names = City.usCities.flatMap(_.cinemaDisplayNames).toSet
    names should not contain "Pickwick Theatre Syracuse"
    names should contain ("Pickwick Theatre Park Ridge")
  }

  "A major city" should "list only the venues inside it" in {
    val la = page("los-angeles")
    la.labels.nominative shouldBe "Los Angeles"
    la.cinemaDisplayNames should contain ("Chinese Theatre Hollywood")       // Hollywood is Los Angeles
    la.cinemaDisplayNames should not contain "Academy Cinemas Pasadena"      // once on the 133-venue metro
    pageOf("Academy Cinemas Pasadena").slug shouldBe "pasadena-california"
    la.lat shouldBe 34.05 +- 0.01                                            // its own centre, not the metro's
    la.zoneId.getId shouldBe "America/Los_Angeles"
  }

  it should "take its boroughs and neighbourhoods, whatever Flicks calls their town" in {
    val nyc = page("new-york")
    nyc.cinemas should have size 67
    nyc.cinemaDisplayNames should contain allOf ("Alamo Drafthouse Brooklyn", "Alamo Drafthouse Staten Island",
                                                 "AMC Bay Terrace 6")      // Bayside, Queens
    nyc.coveredPlaces should contain allOf ("New York", "Brooklyn")
  }

  "A cluster page" should "be named after its biggest town, and speak of it as an area" in {
    val cherryHill = page("cherry-hill")
    cherryHill.labels.nominative shouldBe "Cherry Hill area"
    cherryHill.locativePhrase shouldBe "in the Cherry Hill area"
    cherryHill.coveredPlaces.take(3) shouldBe Seq("Cherry Hill area", "Cherry Hill", "Sewell")
  }

  it should "cross a state line where the towns do: one travel-shed, one page" in {
    // The Twin Cities' Wisconsin side was a metro of its own; Hudson's cinema is on
    // the Woodbury page with Minnesota's now, filed under Minnesota.
    val woodbury = pageOf("Hudson 12 Theatre")
    woodbury.slug shouldBe "woodbury"
    woodbury.cinemaDisplayNames should contain ("Woodbury 10")
    state("Minnesota").cities should contain (woodbury)
    state("Wisconsin").cities should not contain woodbury
  }

  "A one-town page" should "just name its town" in {
    page("bend").labels.nominative shouldBe "Bend"
    page("bend").locativePhrase shouldBe "in Bend"
  }

  "Two towns a mountain range apart" should "be two pages, as the distance rule has them" in {
    // Bishop and Mammoth Lakes, 59 km apart on US-395, were one "Eastern Sierra"
    // metro kept out of Fresno by hand. At 30 km neither reaches the other.
    pageOf("Bishop Twin Theatre").slug shouldBe "bishop"
    pageOf("Minaret Cinema is D'Place").slug shouldBe "mammoth-lakes"
    City.renamedSlugs.get("eastern-sierra").value shouldBe "bishop"
  }

  "A venue whose coordinates were wrong" should "be on the page of the town its address names" in {
    // 50220 is Perry, Iowa — a flipped longitude sign once had it in Mongolia.
    pageOf("Grand Theatre Perry").slug shouldBe "perry"
    // 56256 is Madison, MINNESOTA, harvested at Madison, Wisconsin's coordinates.
    pageOf("Grand Theater Madison").labels.nominative shouldBe "Canby area"
  }

  "A state" should "be a grouping over its pages, not a city" in {
    City.bySlug("texas") shouldBe None
    // A state's name is a page only where a TOWN has it — California, Maryland.
    City.bySlug("california").value.asInstanceOf[UsCity].stateName shouldBe "Maryland"
    val california = state("California")
    california.cities should contain allOf (page("los-angeles"), page("san-francisco"), page("bishop"))
    // Alphabetical — a heading you OPEN is a list you scan for a name you already know.
    val names = california.cities.map(_.labels.nominative)
    val collator = java.text.Collator.getInstance(java.util.Locale.forLanguageTag("en-US"))
    names shouldBe names.sortWith((a, b) => collator.compare(a, b) < 0)
  }

  it should "come in state order — the apps' picker and catalog seed read it top to bottom" in {
    val states = City.usStates.map(_.slug)
    states shouldBe states.sorted
    City.usStates.head.label shouldBe "Alabama"
  }

  it should "partition the country's cities, with none dropped or shared" in {
    City.usStates.flatMap(_.cities) should contain theSameElementsAs City.usCities
    City.usStates.flatMap(_.cities).distinct should have size City.usCities.size
  }

  "A page" should "keep its OWN clock, not its state's predominant one" in {
    page("knoxville").zoneId.getId shouldBe "America/New_York"    // Tennessee is Central
    page("el-paso").zoneId.getId shouldBe "America/Denver"        // Texas is Central
    page("pensacola").zoneId.getId shouldBe "America/Chicago"     // Florida is Eastern
    page("coeur-d-alene").zoneId.getId shouldBe "America/Los_Angeles" // Idaho is Mountain
    page("scottsbluff").zoneId.getId shouldBe "America/Denver"    // Nebraska is Central
    page("nashville").zoneId.getId shouldBe "America/Chicago"
  }

  it should "give a venue across a zone line from the rest of its page its own" in {
    // Campbellsville (Taylor County) keeps Eastern; the rest of its page, Russell
    // Springs among it, keeps Central.
    val green = venue("Green River Cinema Campbellsville")
    pageOf("Green River Cinema Campbellsville").zoneId.getId shouldBe "America/Chicago"
    UsRoster.venueZones.get(green).value.getId shouldBe "America/New_York"
    UsRoster.venueZones.get(venue("Key Twin Russell Springs")) shouldBe None
  }

  "US slugs" should "keep the bare name for the town a page had it for" in {
    // /springfield/ was Springfield, Illinois; Springfield, Missouri is bigger, and
    // is the one qualified, because the URL is published.
    pageOf("Moxie Cinema Springfield 2").slug shouldBe "springfield-missouri"
    page("springfield").asInstanceOf[UsCity].stateName shouldBe "Illinois"
  }

  it should "never take a name another country serves" in {
    City.bySlug("birmingham") shouldBe Some(Birmingham)
    City.bySlug("glasgow")    shouldBe Some(Glasgow)
    pageOf("Alabama Theatre Birmingham").slug shouldBe "birmingham-alabama"
    pageOf("Marquee Highland Glasgow 8").slug shouldBe "glasgow-kentucky"
    City.bySlug("potsdam").value.country shouldBe Country.Germany
    pageOf("Roxy Potsdam 5").slug shouldBe "potsdam-new-york"
    pageOf("Stuttgart Twin Cinema").slug shouldBe "stuttgart-arkansas"
  }

  it should "keep the two Spain yielded to before it had town pages" in {
    page("toledo").country shouldBe Country.UnitedStates
    page("laredo").country shouldBe Country.UnitedStates
    page("toledo-castilla-la-mancha").country shouldBe Country.Spain
  }

  "A metro that is no longer a page" should "redirect to the page holding the town it was named after" in {
    // The halves of a travel-shed the state line used to split go to the city itself.
    City.renamedSlugs.get("philadelphia-new-jersey").value shouldBe "philadelphia"
    City.renamedSlugs.get("cincinnati-kentucky").value shouldBe "cincinnati"
    City.renamedSlugs.get("new-york-new-jersey").value shouldBe "new-york"
    City.renamedSlugs.get("minneapolis-st-paul-wisconsin").value shouldBe "minneapolis"
    City.renamedSlugs.get("san-francisco-bay-area").value shouldBe "san-francisco"
    // Its town is on a cluster named after another: the cluster.
    City.renamedSlugs.get("portland-maine").value shouldBe "westbrook"
    City.renamedSlugs.get("madison-minnesota").value shouldBe "canby"
  }

  it should "fall back to the page holding most of its venues when no town has its name" in {
    City.renamedSlugs.get("inland-empire").value shouldBe "chino"
    City.renamedSlugs.get("hampton-roads").value shouldBe "norfolk-virginia"
    City.renamedSlugs.get("alaska").value shouldBe "anchorage"
  }

  it should "not redirect a slug that is its heir's own URL again" in {
    // The Alabama metro was /birmingham-alabama/, and so is the Birmingham page.
    City.renamedSlugs.get("birmingham-alabama") shouldBe None
    page("birmingham-alabama").labels.nominative shouldBe "Birmingham"
  }

  "Every US slug ever published" should "still answer, at a US page" in {
    val published = Seq(
      "aberdeen", "abilene", "adrian", "alamosa", "albany", "albany-georgia", "albuquerque", "alexandria",
      "altoona-johnstown", "amarillo", "american-samoa", "anchorage", "asheville", "ashland", "aspen",
      "atchison", "athens", "atlanta", "augusta", "augusta-georgia", "austin", "bakersfield", "baltimore",
      "bangor", "barstow", "baton-rouge", "beaumont-port-arthur", "beckley", "bellingham", "bemidji", "bend",
      "bethel", "billings", "binghamton", "birmingham-alabama", "bloomington", "bloomsburg",
      "bluefield-princeton", "boise", "boston", "bowling-green", "bozeman", "brainerd-lakes", "brookfield",
      "brunswick-golden-isles", "buffalo-niagara", "burlington", "burns", "butte", "cadet", "calumet",
      "canadian", "cape-cod", "cape-girardeau", "carnegie", "carroll", "casper", "cedar-city", "cedar-rapids",
      "central-coast", "central-wyoming", "champaign-urbana", "charleston", "charleston-south-carolina",
      "charlotte", "charlotte-south-carolina", "charlottesville", "chattanooga", "cheyenne-laramie", "chicago",
      "chico", "cincinnati", "cincinnati-kentucky", "clarendon", "clarksburg", "cleveland", "coeur-dalene",
      "college-station", "colorado-springs", "columbia", "columbia-missouri", "columbus", "columbus-georgia",
      "colville", "corning-elmira", "corpus-christi", "craig", "daingerfield", "dallas-fort-worth", "dayton",
      "daytona-beach", "decorah", "dekalb", "delaware", "denver", "des-moines", "destin-fort-walton-beach",
      "detroit", "devils-lake", "dickinson", "district-of-columbia", "dodge-city", "dothan", "dubuque",
      "duluth", "durango", "durant", "eads", "east-texas", "eastern-arizona", "eastern-kentucky",
      "eastern-oregon", "eastern-panhandle", "eastern-sierra", "eau-claire", "effingham", "el-centro",
      "el-paso", "elizabeth-city", "elkins", "elko", "ely", "ennis", "erie", "eureka-arcata", "evansville",
      "fairbanks", "fallon", "fargo", "fayetteville", "flagstaff", "flint", "florence", "florence-alabama",
      "florida-keys", "fort-collins", "fort-dodge", "fort-smith", "fort-wayne", "fox-valley", "fresno",
      "gadsden", "gallup", "galveston", "gaylord", "glasgow-montana", "graham", "grand-forks",
      "grand-forks-north-dakota", "grand-island", "grand-junction", "grand-rapids", "great-barrington",
      "great-falls", "green-bay", "greenville", "greenwood", "grove-city", "guam", "gulf-coast", "guymon",
      "hackettstown", "hampton-roads", "harrisburg", "hartford", "harvey", "hattiesburg", "havre",
      "hawaii-big-island", "helena", "higginsville", "highlands", "hilton-head", "holbrook", "holyoke",
      "hood-river", "houston", "hudson-valley", "huntsville", "hutchinson", "idaho-falls", "indian-lake",
      "indianapolis", "inland-empire", "iron-range", "ironwood", "jackson", "jackson-mississippi",
      "jacksonville", "jacksonville-north-carolina", "jamestown", "jamestown-new-york", "jonesboro", "joplin",
      "juneau", "kalamazoo", "kalispell", "kankakee", "kansas-city", "kansas-city-kansas", "kearney",
      "kenai-peninsula", "ketchum", "killeen-temple", "knoxville", "kodiak", "kokomo", "la-crosse", "lafayette",
      "lafayette-tennessee", "lafayette-west-lafayette", "lake-havasu-city", "lake-of-the-ozarks", "lancaster",
      "laredo", "las-cruces", "las-vegas", "lawton", "lebanon-hanover", "lehigh-valley", "lewisburg",
      "lewiston", "lewistown", "lexington", "lexington-virginia", "lima", "lincoln", "little-rock", "littleton",
      "logan", "long-island", "los-angeles", "louisville", "louisville-kentucky", "lubbock", "lufkin", "macon",
      "madison", "madison-minnesota", "manchester-new-hampshire", "manhattan", "mankato", "martin",
      "mason-city", "maui", "mayaguez", "mcalester", "mccook", "mckinney", "mcpherson", "medford", "memphis",
      "memphis-mississippi", "miami", "midland", "midland-odessa", "miles-city", "milwaukee",
      "minneapolis-st-paul", "minneapolis-st-paul-wisconsin", "minot", "missoula", "mitchell", "moab", "mobile",
      "mobridge", "modesto", "monroe", "monterey-county", "montevideo", "montgomery", "montrose", "mount-ayr",
      "murfreesboro", "myrtle-beach", "naples", "nashville", "new-haven", "new-orleans", "new-york",
      "new-york-new-jersey", "nome", "norfolk", "north-central-arkansas", "north-central-ohio", "north-coast",
      "north-georgia", "north-missouri", "north-platte", "northeast-wyoming", "northern-california",
      "northern-michigan", "northwest-arkansas", "northwest-kansas", "northwest-oklahoma",
      "northwest-wisconsin", "oahu", "ocala", "ocean-city", "oelwein", "oklahoma-city", "old-valdez", "olympia",
      "omaha", "omaha-iowa", "ontario", "orlando", "ottumwa", "owego", "owensboro", "paducah", "palm-springs",
      "paullina", "pensacola", "peoria", "philadelphia", "philadelphia-new-jersey", "philip", "phoenix",
      "pierre", "pittsburgh", "plains", "plattsburgh-north-country", "plentywood", "pocatello", "ponca-city",
      "port-huron", "portland", "portland-maine", "portland-washington", "portsmouth",
      "portsmouth-new-hampshire", "presque-isle", "providence", "pueblo", "quad-cities", "quincy", "rapid-city",
      "reno", "rhinelander", "rhode-island", "richfield", "richmond", "richmond-indiana", "rio-grande-valley",
      "roanoke", "rochester", "rochester-minnesota", "rockford", "rocky-mount-wilson", "rolla", "roswell",
      "sacramento", "saginaw-bay-city", "saint-george", "salina", "salmon", "salt-lake-city", "san-antonio",
      "san-diego", "san-francisco-bay-area", "san-juan", "san-luis-obispo", "sandusky", "sanpete-valley",
      "santa-barbara", "santa-fe", "santa-rosa", "sarasota-bradenton", "savannah", "sayville", "scottsbluff",
      "scranton-wilkes-barre", "searcy", "seattle", "sedona", "shelbina", "shenandoah-valley", "sheridan",
      "show-low", "shreveport", "sierra-foothills", "sioux-city", "sioux-falls", "somerset-kentucky",
      "south-bend-elkhart", "southeast-alaska", "southeast-kansas", "southern-illinois", "southwest-kansas",
      "southwest-michigan", "southwest-minnesota", "southwest-virginia", "southwest-washington",
      "southwest-wisconsin", "southwest-wyoming", "space-coast", "spencer", "spokane", "springfield",
      "springfield-missouri", "st-cloud", "st-louis", "st-louis-illinois", "stamford", "star-valley",
      "state-college", "stephenville", "sterling", "stockton", "stuart", "susanville", "syracuse",
      "tallahassee", "tampa-bay", "terre-haute", "toledo", "towanda", "treasure-coast", "tri-cities",
      "tri-cities-tennessee", "triad", "triangle", "truth-or-consequences", "tucson", "tucumcari", "tulsa",
      "tupelo", "tuscaloosa", "twin-falls", "uinta-basin", "ukiah", "upper-peninsula", "utica-rome",
      "vermillion", "vermont", "victoria", "vinita", "virgin-islands", "virginia-eastern-shore", "waco",
      "waimea", "warner-robins", "washington-dc", "washington-dc-maryland", "waterloo", "watertown",
      "watertown-north-country", "wausau-stevens-point", "wenatchee", "west-branch", "west-central-minnesota",
      "west-palm-beach", "west-texas", "western-maryland", "western-massachusetts", "western-nc-mountains",
      "western-pa", "wichita", "wichita-falls", "willamette-valley", "williston", "wilmington", "winnemucca",
      "wytheville", "yakima", "yellowstone-gateway", "yuma", "alaska", "hawaii", "san-francisco", "perry",
      "newport-news", "jefferson", "osage",
    )
    published should have size 475
    published.foreach { slug =>
      withClue(s"$slug: ") {
        val landing = City.renamedSlugs.getOrElse(slug, slug)
        City.bySlug(landing).value.country shouldBe Country.UnitedStates
      }
    }
    // The metros that are still a page under their own slug — among them three
    // retired since (`/perry/`, `/newport-news/`, `/san-francisco/`), whose towns
    // are pages again.
    published.count(City.bySlug(_).isDefined) shouldBe 329
  }

  "Every retired slug" should "be retired, and name a successor that exists" in {
    // A typo in `renamedSlugs` fails silently in the direction that costs
    // most: the redirect 404s and the rows projected under the old slug are
    // read by nobody, so the city serves a near-empty page for a whole scrape
    // cadence and nothing says why.
    City.renamedSlugs.foreach { case (former, successor) =>
      withClue(s"$former → $successor: ") {
        City.bySlug(former) shouldBe None
        City.bySlug(successor) should not be None
      }
    }
  }
}
