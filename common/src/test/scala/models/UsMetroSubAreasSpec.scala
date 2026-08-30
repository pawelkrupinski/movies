package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The five US metros too big to browse as one list are sub-divided into the
 * districts a local names — clustered from the venues' own coordinates and
 * named after the town each cluster centres on, the way the metros themselves
 * are. A metro is a `City` now, so its districts are its `areas`, the level
 * London's compass areas sit at. These lock which metros take part, the
 * partition, and the full name list for each: the names are the whole point, so
 * they are pinned outright.
 */
class UsMetroSubAreasSpec extends AnyFlatSpec with Matchers {

  private def metro(metroSlug: String): City =
    City.usCities.find(_.slug == metroSlug).getOrElse(fail(s"no US city '$metroSlug'"))

  private def sub(metroSlug: String): Seq[CinemaAreaGroup] = metro(metroSlug).areas

  /** District label → venue count, in the order the groups are offered in. */
  private def districts(metroSlug: String): Seq[(String, Int)] =
    sub(metroSlug).map(g => g.area.label -> g.cinemas.size)

  private def district(metroSlug: String, label: String): CinemaAreaGroup =
    sub(metroSlug).find(_.area.label == label)
      .getOrElse(fail(s"'$metroSlug' has no '$label' district"))

  private val subDivided =
    Seq("los-angeles", "new-york", "san-francisco", "dallas-fort-worth", "chicago")

  "Sub-division" should "reach exactly the metros at or past 75 venues" in {
    City.usCities.filter(_.isSplit).map(_.slug) should contain theSameElementsAs subDivided
    // `cluster_metros.MIN_VENUES_TO_SUBDIVIDE`, which the generator applies.
    subDivided.foreach(m => withClue(s"$m: ")(metro(m).cinemas.size should be >= 75))
  }

  it should "leave the next metros down as one list" in {
    // Seattle (70) and Boston (62) are the two nearest misses.
    metro("seattle").cinemas.size shouldBe 70
    sub("seattle") shouldBe empty
    metro("boston").cinemas.size shouldBe 62
    sub("boston") shouldBe empty
  }

  it should "leave a state small enough to be one city undivided too" in {
    metro("rhode-island").areas shouldBe empty
  }

  "Each metro's districts" should "partition it with none dropped or duplicated" in {
    subDivided.foreach { metroSlug =>
      withClue(s"$metroSlug: ") {
        val whole   = metro(metroSlug).cinemas
        val grouped = sub(metroSlug).flatMap(_.cinemas)
        grouped.distinct should have size grouped.size.toLong.toInt
        grouped.toSet shouldBe whole.toSet
        grouped should have size whole.size.toLong.toInt
      }
    }
  }

  it should "come biggest first, each labelled and slugged uniquely in the metro" in {
    subDivided.foreach { metroSlug =>
      withClue(s"$metroSlug: ") {
        val groups = sub(metroSlug)
        groups.size should be > 1
        groups.map(_.cinemas.size) shouldBe groups.map(_.cinemas.size).sorted.reverse
        groups.foreach(_.cinemas should not be empty)
        groups.map(_.area.label).distinct should have size groups.size.toLong.toInt
        val slugs = groups.map(_.area.slug)
        slugs.foreach(_ should fullyMatch regex "[a-z0-9]+(-[a-z0-9]+)*")
        slugs.distinct should have size groups.size.toLong.toInt
      }
    }
  }

  it should "keep each district in the metro's own cinema order" in {
    subDivided.foreach { metroSlug =>
      val whole = metro(metroSlug).cinemas
      sub(metroSlug).foreach { g =>
        withClue(s"$metroSlug/${g.area.slug}: ")(g.cinemas shouldBe whole.filter(g.cinemaSet))
      }
    }
  }

  it should "be named after places, never after a compass point" in {
    // What this split replaced: compass areas describe London, not a US metro.
    val labels = subDivided.flatMap(m => sub(m).map(_.area)).toSet
    labels intersect CinemaArea.compass.toSet shouldBe empty
  }

  // ── The names. Pinned outright: a shifted radius, fold or rename shows up
  //    here as the list a reader can judge, not as a count. ──────────────────

  "New York" should "fall out as the boroughs, then the Long Island and Westchester towns" in {
    districts("new-york") shouldBe Seq(
      "Manhattan" -> 40, "Brooklyn" -> 12, "Forest Hills" -> 7, "Yonkers" -> 5,
      "Bellmore" -> 4, "Staten Island" -> 4, "Farmingdale" -> 3, "Nanuet" -> 3,
      "New Rochelle" -> 3, "Stony Brook" -> 3, "Westbury" -> 3, "White Plains" -> 3,
      "Bayside" -> 2, "Huntington" -> 2, "Lynbrook" -> 2, "Manhasset" -> 2,
      "Northport" -> 2, "The Bronx" -> 2,
    )
    district("new-york", "Manhattan").cinemaDisplayNames should
      contain allOf ("Film Forum New York", "AMC Empire 25", "AMC Lincoln Square 13")
    district("new-york", "Brooklyn").cinemaDisplayNames should
      contain allOf ("BAM Rose Cinemas", "Nitehawk Cinema Williamsburg")
    district("new-york", "The Bronx").cinemaDisplayNames shouldBe
      Seq("AMC Bay Plaza 13 Bronx", "Regal Concourse")
    district("new-york", "Staten Island").cinemaDisplayNames should
      contain("Alamo Drafthouse Staten Island")
    // Queens arrives as its neighbourhoods, which is how its venues are filed.
    district("new-york", "Forest Hills").cinemaDisplayNames should
      contain allOf ("Cinemart Cinemas Forest Hills", "Kew Gardens Cinemas")
  }

  "Los Angeles" should "read as the towns of greater LA" in {
    districts("los-angeles") shouldBe Seq(
      "Los Angeles" -> 32, "Downey" -> 8, "Burbank" -> 6, "Glendale" -> 6, "Pasadena" -> 6,
      "Long Beach" -> 5, "Montebello" -> 5, "Torrance" -> 5, "Buena Park" -> 4,
      "Calabasas" -> 4, "Garden Grove" -> 4, "North Hollywood" -> 4, "Santa Monica" -> 4,
      "Thousand Oaks" -> 4, "Cerritos" -> 3, "Covina" -> 3, "El Segundo" -> 3,
      "Inglewood" -> 3, "La Habra" -> 3, "Lancaster" -> 3, "Marina del Rey" -> 3,
      "Northridge" -> 3, "Santa Clarita" -> 3, "Arcadia" -> 2, "Camarillo" -> 2,
      "Encino" -> 2, "Simi Valley" -> 2, "Avalon" -> 1,
    )
    district("los-angeles", "Santa Monica").cinemaDisplayNames should
      contain allOf ("AMC Santa Monica 7", "Aero Theatre Santa Monica")
    district("los-angeles", "Pasadena").cinemaDisplayNames should contain("IPIC Pasadena")
    district("los-angeles", "Burbank").cinemaDisplayNames should contain("AMC Burbank 16")
    district("los-angeles", "Long Beach").cinemaDisplayNames should
      contain("Art Theatre of Long Beach")
    // Hollywood, Downtown and the Westside are all filed under the city itself.
    district("los-angeles", "Los Angeles").cinemaDisplayNames should
      contain allOf ("Chinese Theatre Hollywood", "Alamo Drafthouse Downtown LA")
    // Catalina Island keeps its own: 35 km of sea from the nearest district.
    district("los-angeles", "Avalon").cinemaDisplayNames shouldBe
      Seq("Avalon Theatre (Catalina Casino)")
  }

  "San Francisco" should "read as the city, the East Bay, the Peninsula and the South Bay" in {
    districts("san-francisco") shouldBe Seq(
      "San Francisco" -> 16, "San Jose" -> 10, "Oakland" -> 9, "Larkspur" -> 6,
      "Concord" -> 5, "Hayward" -> 5, "Brentwood" -> 3, "Fremont" -> 3, "Moraga" -> 3,
      "Palo Alto" -> 3, "San Mateo" -> 3, "Livermore" -> 2, "Mountain View" -> 2,
      "Novato" -> 2, "Richmond" -> 2, "San Ramon" -> 2, "Santa Clara" -> 2, "Vallejo" -> 1,
    )
    district("san-francisco", "San Francisco").cinemaDisplayNames should
      contain allOf ("Castro Theatre", "Roxie Theatre", "AMC Metreon 16 San Francisco")
    district("san-francisco", "Oakland").cinemaDisplayNames should
      contain allOf ("Grand Lake Theatre Oakland", "Berkeley Art Museum & Pacific Film Archive")
    district("san-francisco", "San Jose").cinemaDisplayNames should
      contain("Cinemark San Jose Oakridge 20")
    // Flicks files the Stanford and the Aquarius under East Palo Alto; both are
    // in Palo Alto, which is also the name a local uses.
    district("san-francisco", "Palo Alto").cinemaDisplayNames should
      contain allOf ("Stanford Palo Alto", "Landmark Aquarius")
  }

  "Dallas Fort Worth" should "read as Dallas, Fort Worth and the mid-cities" in {
    districts("dallas-fort-worth") shouldBe Seq(
      "Fort Worth" -> 16, "Dallas" -> 14, "Arlington" -> 4, "Frisco" -> 4, "Grapevine" -> 4,
      "Plano" -> 4, "Bedford" -> 3, "Denton" -> 3, "Grand Prairie" -> 3, "Keller" -> 3,
      "Red Oak" -> 3, "Burleson" -> 2, "Garland" -> 2, "Granbury" -> 2, "Irving" -> 2,
      "Lewisville" -> 2, "Mansfield" -> 2, "The Colony" -> 2, "Cleburne" -> 1,
      "Decatur" -> 1, "Weatherford" -> 1,
    )
    district("dallas-fort-worth", "Dallas").cinemaDisplayNames should
      contain allOf ("AMC North Park 15 Dallas", "Majestic Theatre Dallas")
    district("dallas-fort-worth", "Fort Worth").cinemaDisplayNames should
      contain allOf ("AMC Hulen 10", "Movie Tavern West 7th Street")
    // Flicks splits Arlington between "Arlington" and "Arlington Heights"; the
    // rename puts one town's four cinemas back in one district.
    district("dallas-fort-worth", "Arlington").cinemaDisplayNames should
      contain allOf ("LOOK Cinemas Arlington", "AMC The Parks at Arlington 18")
  }

  "Chicago" should "read as the city and its named suburbs" in {
    districts("chicago") shouldBe Seq(
      "Chicago" -> 22, "Country Club Hills" -> 4, "Elk Grove" -> 4, "Frankfort" -> 4,
      "Niles" -> 4, "Oak Brook" -> 4, "Evanston" -> 3, "Glen Ellyn" -> 3, "Naperville" -> 3,
      "Norridge" -> 3, "North Riverside" -> 3, "Northbrook" -> 3, "Wheeling" -> 3,
      "Woodridge" -> 3, "Bolingbrook" -> 2, "Gurnee" -> 2, "New Lenox" -> 2, "Skokie" -> 2,
      "South Barrington" -> 2, "Vernon Hills" -> 2,
    )
    // The Loop and the North Side are all filed under Chicago itself.
    district("chicago", "Chicago").cinemaDisplayNames should
      contain allOf ("AMC River East Chicago 21", "Music Box Theatre Chicago",
                     "Gene Siskel Film Center Chicago")
    district("chicago", "Evanston").cinemaDisplayNames should contain("AMC Evanston 12")
    district("chicago", "Naperville").cinemaDisplayNames should contain("AMC Naperville 16")
  }

  // ── London is not part of this ──────────────────────────────────────────────

  "London" should "keep its hand-written compass map exactly" in {
    London.areas.map(g => g.area.label -> g.cinemas.size) shouldBe
      Seq("Central" -> 29, "North" -> 24, "East" -> 16, "South" -> 45, "West" -> 19)
    London.areas.map(_.area) shouldBe CinemaArea.compass
    London.areas.map(_.area.slug) shouldBe Seq("central", "north", "east", "south", "west")
    // Bigger than any sub-divided US metro but one, and still not a US metro.
    London.cinemas.size shouldBe 133
  }
}
