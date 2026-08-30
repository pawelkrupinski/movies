package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The five US metros too big to browse as one list are sub-divided into the
 * districts a local names — clustered from the venues' own coordinates and
 * named after the town each cluster centres on, the way the metros themselves
 * are. These lock which metros take part, the partition, and the full name list
 * for each: the names are the whole point, so they are pinned outright.
 */
class UsMetroSubAreasSpec extends AnyFlatSpec with Matchers {

  private def metro(stateSlug: String, metroSlug: String): CinemaAreaGroup =
    City.allModelled.find(_.slug == stateSlug).flatMap(_.areaBySlug(metroSlug))
      .getOrElse(fail(s"no metro '$metroSlug' in '$stateSlug'"))

  private def sub(stateSlug: String, metroSlug: String): Seq[CinemaAreaGroup] =
    UsMetroSubAreas.forMetro(stateSlug, metroSlug)

  /** Sub-area label → venue count, in the order the groups are offered in. */
  private def districts(stateSlug: String, metroSlug: String): Seq[(String, Int)] =
    sub(stateSlug, metroSlug).map(g => g.area.label -> g.cinemas.size)

  private def district(stateSlug: String, metroSlug: String, label: String): CinemaAreaGroup =
    sub(stateSlug, metroSlug).find(_.area.label == label)
      .getOrElse(fail(s"'$metroSlug' has no '$label' district"))

  private val subDivided = Seq(
    "california" -> "los-angeles", "new-york" -> "new-york", "california" -> "san-francisco",
    "texas" -> "dallas-fort-worth", "illinois" -> "chicago",
  )

  "Sub-division" should "reach exactly the metros at or past 75 venues" in {
    val split = UsRoster.regions.flatMap(r => r.areas.map(g => (r.slug, g.area.slug, g.cinemas.size)))
      .filter { case (s, m, _) => sub(s, m).nonEmpty }
    split.map { case (s, m, _) => s -> m } should contain theSameElementsAs subDivided
    // `cluster_metros.MIN_VENUES_TO_SUBDIVIDE`, which the generator applies.
    split.foreach { case (s, m, n) => withClue(s"$s/$m: ")(n should be >= 75) }
  }

  it should "leave the next metros down as one list" in {
    // Seattle (70) and Boston (62) are the two nearest misses.
    metro("washington", "seattle").cinemas.size shouldBe 70
    sub("washington", "seattle") shouldBe empty
    metro("massachusetts", "boston").cinemas.size shouldBe 62
    sub("massachusetts", "boston") shouldBe empty
  }

  it should "answer an unknown state or metro with no districts" in {
    sub("atlantis", "los-angeles") shouldBe empty
    sub("california", "atlantis") shouldBe empty
    // A flat state has no metros at all to sub-divide.
    sub("rhode-island", "providence") shouldBe empty
  }

  "Each metro's districts" should "partition it with none dropped or duplicated" in {
    subDivided.foreach { case (stateSlug, metroSlug) =>
      withClue(s"$stateSlug/$metroSlug: ") {
        val whole   = metro(stateSlug, metroSlug).cinemas
        val grouped = sub(stateSlug, metroSlug).flatMap(_.cinemas)
        grouped.distinct should have size grouped.size.toLong.toInt
        grouped.toSet shouldBe whole.toSet
        grouped should have size whole.size.toLong.toInt
      }
    }
  }

  it should "come biggest first, each labelled and slugged uniquely in the metro" in {
    subDivided.foreach { case (stateSlug, metroSlug) =>
      withClue(s"$stateSlug/$metroSlug: ") {
        val groups = sub(stateSlug, metroSlug)
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
    subDivided.foreach { case (stateSlug, metroSlug) =>
      val whole = metro(stateSlug, metroSlug).cinemas
      sub(stateSlug, metroSlug).foreach { g =>
        withClue(s"$stateSlug/$metroSlug/${g.area.slug}: ")(g.cinemas shouldBe whole.filter(g.cinemaSet))
      }
    }
  }

  it should "be named after places, never after a compass point" in {
    // What this split replaced: compass areas describe London, not a US metro.
    val labels = subDivided.flatMap { case (s, m) => sub(s, m).map(_.area) }.toSet
    labels intersect CinemaArea.compass.toSet shouldBe empty
  }

  it should "be byte-identical on a second call" in {
    subDivided.foreach { case (stateSlug, metroSlug) =>
      sub(stateSlug, metroSlug) shouldBe sub(stateSlug, metroSlug)
    }
  }

  // ── The names. Pinned outright: a shifted radius, fold or rename shows up
  //    here as the list a reader can judge, not as a count. ──────────────────

  "New York" should "fall out as the boroughs, then the Long Island and Westchester towns" in {
    districts("new-york", "new-york") shouldBe Seq(
      "Manhattan" -> 40, "Brooklyn" -> 12, "Forest Hills" -> 7, "Yonkers" -> 5,
      "Bellmore" -> 4, "Staten Island" -> 4, "Farmingdale" -> 3, "Nanuet" -> 3,
      "New Rochelle" -> 3, "Stony Brook" -> 3, "Westbury" -> 3, "White Plains" -> 3,
      "Bayside" -> 2, "Huntington" -> 2, "Lynbrook" -> 2, "Manhasset" -> 2,
      "Northport" -> 2, "The Bronx" -> 2,
    )
    district("new-york", "new-york", "Manhattan").cinemaDisplayNames should
      contain allOf ("Film Forum New York", "AMC Empire 25", "AMC Lincoln Square 13")
    district("new-york", "new-york", "Brooklyn").cinemaDisplayNames should
      contain allOf ("BAM Rose Cinemas", "Nitehawk Cinema Williamsburg")
    district("new-york", "new-york", "The Bronx").cinemaDisplayNames shouldBe
      Seq("AMC Bay Plaza 13 Bronx", "Regal Concourse")
    district("new-york", "new-york", "Staten Island").cinemaDisplayNames should
      contain("Alamo Drafthouse Staten Island")
    // Queens arrives as its neighbourhoods, which is how its venues are filed.
    district("new-york", "new-york", "Forest Hills").cinemaDisplayNames should
      contain allOf ("Cinemart Cinemas Forest Hills", "Kew Gardens Cinemas")
  }

  "Los Angeles" should "read as the towns of greater LA" in {
    districts("california", "los-angeles") shouldBe Seq(
      "Los Angeles" -> 32, "Downey" -> 8, "Burbank" -> 6, "Glendale" -> 6, "Pasadena" -> 6,
      "Long Beach" -> 5, "Montebello" -> 5, "Torrance" -> 5, "Buena Park" -> 4,
      "Calabasas" -> 4, "Garden Grove" -> 4, "North Hollywood" -> 4, "Santa Monica" -> 4,
      "Thousand Oaks" -> 4, "Cerritos" -> 3, "Covina" -> 3, "El Segundo" -> 3,
      "Inglewood" -> 3, "La Habra" -> 3, "Lancaster" -> 3, "Marina del Rey" -> 3,
      "Northridge" -> 3, "Santa Clarita" -> 3, "Arcadia" -> 2, "Camarillo" -> 2,
      "Encino" -> 2, "Simi Valley" -> 2, "Avalon" -> 1,
    )
    district("california", "los-angeles", "Santa Monica").cinemaDisplayNames should
      contain allOf ("AMC Santa Monica 7", "Aero Theatre Santa Monica")
    district("california", "los-angeles", "Pasadena").cinemaDisplayNames should contain("IPIC Pasadena")
    district("california", "los-angeles", "Burbank").cinemaDisplayNames should contain("AMC Burbank 16")
    district("california", "los-angeles", "Long Beach").cinemaDisplayNames should
      contain("Art Theatre of Long Beach")
    // Hollywood, Downtown and the Westside are all filed under the city itself.
    district("california", "los-angeles", "Los Angeles").cinemaDisplayNames should
      contain allOf ("Chinese Theatre Hollywood", "Alamo Drafthouse Downtown LA")
    // Catalina Island keeps its own: 35 km of sea from the nearest district.
    district("california", "los-angeles", "Avalon").cinemaDisplayNames shouldBe
      Seq("Avalon Theatre (Catalina Casino)")
  }

  "San Francisco" should "read as the city, the East Bay, the Peninsula and the South Bay" in {
    districts("california", "san-francisco") shouldBe Seq(
      "San Francisco" -> 16, "San Jose" -> 10, "Oakland" -> 9, "Larkspur" -> 6,
      "Concord" -> 5, "Hayward" -> 5, "Brentwood" -> 3, "Fremont" -> 3, "Moraga" -> 3,
      "Palo Alto" -> 3, "San Mateo" -> 3, "Livermore" -> 2, "Mountain View" -> 2,
      "Novato" -> 2, "Richmond" -> 2, "San Ramon" -> 2, "Santa Clara" -> 2, "Vallejo" -> 1,
    )
    district("california", "san-francisco", "San Francisco").cinemaDisplayNames should
      contain allOf ("Castro Theatre", "Roxie Theatre", "AMC Metreon 16 San Francisco")
    district("california", "san-francisco", "Oakland").cinemaDisplayNames should
      contain allOf ("Grand Lake Theatre Oakland", "Berkeley Art Museum & Pacific Film Archive")
    district("california", "san-francisco", "San Jose").cinemaDisplayNames should
      contain("Cinemark San Jose Oakridge 20")
    // Flicks files the Stanford and the Aquarius under East Palo Alto; both are
    // in Palo Alto, which is also the name a local uses.
    district("california", "san-francisco", "Palo Alto").cinemaDisplayNames should
      contain allOf ("Stanford Palo Alto", "Landmark Aquarius")
  }

  "Dallas Fort Worth" should "read as Dallas, Fort Worth and the mid-cities" in {
    districts("texas", "dallas-fort-worth") shouldBe Seq(
      "Fort Worth" -> 16, "Dallas" -> 14, "Arlington" -> 4, "Frisco" -> 4, "Grapevine" -> 4,
      "Plano" -> 4, "Bedford" -> 3, "Denton" -> 3, "Grand Prairie" -> 3, "Keller" -> 3,
      "Red Oak" -> 3, "Burleson" -> 2, "Garland" -> 2, "Granbury" -> 2, "Irving" -> 2,
      "Lewisville" -> 2, "Mansfield" -> 2, "The Colony" -> 2, "Cleburne" -> 1,
      "Decatur" -> 1, "Weatherford" -> 1,
    )
    district("texas", "dallas-fort-worth", "Dallas").cinemaDisplayNames should
      contain allOf ("AMC North Park 15 Dallas", "Majestic Theatre Dallas")
    district("texas", "dallas-fort-worth", "Fort Worth").cinemaDisplayNames should
      contain allOf ("AMC Hulen 10", "Movie Tavern West 7th Street")
    // Flicks splits Arlington between "Arlington" and "Arlington Heights"; the
    // rename puts one town's four cinemas back in one district.
    district("texas", "dallas-fort-worth", "Arlington").cinemaDisplayNames should
      contain allOf ("LOOK Cinemas Arlington", "AMC The Parks at Arlington 18")
  }

  "Chicago" should "read as the city and its named suburbs" in {
    districts("illinois", "chicago") shouldBe Seq(
      "Chicago" -> 22, "Country Club Hills" -> 4, "Elk Grove" -> 4, "Frankfort" -> 4,
      "Niles" -> 4, "Oak Brook" -> 4, "Evanston" -> 3, "Glen Ellyn" -> 3, "Naperville" -> 3,
      "Norridge" -> 3, "North Riverside" -> 3, "Northbrook" -> 3, "Wheeling" -> 3,
      "Woodridge" -> 3, "Bolingbrook" -> 2, "Gurnee" -> 2, "New Lenox" -> 2, "Skokie" -> 2,
      "South Barrington" -> 2, "Vernon Hills" -> 2,
    )
    // The Loop and the North Side are all filed under Chicago itself.
    district("illinois", "chicago", "Chicago").cinemaDisplayNames should
      contain allOf ("AMC River East Chicago 21", "Music Box Theatre Chicago",
                     "Gene Siskel Film Center Chicago")
    district("illinois", "chicago", "Evanston").cinemaDisplayNames should contain("AMC Evanston 12")
    district("illinois", "chicago", "Naperville").cinemaDisplayNames should contain("AMC Naperville 16")
  }

  // ── London is not part of this ──────────────────────────────────────────────

  "London" should "keep its hand-written compass map exactly" in {
    London.areas.map(g => g.area.label -> g.cinemas.size) shouldBe
      Seq("Central" -> 29, "North" -> 24, "East" -> 16, "South" -> 45, "West" -> 19)
    London.areas.map(_.area) shouldBe CinemaArea.compass
    London.areas.map(_.area.slug) shouldBe Seq("central", "north", "east", "south", "west")
    // Bigger than any sub-divided US metro but one, and still not a US metro.
    London.cinemas.size shouldBe 133
    UsMetroSubAreas.forMetro("london", "central") shouldBe empty
  }
}
