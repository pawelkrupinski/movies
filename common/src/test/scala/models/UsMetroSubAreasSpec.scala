package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The five US metros too big to browse as one list are sub-divided by compass
 * the way London is — but from the venues' own coordinates rather than a
 * hand-written map. These lock the threshold, the partition, the compass
 * identities, and the placements a resident would check first.
 */
class UsMetroSubAreasSpec extends AnyFlatSpec with Matchers {

  private def metro(stateSlug: String, metroSlug: String): CinemaAreaGroup =
    City.allModelled.find(_.slug == stateSlug).flatMap(_.areaBySlug(metroSlug))
      .getOrElse(fail(s"no metro '$metroSlug' in '$stateSlug'"))

  private def sub(stateSlug: String, metroSlug: String): Seq[CinemaAreaGroup] =
    UsMetroSubAreas.forMetro(stateSlug, metroSlug)

  /** Compass label → venue count, in the compass's own display order. */
  private def breakdown(stateSlug: String, metroSlug: String): Seq[(String, Int)] =
    sub(stateSlug, metroSlug).map(g => g.area.label -> g.cinemas.size)

  private def area(stateSlug: String, metroSlug: String, label: String): CinemaAreaGroup =
    sub(stateSlug, metroSlug).find(_.area.label == label)
      .getOrElse(fail(s"'$metroSlug' has no '$label' sub-area"))

  /** Every metro the threshold catches, as (state, metro) slug pairs. */
  private val qualifying = Seq(
    "california" -> "los-angeles", "new-york" -> "new-york", "california" -> "san-francisco",
    "texas" -> "dallas-fort-worth", "illinois" -> "chicago",
  )

  "The compass threshold" should "catch exactly the metros at or past 75 venues" in {
    val split = UsRoster.regions.flatMap(r => r.areas.map(g => (r.slug, g.area.slug, g.cinemas.size)))
      .filter { case (s, m, _) => sub(s, m).nonEmpty }
    split.map { case (s, m, _) => s -> m } should contain theSameElementsAs qualifying
    split.foreach { case (s, m, n) =>
      withClue(s"$s/$m: ")(n should be >= UsMetroSubAreas.MinCinemasForCompassSplit)
    }
  }

  it should "leave the next metros down flat" in {
    // Seattle (70) and Boston (62) are the two nearest misses.
    metro("washington", "seattle").cinemas.size shouldBe 70
    sub("washington", "seattle") shouldBe empty
    metro("massachusetts", "boston").cinemas.size shouldBe 62
    sub("massachusetts", "boston") shouldBe empty
  }

  it should "answer an unknown state or metro with no sub-areas" in {
    sub("atlantis", "los-angeles") shouldBe empty
    sub("california", "atlantis") shouldBe empty
    // A flat state has no metros at all to sub-divide.
    sub("rhode-island", "providence") shouldBe empty
  }

  "Each split metro's sub-areas" should "partition it with none dropped or duplicated" in {
    qualifying.foreach { case (stateSlug, metroSlug) =>
      withClue(s"$stateSlug/$metroSlug: ") {
        val whole   = metro(stateSlug, metroSlug).cinemas
        val grouped = sub(stateSlug, metroSlug).flatMap(_.cinemas)
        grouped.distinct should have size grouped.size.toLong.toInt
        grouped.toSet shouldBe whole.toSet
        grouped should have size whole.size.toLong.toInt
      }
    }
  }

  it should "reuse London's compass singletons, in the same display order, none empty" in {
    qualifying.foreach { case (stateSlug, metroSlug) =>
      withClue(s"$stateSlug/$metroSlug: ") {
        val areas = sub(stateSlug, metroSlug).map(_.area)
        areas shouldBe CinemaArea.compass
        areas.map(_.slug) shouldBe Seq("central", "north", "east", "south", "west")
        sub(stateSlug, metroSlug).foreach(_.cinemas should not be empty)
      }
    }
  }

  it should "keep each sub-area in the metro's own cinema order" in {
    qualifying.foreach { case (stateSlug, metroSlug) =>
      val whole = metro(stateSlug, metroSlug).cinemas
      sub(stateSlug, metroSlug).foreach { g =>
        withClue(s"$stateSlug/$metroSlug/${g.area.slug}: ")(g.cinemas shouldBe whole.filter(g.cinemaSet))
      }
    }
  }

  it should "be byte-identical on a second call" in {
    qualifying.foreach { case (stateSlug, metroSlug) =>
      sub(stateSlug, metroSlug) shouldBe sub(stateSlug, metroSlug)
    }
  }

  // ── The measured breakdowns. A change to the radius or the centroid moves
  //    these, which is exactly when someone should have to look again. ────────

  "Los Angeles" should "split into a downtown core with the coast to its west" in {
    breakdown("california", "los-angeles") shouldBe
      Seq("Central" -> 33, "North" -> 20, "East" -> 29, "South" -> 24, "West" -> 27)
    // Hollywood / Downtown / Beverly Hills are the core.
    area("california", "los-angeles", "Central").cinemaDisplayNames should
      contain allOf ("Alamo Drafthouse Downtown LA", "Chinese Theatre Hollywood", "AMC The Grove 14")
    // The coast — the check the whole split exists to pass.
    area("california", "los-angeles", "West").cinemaDisplayNames should
      contain allOf ("AMC Santa Monica 7", "Aero Theatre Santa Monica", "AMC DINE-IN Theatres Marina Marina Del Rey 6")
    area("california", "los-angeles", "North").cinemaDisplayNames should contain("AMC Burbank 16")
    area("california", "los-angeles", "East").cinemaDisplayNames should contain("IPIC Pasadena")
    area("california", "los-angeles", "South").cinemaDisplayNames should contain("Art Theatre of Long Beach")
  }

  "New York" should "separate the city from Long Island and Westchester" in {
    breakdown("new-york", "new-york") shouldBe
      Seq("Central" -> 33, "North" -> 14, "East" -> 21, "South" -> 5, "West" -> 29)
    // The metro is the STATE's share of it, so Long Island drags the centroid
    // into Queens and Manhattan lands west of it — Midtown and downtown in
    // West, upper Manhattan and the western boroughs in Central.
    area("new-york", "new-york", "West").cinemaDisplayNames should
      contain allOf ("AMC Empire 25", "Film Forum New York", "IFC Center New York")
    area("new-york", "new-york", "Central").cinemaDisplayNames should
      contain allOf ("AMC Lincoln Square 13", "Museum of the Moving Image Astoria")
    area("new-york", "new-york", "East").cinemaDisplayNames should contain("AMC Stony Brook 17")   // Suffolk
    area("new-york", "new-york", "North").cinemaDisplayNames should contain("Alamo Drafthouse Yonkers")
    area("new-york", "new-york", "South").cinemaDisplayNames should contain("Regal UA Sheepshead Bay")
  }

  "San Francisco" should "put the city west of the bay and the peninsula south" in {
    breakdown("california", "san-francisco") shouldBe
      Seq("Central" -> 6, "North" -> 20, "East" -> 7, "South" -> 22, "West" -> 24)
    area("california", "san-francisco", "West").cinemaDisplayNames should
      contain allOf ("Castro Theatre", "Roxie Theatre", "AMC Metreon 16 San Francisco")
    area("california", "san-francisco", "South").cinemaDisplayNames should
      contain allOf ("Cinemark San Jose Oakridge 20", "Cinemark Century Mountain View 16")
    area("california", "san-francisco", "North").cinemaDisplayNames should
      contain allOf ("Grand Lake Theatre Oakland", "Berkeley Art Museum & Pacific Film Archive")
    area("california", "san-francisco", "East").cinemaDisplayNames should contain("Vine Cinema Livermore")
  }

  "Dallas Fort Worth" should "put Dallas east, Fort Worth west and the mid-cities in the middle" in {
    breakdown("texas", "dallas-fort-worth") shouldBe
      Seq("Central" -> 6, "North" -> 16, "East" -> 20, "South" -> 14, "West" -> 22)
    area("texas", "dallas-fort-worth", "East").cinemaDisplayNames should
      contain allOf ("AMC North Park 15 Dallas", "Majestic Theatre Dallas")
    area("texas", "dallas-fort-worth", "West").cinemaDisplayNames should
      contain allOf ("AMC Hulen 10", "Movie Tavern West 7th Street")
    area("texas", "dallas-fort-worth", "Central").cinemaDisplayNames should contain("AMC Irving Mall 14")
    area("texas", "dallas-fort-worth", "North").cinemaDisplayNames should contain("Cinemark Denton 14")
  }

  "Chicago" should "split the lakefront city off from its suburbs" in {
    breakdown("illinois", "chicago") shouldBe
      Seq("Central" -> 15, "North" -> 18, "East" -> 18, "South" -> 14, "West" -> 13)
    // Lake Michigan bounds the metro on the east, so the venue centroid sits out
    // in the western suburbs and the Loop reads as EAST of it.
    area("illinois", "chicago", "East").cinemaDisplayNames should
      contain allOf ("AMC River East Chicago 21", "Music Box Theatre Chicago", "Gene Siskel Film Center Chicago")
    area("illinois", "chicago", "North").cinemaDisplayNames should contain("AMC Evanston 12")
    area("illinois", "chicago", "South").cinemaDisplayNames should contain("Marcus Orland Park Cinema")
    area("illinois", "chicago", "West").cinemaDisplayNames should contain("AMC Naperville 16")
  }

  // ── London stays hand-written ───────────────────────────────────────────────

  "London" should "keep its hand-written compass map, untouched by the US split" in {
    London.areas.map(g => g.area.label -> g.cinemas.size) shouldBe
      Seq("Central" -> 29, "North" -> 24, "East" -> 16, "South" -> 45, "West" -> 19)
    // Past the US threshold (133 venues) and still not a US metro.
    London.cinemas.size should be >= UsMetroSubAreas.MinCinemasForCompassSplit
    UsMetroSubAreas.forMetro("london", "central") shouldBe empty
  }

  // ── The geometry seam ───────────────────────────────────────────────────────

  "CompassPlacement" should "average a metro's venues into its centroid" in {
    CompassPlacement.centroid(Seq(GeoPoint(10, 20), GeoPoint(20, 40))) shouldBe GeoPoint(15, 30)
  }

  it should "call anything inside the central radius Central, whatever its bearing" in {
    val centre = GeoPoint(34.0, -118.0)
    // ~11 km north and ~11 km east: both inside a 12 km radius.
    CompassPlacement.areaOf(centre, GeoPoint(34.1, -118.0), 12.0) shouldBe CinemaArea.Central
    CompassPlacement.areaOf(centre, GeoPoint(34.0, -117.88), 12.0) shouldBe CinemaArea.Central
    CompassPlacement.areaOf(centre, centre, 12.0) shouldBe CinemaArea.Central
  }

  it should "take the compass point of a bearing outside it, splitting at the diagonals" in {
    val centre = GeoPoint(34.0, -118.0)
    CompassPlacement.areaOf(centre, GeoPoint(35.0, -118.0), 12.0) shouldBe CinemaArea.North
    CompassPlacement.areaOf(centre, GeoPoint(33.0, -118.0), 12.0) shouldBe CinemaArea.South
    CompassPlacement.areaOf(centre, GeoPoint(34.0, -117.0), 12.0) shouldBe CinemaArea.East
    CompassPlacement.areaOf(centre, GeoPoint(34.0, -119.0), 12.0) shouldBe CinemaArea.West
    // The boundaries are half-open, so a bearing either side of a diagonal
    // lands in exactly one area and none falls between two.
    CompassPlacement.areaOf(centre, GeoPoint(34.6, -117.20), 12.0) shouldBe CinemaArea.East   // 47.5°
    CompassPlacement.areaOf(centre, GeoPoint(34.6, -118.72), 12.0) shouldBe CinemaArea.North  // 315.5°
    CompassPlacement.areaOf(centre, GeoPoint(34.6, -118.80), 12.0) shouldBe CinemaArea.West   // 312.5°
  }

  it should "measure the great-circle distance between two points" in {
    // Los Angeles → New York, ~3,936 km.
    CompassPlacement.distanceKm(GeoPoint(34.05, -118.25), GeoPoint(40.71, -74.01)) shouldBe 3936.0 +- 5.0
    CompassPlacement.distanceKm(GeoPoint(34.05, -118.25), GeoPoint(34.05, -118.25)) shouldBe 0.0
  }
}
