package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A split city's [[CinemaAreaGroup]]s must PARTITION its `cinemas` — every venue
 * in exactly one area, none dropped, none duplicated. The filter renders one
 * group per area, so a venue missing from `londonAreaOf` (or from a US state's
 * metro map) would silently vanish from the picker; these lock the partition for
 * EVERY split city, plus a few concrete placements.
 */
class CinemaAreaSpec extends AnyFlatSpec with Matchers {

  /** Every city that opts into areas, disabled ones included — the invariants
   *  below hold for the model, not just for the currently-live roster. */
  private val splitCities: Seq[City] = City.allModelled.filter(_.isSplit)

  private def state(slug: String): City =
    City.allModelled.find(_.slug == slug).getOrElse(fail(s"no city '$slug'"))

  /** Every area of every US state — the whole clustered partition. */
  private val usAreas: Seq[CinemaAreaGroup] = UsRoster.regions.flatMap(_.areas)

  /** The area a named venue was filed under. */
  private def areaOf(stateSlug: String, displayName: String): CinemaAreaGroup =
    state(stateSlug).areas
      .find(_.cinemaDisplayNames.contains(displayName))
      .getOrElse(fail(s"no area in '$stateSlug' holds '$displayName'"))

  "Every split city" should "partition its cinemas with none dropped or duplicated" in {
    splitCities.size should be > 1
    splitCities.foreach { city =>
      withClue(s"${city.slug}: ") {
        val grouped = city.areas.flatMap(_.cinemas)
        grouped.distinct should have size grouped.size.toLong.toInt
        grouped.toSet shouldBe city.cinemas.toSet
        grouped should have size city.cinemas.size.toLong.toInt
      }
    }
  }

  it should "keep each group non-empty, in the city's own cinema order" in {
    splitCities.foreach { city =>
      city.areas.foreach { g =>
        withClue(s"${city.slug}/${g.area.slug}: ") {
          g.cinemas should not be empty
          g.cinemas shouldBe city.cinemas.filter(g.cinemas.toSet)
        }
      }
    }
  }

  it should "carry more than one area, each with a non-empty label and a slug unique in the city" in {
    splitCities.foreach { city =>
      withClue(s"${city.slug}: ") {
        city.areas.size should be > 1
        city.areas.foreach(_.area.label.trim should not be empty)
        val slugs = city.areas.map(_.area.slug)
        slugs.foreach(_ should fullyMatch regex "[a-z0-9]+(-[a-z0-9]+)*")
        slugs.distinct should have size slugs.size.toLong.toInt
      }
    }
  }

  "London.areas" should "expose the five compass areas in Central→North→East→South→West order" in {
    London.areas.map(_.area) shouldBe
      Seq(CinemaArea.Central, CinemaArea.North, CinemaArea.East, CinemaArea.South, CinemaArea.West)
    London.areas.map(_.area.slug) shouldBe Seq("central", "north", "east", "south", "west")
  }

  it should "place venues in their real compass area" in {
    def areaOf(c: Cinema): CinemaArea =
      London.areas.collectFirst { case g if g.cinemas.contains(c) => g.area }.get
    areaOf(CurzonSoho) shouldBe CinemaArea.Central
    areaOf(TheRitzyPicturehouseBrixton) shouldBe CinemaArea.South   // Brixton, south of the Thames
    areaOf(GenesisTowerHamlets) shouldBe CinemaArea.East            // Whitechapel
    areaOf(EverymanCinemaHampstead) shouldBe CinemaArea.North
    areaOf(RiversideStudiosHammersmith) shouldBe CinemaArea.West
  }

  "A big US state" should "be split into distance-clustered metros, biggest first" in {
    val california = state("california")
    california.isSplit shouldBe true
    // 486 venues over 21 clusters, not the 33 raw Flicks metros they arrived in.
    california.areas.size shouldBe 21
    california.areas.head.area.label shouldBe "Los Angeles"
    california.areas.head.cinemas.size shouldBe 133
    val sizes = california.areas.map(_.cinemas.size)
    sizes shouldBe sizes.sorted.reverse
  }

  it should "name each metro after the place it centres on" in {
    val texas = state("texas").areas.map(g => g.area.label -> g.area.slug)
    texas should contain("Dallas Fort Worth" -> "dallas-fort-worth")
    texas should contain("Houston" -> "houston")
  }

  "Flicks metros inside one travel-shed" should "cluster into a single area" in {
    // `dallas` and `fort-worth` are separate Flicks metros and one metroplex.
    val metroplex = areaOf("texas", "AMC North Park 15 Dallas")
    metroplex.area.label shouldBe "Dallas Fort Worth"
    metroplex.cinemaDisplayNames should contain("AMC Hulen 10")          // Fort Worth
    // Greater Cleveland clusters on Akron — whose own cinemas outnumber
    // Cleveland's, those being spread over a dozen separately-named suburbs —
    // but keeps the name a visitor is looking for.
    val cleveland = areaOf("ohio", "Capitol Theatre Cleveland")
    cleveland.area.label shouldBe "Cleveland"
    cleveland.cinemaDisplayNames should contain("Highland Theater Akron")
  }

  "A venue Flicks files under no metro" should "join the metro nearest it" in {
    // No `region_slug`, so the raw grouping dumped it in the catch-all.
    areaOf("alabama", "Alabama Theatre Birmingham").area.label shouldBe "Birmingham"
    usAreas.map(_.area) should not contain CinemaArea.Other
  }

  "A US metro" should "be big enough to be worth its own group" in {
    // Only a venue with no neighbouring metro within 150 km keeps an area of
    // its own; everything else is folded into the metro nearest it.
    usAreas.count(_.cinemas.sizeIs <= 2) should be <= 20
  }

  "A US state with too few cinemas to be worth grouping" should "stay flat" in {
    state("rhode-island").isSplit shouldBe false
    state("delaware").isSplit shouldBe false
  }

  "A flat city" should "have no areas and report isSplit == false" in {
    Poznan.areas shouldBe empty
    Poznan.isSplit shouldBe false
    London.isSplit shouldBe true
  }

  // ── The metro chooser gate (City.hasAreaChooser) ────────────────────────────

  "Every split US state" should "offer a metro chooser, not just the biggest ones" in {
    val splitStates = Country.UnitedStates.cities.filter(_.isSplit)
    // The whole point of dropping the old venue threshold: 46 states are split,
    // and every one of them is a list of metros rather than a city.
    splitStates.size should be > 20
    splitStates.foreach(s => withClue(s"${s.slug}: ")(s.hasAreaChooser shouldBe true))
    state("california").hasAreaChooser shouldBe true
    // A small split state — the case the 150-venue threshold used to exclude.
    splitStates.minBy(_.cinemas.size).hasAreaChooser shouldBe true
  }

  "A flat US state" should "keep serving its own listing" in {
    Country.UnitedStates.cities.filterNot(_.isSplit)
      .foreach(s => withClue(s"${s.slug}: ")(s.hasAreaChooser shouldBe false))
  }

  "London" should "stay a single listing — split, but not a state" in {
    // The screen the chooser is modelled on, deliberately NOT a target. It is
    // the heaviest page in the fleet and STILL stays one page: a product
    // decision, not a size one (see City.hasAreaChooser's doc).
    London.isSplit shouldBe true
    London.hasAreaChooser shouldBe false
  }

  "No city outside the US" should "ever offer a chooser, however it is split" in {
    City.allModelled.filter(_.country != Country.UnitedStates)
      .foreach(c => withClue(s"${c.slug}: ")(c.hasAreaChooser shouldBe false))
  }

  "A flat city" should "never offer a chooser however many venues it holds" in {
    City.all.filterNot(_.isSplit).foreach(c => withClue(s"${c.slug}: ")(c.hasAreaChooser shouldBe false))
  }

  "areaBySlug" should "resolve a real area and refuse an unknown one" in {
    val california = state("california")
    california.areaBySlug("los-angeles").map(_.area.label) shouldBe Some("Los Angeles")
    california.areaBySlug("atlantis") shouldBe None
    // A flat city has nothing to resolve.
    Poznan.areaBySlug("central") shouldBe None
  }

  "CinemaArea.slug" should "be the kebab-cased label" in {
    CinemaArea.Central.slug shouldBe "central"
    CinemaArea("Des Moines").slug shouldBe "des-moines"
    CinemaArea.compass.map(_.slug).distinct should have size CinemaArea.compass.size.toLong.toInt
  }

  "CinemaAreaGroup.cinemaDisplayNames" should "list its cinemas' display names" in {
    val central = London.areas.find(_.area == CinemaArea.Central).get
    central.cinemaDisplayNames should contain("Curzon Soho")
  }
}
