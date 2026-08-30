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

  "A big US state" should "be split by metro area, biggest metro first" in {
    val california = state("california")
    california.isSplit shouldBe true
    california.areas.size should be > 5
    california.areas.head.area.label shouldBe "Los Angeles"
    // Biggest metro first; the catch-all is pinned last whatever its size.
    val metroSizes = california.areas.filterNot(_.area == CinemaArea.Other).map(_.cinemas.size)
    metroSizes shouldBe metroSizes.sorted.reverse
  }

  it should "read the metro label off the venue's Flicks region" in {
    val texas = state("texas").areas.map(g => g.area.label -> g.area.slug)
    texas should contain("Dallas Fort Worth" -> "dallas-fort-worth")
    texas should contain("Houston" -> "houston")
  }

  "Venues Flicks files under no metro" should "land in a labelled catch-all, listed last" in {
    val california = state("california")
    california.areas.last.area shouldBe CinemaArea.Other
    california.areas.last.cinemas should not be empty
    CinemaArea.Other.slug shouldBe "other-areas"
  }

  "A US state with too few cinemas to be worth grouping" should "stay flat" in {
    state("rhode-island").isSplit shouldBe false
    state("delaware").isSplit shouldBe false
  }

  "A US state Flicks covers with a single metro" should "stay flat" in {
    state("puerto-rico").isSplit shouldBe false
  }

  "A flat city" should "have no areas and report isSplit == false" in {
    Poznan.areas shouldBe empty
    Poznan.isSplit shouldBe false
    London.isSplit shouldBe true
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
