package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A split city's [[CinemaAreaGroup]]s must PARTITION its `cinemas` — every venue
 * in exactly one area, none dropped, none duplicated. The filter renders one
 * group per area, so a venue missing from `londonAreaOf` would silently vanish
 * from the picker; these lock the partition and a few concrete placements.
 */
class CinemaAreaSpec extends AnyFlatSpec with Matchers {

  "London.areas" should "partition Cinema.london with no venue dropped or duplicated" in {
    val grouped = London.areas.flatMap(_.cinemas)
    grouped.toSet shouldBe Cinema.london.toSet
    grouped.distinct should have size grouped.size.toLong.toInt
    grouped should have size Cinema.london.size.toLong.toInt
  }

  it should "keep each group in Cinema.london's order" in {
    London.areas.foreach { g =>
      g.cinemas shouldBe Cinema.london.filter(g.cinemas.toSet)
    }
  }

  it should "expose the five compass areas in Central→North→East→South→West order, all non-empty" in {
    London.areas.map(_.area) shouldBe
      Seq(CinemaArea.Central, CinemaArea.North, CinemaArea.East, CinemaArea.South, CinemaArea.West)
    London.areas.foreach(_.cinemas should not be empty)
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

  "A flat city" should "have no areas and report isSplit == false" in {
    Poznan.areas shouldBe empty
    Poznan.isSplit shouldBe false
    London.isSplit shouldBe true
  }

  "CinemaArea.slug" should "be the kebab-cased label" in {
    CinemaArea.Central.slug shouldBe "central"
    CinemaArea.values.map(_.slug).distinct should have size CinemaArea.values.length.toLong.toInt
  }

  "CinemaAreaGroup.cinemaDisplayNames" should "list its cinemas' display names" in {
    val central = London.areas.find(_.area == CinemaArea.Central).get
    central.cinemaDisplayNames should contain("Curzon Soho")
  }
}
