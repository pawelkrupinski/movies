package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `Cinema.byCity` groups every modelled venue under its city's display label
 * and is the single source of truth `all` flattens from. The uptime page reads
 * it to render one subsection per city, so a venue silently dropped from a city
 * group would vanish from the page — these lock the grouping to `all`.
 */
class CinemaByCitySpec extends AnyFlatSpec with Matchers {

  "Cinema.byCity" should "cover exactly the venues in Cinema.all, in the same order" in {
    Cinema.byCity.flatMap(_._2) shouldBe Cinema.all
  }

  it should "partition the venues with no cinema dropped or duplicated across cities" in {
    val grouped = Cinema.byCity.flatMap(_._2)
    grouped.distinct should have size grouped.size.toLong.toInt
    grouped.toSet shouldBe Cinema.all.toSet
  }

  it should "label each group with a non-empty, unique city name" in {
    val labels = Cinema.byCity.map(_._1)
    labels.foreach(_.trim should not be empty)
    labels.distinct shouldBe labels
  }

  it should "place a venue under its real city" in {
    def cityOf(c: Cinema): String = Cinema.byCity.collectFirst { case (city, vs) if vs.contains(c) => city }.get
    cityOf(Multikino) shouldBe "Poznań"          // Multikino Stary Browar
    cityOf(CinemaCityArkadia) shouldBe "Warszawa"
    cityOf(MultikinoKrakow) shouldBe "Kraków"
  }
}
