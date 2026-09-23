package models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GeoPointSpec extends AnyFlatSpec with Matchers {

  "kmTo" should "measure the great-circle distance between two points" in {
    val poznan   = GeoPoint(52.4064, 16.9252)
    val warszawa = GeoPoint(52.2297, 21.0122)
    poznan.kmTo(warszawa) shouldBe 279.0 +- 1.0
    warszawa.kmTo(poznan) shouldBe poznan.kmTo(warszawa)
    poznan.kmTo(poznan) shouldBe 0.0
  }
}
