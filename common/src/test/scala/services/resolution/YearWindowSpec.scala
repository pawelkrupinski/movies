package services.resolution

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YearWindowSpec extends AnyFlatSpec with Matchers {

  "agrees" should "say yes within the tolerance, inclusive at the edge" in {
    YearWindow.agrees(Some(2026), Some(2026), 1)  shouldBe Some(true)
    YearWindow.agrees(Some(2026), Some(2025), 1)  shouldBe Some(true)
    YearWindow.agrees(Some(2026), Some(2027), 1)  shouldBe Some(true)
    YearWindow.agrees(Some(1975), Some(1979), 15) shouldBe Some(true)
    YearWindow.agrees(Some(1996), Some(2011), 15) shouldBe Some(true)
  }

  it should "say no when both years are known and further apart than the tolerance" in {
    YearWindow.agrees(Some(2026), Some(2024), 1)  shouldBe Some(false)
    YearWindow.agrees(Some(2026), Some(1986), 1)  shouldBe Some(false)
    YearWindow.agrees(Some(1996), Some(2026), 15) shouldBe Some(false)
  }

  it should "stay silent when either side has no year" in {
    YearWindow.agrees(None, Some(2026), 1)              shouldBe None
    YearWindow.agrees(Some(2026), None, 1)              shouldBe None
    YearWindow.agrees(None, None, 15)                   shouldBe None
    YearWindow.agrees(Seq.empty[Int], Some(2026), 15)   shouldBe None
  }

  it should "agree when ANY of several years of ours is within the window" in {
    YearWindow.agrees(Seq(2019, 2026), Some(2027), 1) shouldBe Some(true)
    YearWindow.agrees(Seq(2019, 2021), Some(2027), 1) shouldBe Some(false)
  }

  "contradicts" should "be the positive-evidence reading: true only on a known gap, never on silence" in {
    YearWindow.contradicts(Some(2026), Some(2018), 1)  shouldBe true
    YearWindow.contradicts(Some(2026), Some(2025), 1)  shouldBe false
    YearWindow.contradicts(None, Some(2018), 1)        shouldBe false
    YearWindow.contradicts(Some(2026), None, 1)        shouldBe false
    YearWindow.contradicts(Seq(2021, 2026), Some(2018), 1) shouldBe true
    YearWindow.contradicts(Seq(2019, 2026), Some(2020), 1) shouldBe false
  }
}
