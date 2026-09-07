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

  "agrees between two sets" should "agree on any pair within the window and contradict only when every pair is outside it" in {
    YearWindow.agrees(Set(1999), Set(2000), 1)         shouldBe Some(true)
    YearWindow.agrees(Set(1999), Set(2001), 1)         shouldBe Some(false)
    YearWindow.agrees(Set(1999, 2025), Set(2026), 1)   shouldBe Some(true)
    YearWindow.contradicts(Set(1999), Set(2025), 1)    shouldBe true
    YearWindow.contradicts(Set(1999), Set(2000), 1)    shouldBe false
  }

  it should "stay silent when either side published nothing" in {
    YearWindow.agrees(Set.empty[Int], Set(2026), 1)      shouldBe None
    YearWindow.agrees(Set(2026), Set.empty[Int], 1)      shouldBe None
    YearWindow.contradicts(Set.empty[Int], Set(2026), 1) shouldBe false
    YearWindow.contradicts(Set(2026), Set.empty[Int], 1) shouldBe false
  }

  "distance" should "be the absolute gap, either way round" in {
    YearWindow.distance(1989, 1991) shouldBe 2
    YearWindow.distance(1991, 1989) shouldBe 2
    YearWindow.distance(2026, 2026) shouldBe 0
  }
}
