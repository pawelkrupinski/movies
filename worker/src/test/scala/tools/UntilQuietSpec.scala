package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UntilQuietSpec extends AnyFlatSpec with Matchers {

  "a phase that goes quiet" should "run until its first idle round, and say how many it took" in {
    UntilQuiet("ratings", maxRounds = 5)(n => if (n < 3) 7 else 0) shouldBe 3
  }

  "a phase that never goes quiet" should "fail after its bound instead of looping forever" in {
    var rounds = 0
    val e = intercept[IllegalStateException](UntilQuiet("ratings", maxRounds = 5) { _ => rounds += 1; 1392 })
    rounds shouldBe 5
    e.getMessage should (include("ratings") and include("5 rounds") and include("1392"))
  }
}
