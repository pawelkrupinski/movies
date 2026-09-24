package tools

import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EventuallySpec extends AnyFlatSpec with Matchers {

  "Eventually.eventually" should "report the check's own failure, not a NullPointerException, when the budget is already spent" in {
    val thrown = intercept[TestFailedException](Eventually.eventually(1 shouldBe 2, timeoutMs = 0))
    thrown.getMessage should include("1 was not equal to 2")
  }

  it should "give the check one last try at the deadline instead of failing on a sleep that overran it" in {
    val start = System.nanoTime() / 1000000
    // Passes only once 150ms have gone by; the poll interval (500ms) overshoots the whole
    // budget, so the only attempt that can pass is one made AFTER the deadline's sleep.
    Eventually.eventually(assert(System.nanoTime() / 1000000 - start >= 150), timeoutMs = 200, pollMs = 500)
  }
}
