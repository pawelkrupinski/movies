package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LogThrottleSpec extends AnyFlatSpec with Matchers {

  "A log throttle" should "admit the first call, hold back the rest of the interval, then admit again with the count held back" in {
    var now      = 0L
    val throttle = new LogThrottle(intervalNanos = 100, nanoTime = () => now)
    throttle.admit() shouldBe Some(0)
    now = 50
    throttle.admit() shouldBe None
    throttle.admit() shouldBe None
    now = 100
    throttle.admit() shouldBe Some(2)
    throttle.admit() shouldBe None
  }

  it should "keep its allowance to itself" in {
    val first  = new LogThrottle(intervalNanos = Long.MaxValue)
    val second = new LogThrottle(intervalNanos = Long.MaxValue)
    first.admit() shouldBe Some(0)
    second.admit() shouldBe Some(0)
  }
}
