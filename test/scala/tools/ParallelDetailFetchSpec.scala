package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._

class ParallelDetailFetchSpec extends AnyFlatSpec with Matchers {

  "ParallelDetailFetch" should "return fetched results keyed by URL" in {
    val result = ParallelDetailFetch("test-happy", Seq("a", "b", "c"), 5.seconds) { url =>
      url.toUpperCase
    }
    result shouldBe Map("a" -> "A", "b" -> "B", "c" -> "C")
  }

  it should "return an empty map for empty URLs without creating an EC" in {
    val result = ParallelDetailFetch("test-empty", Seq.empty[String], 5.seconds) { _ =>
      fail("should not be called")
    }
    result shouldBe empty
  }

  it should "throw TimeoutException when a fetch exceeds the timeout" in {
    a[TimeoutException] should be thrownBy {
      ParallelDetailFetch("test-timeout", Seq("fast", "slow"), 500.millis) { url =>
        if (url == "slow") Thread.sleep(5000)
        url
      }
    }
  }

  it should "propagate exceptions from the fetch function" in {
    an[Exception] should be thrownBy {
      ParallelDetailFetch("test-error", Seq("ok", "boom"), 5.seconds) { url =>
        if (url == "boom") throw new RuntimeException("kaboom")
        url
      }
    }
  }
}
