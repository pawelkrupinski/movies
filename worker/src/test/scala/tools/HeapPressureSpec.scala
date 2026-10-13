package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The heap's live share ([[HeapPressure]]): a share of its maximum, pressed at a threshold it has reached. */
class HeapPressureSpec extends AnyFlatSpec with Matchers {
  "the heap held after a collection" should "be a share of its maximum, pressed at no more than it holds" in {
    System.gc()
    val held = HeapPressure.heldAfterCollection()
    held should (be > 0.0 and be < 1.0)
    HeapPressure.pressed(threshold = held / 2) shouldBe true
    HeapPressure.pressed(threshold = 1.0) shouldBe false
  }
}
