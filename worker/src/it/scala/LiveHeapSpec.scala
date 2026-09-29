package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LiveHeapSpec extends AnyFlatSpec with Matchers {

  "the live heap" should "grow by what a structure keeps, and not by garbage it left behind" in {
    val before  = LiveHeap.bytes()
    // Small objects, as a model is made of: G1 lays out objects of a region's size apart, and a
    // 1 MB array read about twice its size here while 16 KB arrays and one 64 MB array read true.
    val kept    = Array.fill(4096)(new Array[Byte](16 << 10))     // 64 MB, still reachable
    (1 to 4096).foreach(_ => new Array[Byte](16 << 10))           // 64 MB, garbage at once
    val grown   = LiveHeap.bytes() - before
    kept.length shouldBe 4096
    (grown >> 20) should (be >= 63L and be <= 66L)
  }

  "the live heap's classes" should "name what a structure keeps, largest first" in {
    val before = LiveHeap.classes()
    val kept   = Array.fill(20000)(LiveHeapSpec.Held(Array.fill(8)(0L)))   // ~2.1 MB of Held + its arrays
    val grown  = LiveHeap.grown(before, LiveHeap.classes())
    kept.length shouldBe 20000
    val held = grown.find(_.name.endsWith("LiveHeapSpec$Held")).getOrElse(fail(s"Held not among ${grown.take(10)}"))
    held.instances shouldBe 20000L +- 50L
    grown.map(_.bytes) shouldBe grown.map(_.bytes).sortBy(bytes => -bytes)
  }
}

object LiveHeapSpec {
  final case class Held(values: Array[Long])
}
