package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LiveHeapSpec extends AnyFlatSpec with Matchers {

  // Counted by classes only this spec allocates: `itAll` runs suites in parallel in one JVM, so a
  // whole-heap delta moves with whatever the other suites allocate or free meanwhile (it failed
  // there with the 63–66 MB window this read before, and passed alone).
  "the live heap" should "count what a structure keeps, and none of the garbage it left behind" in {
    val kept = Array.fill(4096)(LiveHeapSpec.Kept(new Array[Byte](1024)))
    (1 to 4096).foreach(_ => LiveHeapSpec.Garbage(new Array[Byte](1024)))
    val live = LiveHeap.classes()
    kept.length shouldBe 4096
    live.get(classOf[LiveHeapSpec.Kept].getName).map(_.instances) shouldBe Some(4096L)
    live.get(classOf[LiveHeapSpec.Garbage].getName).map(_.instances).getOrElse(0L) shouldBe 0L
    LiveHeap.bytes() should be >= 4096L * 1024
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
  final case class Kept(payload: Array[Byte])
  final case class Garbage(payload: Array[Byte])
}
