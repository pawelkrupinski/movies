package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The phase log is the only diagnosis a five-hour replay leg gets — it runs on a CI
 * runner nobody can attach a profiler to, and when it dies of heap the JVM exits
 * before ScalaTest writes a report at all. So the heap a phase leaves behind has to
 * be in the line the phase already prints.
 */
class PhaseTimerSpec extends AnyFlatSpec with Matchers {

  private val Gb = 1024L * 1024L * 1024L

  "the phase heap note" should "read in the same units as sbt's own GC warning" in {
    // sbt says "[Heap: 0.27GB free of 8.00GB, max 8.00GB]"; a phase line has to be
    // comparable to it at a glance, so both are binary gigabytes to two places.
    PhaseTimer.heapNote(usedBytes = 7 * Gb + Gb / 2, maxBytes = 8 * Gb) shouldBe ", heap 7.50 of 8.00GB"
  }

  it should "stay readable on a heap that has barely been touched" in {
    PhaseTimer.heapNote(usedBytes = Gb / 100, maxBytes = 4 * Gb) shouldBe ", heap 0.01 of 4.00GB"
  }

  it should "report the live heap alongside the phase it timed" in {
    val note = PhaseTimer.heapNote()

    note should startWith (", heap ")
    note should endWith ("GB")
    // A running JVM has used something and has a ceiling; the point of the line is
    // that both numbers are real, not that they are any particular value.
    note.stripPrefix(", heap ").stripSuffix("GB").split(" of ").map(_.toDouble) match {
      case Array(used, max) => used should be > 0.0; max should be > used
      case other            => fail(s"unreadable heap note: ${other.mkString(", ")}")
    }
  }
}
