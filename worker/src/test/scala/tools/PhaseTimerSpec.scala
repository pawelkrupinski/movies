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

  "a phase line" should "say how much CPU the process spent across the phase, beside its wall time" in {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      PhaseTimer.timed("xx", "busy") {
        val until = System.nanoTime() + 300_000_000L
        var spins = 0L
        while (System.nanoTime() < until) spins += 1
        spins
      }
    }
    val cpu = """, cpu ([0-9.]+)s""".r.findFirstMatchIn(out.toString).map(_.group(1).toDouble)
    withClue(s"no CPU in the phase line: ${out.toString}") { cpu should not be empty }
    cpu.get should be >= 0.2
  }

  "the CPU note" should "read in seconds to one place" in {
    PhaseTimer.cpuNote(12_345_000_000L) shouldBe ", cpu 12.3s"
  }

  "a phase line" should "say how long the collectors ran across the phase, beside its CPU" in {
    val out = new java.io.ByteArrayOutputStream()
    Console.withOut(out) {
      PhaseTimer.timed("xx", "churn") {
        var kept = List.empty[Array[Byte]]
        (1 to 2000).foreach { i => kept = new Array[Byte](256 * 1024) :: kept.take(8); if (i % 500 == 0) System.gc() }
        kept.size
      }
    }
    val gc = """, cpu [0-9.]+s, gc ([0-9.]+)s""".r.findFirstMatchIn(out.toString).map(_.group(1).toDouble)
    withClue(s"no GC time in the phase line: ${out.toString}") { gc should not be empty }
    gc.get should be > 0.0
  }

  "the GC note" should "read in seconds to one place" in {
    PhaseTimer.gcNote(4_560L) shouldBe ", gc 4.6s"
  }
}
