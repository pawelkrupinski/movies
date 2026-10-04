package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

/**
 * Heap allocation is measured in ONE way: production through `tools.ThreadAllocation`, specs through
 * `tools.costs.AllocationMeter` (warmed, median of several runs) against a budget in
 * `tools.costs.PerformanceBudgets` — never by reading `ThreadMXBean`'s counters by hand.
 *
 * The hand-rolled shape was copied into eight specs and two production classes, each warming (or not)
 * its own way, each with its bound written inline: a literal `64L * 1024` in one spec, `4L * 1024 * 1024`
 * in another, none of them findable from the others, and every render and projection regression of
 * October 2026 found by luck rather than by one of them. A counter read anywhere but the two readers
 * fails here.
 *
 * Every allow-list entry says why; the list may only shrink, and an entry naming a file that no longer
 * reads the counter fails too.
 */
class NoHandRolledAllocationMeasurementSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{MainRoots, TestRoots, codeOf, scalaFiles}

  private val CounterRead =
    """\b(getThreadAllocatedBytes|getCurrentThreadAllocatedBytes|getTotalThreadAllocatedBytes)\b""".r

  /** file → why it may read the counter itself. */
  private val Allowed: Map[String, String] = Map(
    "common/src/main/scala/tools/ThreadAllocation.scala" ->
      "the one reader: every production metric and AllocationMeter measure through it",
    "worker/src/test/scala/scripts/CorpusCensusBench.scala" ->
      "a hand-run bench of the WHOLE process (getTotalThreadAllocatedBytes, every pool's threads), which no per-thread meter can say",
    "worker/src/test/scala/scripts/IdentityListingsBench.scala" ->
      "a hand-run bench of the WHOLE process (getTotalThreadAllocatedBytes): the intake's reads decode on the driver's threads",
    "worker/src/test/scala/tools/NoHandRolledAllocationMeasurementSpec.scala" ->
      "this lint: its own self-test spells the shapes it bans",
  )

  private def offenders: Seq[String] = for {
    file <- scalaFiles(MainRoots ++ TestRoots)
    if CounterRead.findFirstIn(codeOf(file)).isDefined
  } yield file.toString

  "allocation" should "be read only through ThreadAllocation and AllocationMeter" in {
    (MainRoots ++ TestRoots).foreach(root => withClue(s"$root must exist (run from the repo root)")(java.nio.file.Files.isDirectory(Paths.get(root)) shouldBe true))
    val unexplained = offenders.filterNot(Allowed.contains)
    withClue("measure a spec's allocation with tools.costs.AllocationMeter against a tools.costs.PerformanceBudgets budget, " +
      "and production's with tools.ThreadAllocation:\n" + unexplained.mkString("\n") + "\n")(unexplained shouldBe empty)
  }

  it should "name no allow-listed file that no longer reads the counter" in {
    Allowed.keySet.toSeq.sorted.filterNot(offenders.contains) shouldBe empty
  }

  "the lint" should "see a counter read written the way the old specs wrote it" in {
    CounterRead.findFirstIn("val before = threads.getThreadAllocatedBytes(id)") shouldBe defined
    CounterRead.findFirstIn("threads.getCurrentThreadAllocatedBytes - b") shouldBe defined
    CounterRead.findFirstIn("tools.ThreadAllocation.of(block)") shouldBe empty
  }
}
