package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the two heap numbers a local `sbt` run depends on, because both were paid
 * for with measurement and neither is guessable from the code.
 *
 * `.jvmopts` is SHARED: `testUnit` runs every module's specs unforked in that JVM
 * and peaked at 4092MB against its 4096MB ceiling, so the shared number cannot come
 * down. The dev server, whose worst measured run (incremental compile of 387 files
 * plus two app boots) was 1577MB, gets a deliberately tighter heap of its own so an
 * OOM anomaly trips early enough to leave a dump instead of dying silently at 4GB.
 *
 * The invariant that matters is the ORDER: dev-server ceiling < canary < shared
 * heap. Collapse it in either direction and one of the two purposes breaks — a
 * canary at or above the shared heap never trips, and one at or below the dev
 * server's real high-water mark turns every heavy compile into a false OOM.
 */
class DevServerHeapCanarySpec extends AnyFlatSpec with Matchers {

  private val jvmopts   = RepoFile.read(".jvmopts")
  private val devServer = RepoFile.read("scripts/dev-server.sh")

  /** `-Xmx` in megabytes, accepting the `g`/`m` suffixes the JVM accepts.
   *
   *  Comment lines are stripped first: both files DISCUSS other heap sizes in prose
   *  (`.jvmopts` explains the `-J` escape hatch, the script documents how to raise
   *  it back), and matching one of those instead of the effective flag is exactly
   *  the false green this spec exists to prevent. It caught itself doing that. */
  private def maxHeapMib(text: String): Int = {
    val effective = text.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    val m = """-Xmx(\d+)([gGmM])""".r.findFirstMatchIn(effective)
      .getOrElse(fail(s"no -Xmx outside comments in:\n$text"))
    val n = m.group(1).toInt
    if (m.group(2).equalsIgnoreCase("g")) n * 1024 else n
  }

  // The measured facts this spec exists to defend (2026-09-04, this machine).
  private val TestUnitPeakMib  = 4092
  private val DevServerPeakMib = 1577

  ".jvmopts" should "keep the shared heap at or above testUnit's measured peak" in {
    // testUnit runs unforked in this JVM; below its peak the run dies partway and
    // still exits 0, which is the silent-partial-green the file's comment warns of.
    maxHeapMib(jvmopts) should be >= TestUnitPeakMib
  }

  it should "dump the heap on OOM, so a death names its retainer" in {
    // The 2026-09-04 dev-server OOM cost a long investigation purely because
    // ExitOnOutOfMemoryError prints one line and leaves nothing behind.
    jvmopts should include("-XX:+HeapDumpOnOutOfMemoryError")
    jvmopts should include("-XX:HeapDumpPath=")
    // `%p` — a bare directory gives every dump one constant name and only the
    // first is ever written.
    jvmopts should include("%p")
  }

  "scripts/dev-server.sh" should "narrow the heap for the dev server only" in {
    // `-J` is appended after `.jvmopts` and the JVM honours the last `-Xmx`, so
    // this must be a `-J` flag; editing `.jvmopts` instead would hit testUnit.
    devServer should include("-J-Xmx")
    devServer should include("web/run")
  }

  it should "sit below the shared heap, or the canary can never trip" in {
    maxHeapMib(devServer) should be < maxHeapMib(jvmopts)
  }

  it should "stay clear of the dev server's own measured high-water mark" in {
    // Worst measured legitimate run was 1577MB; a canary at or under that would
    // turn a heavy incremental compile into a false OOM. Keep real headroom.
    withClue(s"canary must leave headroom over the ${DevServerPeakMib}MiB worst measured run: ")(
      maxHeapMib(devServer) should be > (DevServerPeakMib * 3 / 2))
  }
}
