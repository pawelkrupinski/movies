package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A dump's file name is the only thing the node's heap-dump timer sees, and its report
 * (infra/nix/files/heap-dumps.sh) leaves `requested-*` out of the newest-death time that
 * HeapDumpWritten alerts on. So the name must say WHY the dump was taken: a watchdog
 * catching a wedged JVM is a death, a POST /heapdump is somebody looking.
 */
class HeapDumperSpec extends AnyFlatSpec with Matchers {

  "HeapDumper.fileName" should "name the liveness watchdog's dump as a wedge" in {
    HeapDumper.fileName(HeapDumper.Wedged, 1790687125837L) shouldBe "wedge-1790687125837.hprof"
  }

  it should "name an on-demand dump as requested, the prefix the node's report excludes" in {
    HeapDumper.fileName(HeapDumper.Requested, 1790703766304L) shouldBe "requested-1790703766304.hprof"
  }
}
