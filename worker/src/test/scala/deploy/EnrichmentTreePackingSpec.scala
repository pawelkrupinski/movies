package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.sys.process.*

/**
 * The convergence publish, run for real against fabricated trees.
 *
 * What a leg hands the next one is this archive, and the step that builds it runs
 * under `always()` on a job that has usually just failed — so its own failures land
 * on top of a red leg and read as part of it. Spain's first convergence leg failed
 * for want of a corpus on 2026-09-02 and then failed a SECOND time here, on a guard
 * that read an empty `.enrichment-cache/` as "the cache is missing from the tarball".
 * Two errors, one of them fictional, in front of the one that mattered.
 *
 * `ConvergenceLegWiringSpec` asserts the publish is wired into both jobs and that
 * tar's warning status doesn't discard the capture; this spec runs the script.
 */
class EnrichmentTreePackingSpec extends AnyFlatSpec with Matchers {

  private val Packer = ".github/scripts/pack-enrichment-tree.sh"

  /** Runs the real script over `tree`, returning its exit status and combined output. */
  private def pack(tree: Path, archive: Path): (Int, String) = {
    val out    = new StringBuilder
    val logger = ProcessLogger(line => out.append(line).append('\n'))
    val status = Seq("bash", Packer, tree.toString, archive.toString).!(logger)
    (status, out.toString)
  }

  private def tempTree(): Path = Files.createTempDirectory("enrichment-tree")

  private def write(file: Path, content: String): Unit = {
    Files.createDirectories(file.getParent)
    Files.writeString(file, content)
    ()
  }

  private def listing(archive: Path): Vector[String] =
    Seq("tar", "-tzf", archive.toString).!!.linesIterator.toVector

  "the packer" should "carry the remembered-answer cache into the archive with the recorded responses" in {
    // The cache is dot-prefixed and lives inside the tree; if a change to the tar
    // ever dropped hidden paths the loss would be invisible — every leg would simply
    // get slower and still pass.
    val tree = tempTree()
    write(tree.resolve("responses/tmdb-search.json"), """{"results":[]}""")
    write(tree.resolve(".enrichment-cache/metacritic-dune.entry"), "hit")
    write(tree.resolve(".enrichment-cache/rt-dune.entry"), "miss")
    val archive = tree.resolveSibling("enrichment-es.tar.gz")

    val (status, out) = pack(tree, archive)

    withClue(s"the packer failed on a healthy tree:\n$out")(status shouldBe 0)
    out should include("remembered enrichment answers: 2")
    out should include("remembered answers inside the archive: 2")
    listing(archive).count(_.endsWith(".entry")) shouldBe 2
  }

  it should "publish a leg that recorded nothing rather than calling its empty cache a loss" in {
    // THE SPAIN CASE. `FileEnrichmentCacheStore` creates its directory when it is
    // constructed, not when it first remembers something, so a leg that failed before
    // it enriched anything leaves the cache there and empty. Nothing was lost — there
    // was nothing to lose.
    val tree = tempTree()
    Files.createDirectories(tree.resolve(".enrichment-cache"))
    val archive = tree.resolveSibling("enrichment-es.tar.gz")

    val (status, out) = pack(tree, archive)

    withClue(s"an empty cache was reported as missing from the tarball:\n$out")(status shouldBe 0)
    out should include("remembered enrichment answers: 0")
    Files.exists(archive) shouldBe true
  }

  it should "say so and succeed when the country has no tree at all" in {
    // The first run on a newly onboarded country, before anything has been recorded.
    val tree    = tempTree()
    val absent  = tree.resolve("enrichment-xx")
    val archive = tree.resolve("enrichment-xx.tar.gz")

    val (status, out) = pack(absent, archive)

    status shouldBe 0
    out should include("nothing recorded")
    withClue("an archive of a tree that does not exist must not be published: ") {
      Files.exists(archive) shouldBe false
    }
  }
}
