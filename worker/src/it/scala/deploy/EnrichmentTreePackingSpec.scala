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
class EnrichmentTreePackingSpec extends AnyFlatSpec with Matchers with tools.SuiteConfiguration {

  private val Packer = ".github/scripts/pack-enrichment-tree.sh"

  /** Runs the real script over `tree`, returning its exit status and combined output —
   *  with `bin` ahead of the PATH when given, to stand a tool in for the runner's. */
  private def pack(tree: Path, archive: Path, bin: Option[Path] = None): (Int, String) = {
    val out    = new StringBuilder
    val logger = ProcessLogger(line => out.append(line).append('\n'))
    val search = configuration.executableSearchPath.value.mkString(java.io.File.pathSeparator)
    val path   = bin.fold(search)(dir => s"$dir:$search")
    val status = Process(Seq("bash", Packer, tree.toString, archive.toString), None, "PATH" -> path).!(logger)
    (status, out.toString)
  }

  private def tempTree(): Path = Files.createTempDirectory("enrichment-tree")

  private def write(file: Path, content: String): Unit = {
    Files.createDirectories(file.getParent)
    Files.writeString(file, content)
    ()
  }

  /** tar's own detection, as `-tf` reads either compressor. */
  private def listing(archive: Path): Vector[String] =
    Seq("tar", "-tf", archive.toString).!!.linesIterator.toVector

  private val ZstdMagic = "28b52ffd"
  private def magic(file: Path): String =
    Files.readAllBytes(file).take(4).map(b => f"${b & 0xff}%02x").mkString

  private def unpack(archive: Path, into: Path): Int =
    Process(Seq("bash", ".github/scripts/unpack-fixture-archive.sh", archive.toString, into.toString), None,
      "PATH" -> configuration.executableSearchPath.value.mkString(java.io.File.pathSeparator)).!(ProcessLogger(_ => ()))

  "the packer" should "carry the remembered-answer cache into the archive with the recorded responses" in {
    // The cache is dot-prefixed and lives inside the tree; if a change to the tar
    // ever dropped hidden paths the loss would be invisible — every leg would simply
    // get slower and still pass.
    val tree = tempTree()
    write(tree.resolve("responses/tmdb-search.json"), """{"results":[]}""")
    write(tree.resolve(".enrichment-cache/metacritic-dune.entry"), "hit")
    write(tree.resolve(".enrichment-cache/rt-dune.entry"), "miss")
    val archive = tree.resolveSibling("enrichment-es.tar.zst")

    val (status, out) = pack(tree, archive)

    withClue(s"the packer failed on a healthy tree:\n$out")(status shouldBe 0)
    out should include("remembered enrichment answers: 2")
    out should include("remembered answers inside the archive: 2")
    listing(archive).count(_.endsWith(".entry")) shouldBe 2
  }

  it should "pack with zstd, which halves the archive every leg uploads and downloads" in {
    // Measured on the UK tree (2.2 GB, 95k files, 4 threads): `pigz -6` 9.4 s → 410 MB,
    // `zstd -3` 0.9 s → 206 MB. The archive is uploaded twice per recording (working asset +
    // pinned copy, ~20 s each, run 37071880312) and downloaded by every leg replaying it.
    val tree = tempTree()
    write(tree.resolve("responses/tmdb-search.json"), """{"results":[]}""")
    write(tree.resolve(".enrichment-cache/metacritic-dune.entry"), "hit")
    val archive = tree.resolveSibling("enrichment-us.tar.zst")

    val (status, out) = pack(tree, archive)

    withClue(s"the packer failed:\n$out")(status shouldBe 0)
    magic(archive) shouldBe ZstdMagic
    out should include("remembered answers inside the archive: 1")
    listing(archive).count(_.endsWith(".entry")) shouldBe 1
  }

  "the unpacker" should "restore a zstd tree and a gzip one alike, by the archive's magic" in {
    // The release still holds gzip trees pinned before the move to zstd, and the scrape
    // corpus is gzip — so readers must not assume either from a name or a `tar -z`.
    val tree = tempTree()
    write(tree.resolve("enrichment-uk/responses/a.json"), "zstd-packed")
    val zst = tree.resolveSibling(s"${tree.getFileName}-enrichment-uk.tar.zst")
    pack(tree.resolve("enrichment-uk"), zst)._1 shouldBe 0
    val gz = tree.resolveSibling(s"${tree.getFileName}-legacy.tar.gz")
    Process(Seq("tar", "-czf", gz.toString, "-C", tree.toString, "enrichment-uk")).! shouldBe 0

    Seq(zst -> "zstd", gz -> "gzip").foreach { case (archive, kind) =>
      val into = Files.createTempDirectory("unpacked")
      withClue(s"the $kind archive did not unpack: ")(unpack(archive, into) shouldBe 0)
      val restored = Files.walk(into).filter(_.toString.endsWith("responses/a.json")).findFirst()
      withClue(s"the $kind archive's file is missing: ")(restored.isPresent shouldBe true)
      Files.readString(restored.get) shouldBe "zstd-packed"
    }
  }

  it should "refuse a file that is neither, rather than unpacking nothing and passing" in {
    val junk = Files.createTempFile("not-an-archive", ".tar.zst")
    Files.writeString(junk, "<html>404</html>")
    unpack(junk, Files.createTempDirectory("unpacked")) should not be 0
  }

  it should "publish a leg that recorded nothing rather than calling its empty cache a loss" in {
    // THE SPAIN CASE. `FileEnrichmentCacheStore` creates its directory when it is
    // constructed, not when it first remembers something, so a leg that failed before
    // it enriched anything leaves the cache there and empty. Nothing was lost — there
    // was nothing to lose.
    val tree = tempTree()
    Files.createDirectories(tree.resolve(".enrichment-cache"))
    val archive = tree.resolveSibling("enrichment-es.tar.zst")

    val (status, out) = pack(tree, archive)

    withClue(s"an empty cache was reported as missing from the tarball:\n$out")(status shouldBe 0)
    out should include("remembered enrichment answers: 0")
    Files.exists(archive) shouldBe true
  }

  it should "say so and succeed when the country has no tree at all" in {
    // The first run on a newly onboarded country, before anything has been recorded.
    val tree    = tempTree()
    val absent  = tree.resolve("enrichment-xx")
    val archive = tree.resolve("enrichment-xx.tar.zst")

    val (status, out) = pack(absent, archive)

    status shouldBe 0
    out should include("nothing recorded")
    withClue("an archive of a tree that does not exist must not be published: ") {
      Files.exists(archive) shouldBe false
    }
  }
}
