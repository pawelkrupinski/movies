package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/** Every pod's JAVA_OPTS must let the image's AOT cache map (Dockerfile, `tools.ClassArchiveTraining`).
 *  The cache is trained through the launcher under the options baked into it (infra/jvm/<tier>.options),
 *  and the overlay's JAVA_OPTS come AFTER those, so an overlay naming another collector or
 *  type-speculation setting would win at run time and the cache would not map — silently: every
 *  class back in metaspace, where worker-pl died at its 128m cap. A CDS archive flag is worse: the
 *  JVM refuses to start beside -XX:AOTCache, which is why the Dockerfile then leaves the cache out. */
class AotCacheOptionsSpec extends AnyFlatSpec with Matchers {

  private val Collector   = """-XX:\+Use\w*GC""".r
  private val Speculation = """-XX:[+-]UseTypeSpeculation""".r
  private val CdsArchive  = """-XX:(SharedArchiveFile=\S*|\+AutoCreateSharedArchive)|-Xshare:\w+""".r

  private def baked(tier: String): Set[String] =
    RepoFile.read(s"infra/jvm/$tier.options").linesIterator.map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#")).toSet

  /** Each manifest under the tier that sets JAVA_OPTS, with them. */
  private def javaOpts(tier: String): Seq[(String, String)] = {
    RepoFile.read(s"infra/kubernetes/$tier/base/all.yaml") // the missing-checkout message, if absent
    Files.walk(Paths.get(s"infra/kubernetes/$tier")).iterator.asScala.toSeq
      .filter(_.toString.endsWith(".yaml")).map(_.toString).sorted
      .flatMap(path => """JAVA_OPTS: >-\n\s+(.+)""".r.findFirstMatchIn(RepoFile.read(path)).map(path -> _.group(1)))
  }

  Seq("web", "worker").foreach { tier =>
    s"the $tier tier's JAVA_OPTS" should "name no collector or type speculation other than its launcher's" in {
      val manifests = javaOpts(tier)
      manifests should not be empty
      manifests.foreach { case (path, opts) =>
        val overridden = (Collector.findAllIn(opts) ++ Speculation.findAllIn(opts)).toSet -- baked(tier)
        withClue(s"$path overrides infra/jvm/$tier.options, which the AOT cache is trained under: ")(overridden shouldBe empty)
      }
    }

    it should "carry no CDS archive flag, beside which the JVM refuses the AOT cache" in {
      javaOpts(tier).foreach { case (path, opts) =>
        withClue(s"$path: ")(CdsArchive.findAllIn(opts).toSeq shouldBe empty)
      }
    }
  }

  /** The options each tier's launcher bakes into conf/application.ini, in its order — build.sbt's
   *  `launcherOptions(...)` calls, which this mirrors. */
  private val LauncherFiles = Map(
    "worker" -> Seq("jdk25-parity.options", "worker.options"),
    "web"    -> Seq("jdk25-parity.options", "jdk25-parity-g1.options", "web.options"))
  private def launcher(tier: String): Seq[String] = LauncherFiles(tier)
    .flatMap(file => RepoFile.read(s"infra/jvm/$file").linesIterator.map(_.trim).filterNot(l => l.isEmpty || l.startsWith("#")))

  /** A relocated archive costs its whole size in private memory: the JVM maps it at a random base
   *  and patches every pointer, dirtying every page. Measured in the worker image under PL's options
   *  over one fixture pipeline run: 741 MB anonymous RSS relocated against 638 MB mapped in place,
   *  the difference moving to clean file-backed pages the kernel can reclaim. */
  LauncherFiles.keys.toSeq.sorted.foreach { tier =>
    s"the $tier launcher" should "map the AOT cache in place rather than relocate it" in {
      launcher(tier) should contain ("-XX:ArchiveRelocationMode=0")
      javaOpts(tier).foreach { case (path, opts) =>
        withClue(s"$path overrides the launcher's archive relocation: ")("""-XX:ArchiveRelocationMode=\d""".r.findAllIn(opts).toSeq shouldBe empty)
      }
    }

    // NIO reads a socket into a heap buffer through a temporary direct one of the same size, and
    // caches it per thread, unbounded: NMT's "Other" held 161 MB of them on worker-uk (~75 buffers of
    // ~2 MB, the Mongo driver's replies) and fell out of the top categories under the cap (09-29).
    it should "cap the temporary direct buffers NIO caches per thread" in {
      launcher(tier) should contain ("-Djdk.nio.maxCachedBufferSize=262144")
      javaOpts(tier).foreach { case (path, opts) =>
        withClue(s"$path overrides the launcher's NIO buffer cap: ")("""-Djdk\.nio\.maxCachedBufferSize=""".r.findAllIn(opts).toSeq shouldBe empty)
      }
    }

    // 57.9 MB of worker-pl's heap was duplicate String contents ("US", "2D", titles, JSON values);
    // deduplicating their byte arrays took 11 MB off the live old generation over a fixture run.
    it should "deduplicate strings the collector keeps" in {
      launcher(tier) should contain ("-XX:+UseStringDeduplication")
    }

    // The identity model boxes its family ids in five maps and sets; kept below the live family
    // count (IncrementalResolver recycles them), they are all the cached Integers of a raised cache
    // instead of a fresh box each (~10 MB on worker-us). The worker's own flag, not web's.
    if (tier == "worker") it should "cache the boxes of the identity model's family ids" in {
      launcher(tier) should contain ("-XX:AutoBoxCacheMax=16384")
    }

    // The flag is diagnostic, and so are several parity flags: a launcher that names one before
    // -XX:+UnlockDiagnosticVMOptions does not start at all. Starting a JVM under exactly the baked
    // options, in their order, is the check a pod would otherwise make.
    it should "start a JVM under exactly the options it bakes, in order" in {
      val java    = ProcessHandle.current().info().command().orElseThrow() // this JVM's own `java`
      val process = new ProcessBuilder((java +: launcher(tier) :+ "-version")*).redirectErrorStream(true).start()
      val output  = new String(process.getInputStream.readAllBytes())
      withClue(s"infra/jvm options for $tier:\n$output")(process.waitFor() shouldBe 0)
    }
  }

  // The worker's cache is trained on a replayed POLISH boot. Its method profiles saved neither JIT nor
  // CPU in production and cost worker-es ~13% boot CPU (+20 s JIT); the archived classes are the win
  // (6–11 MB less non-heap), and they do not need the profiles. Measured 2026-10-02, re-seed boots
  // against re-seed boots.
  "the worker's AOT training run" should "archive classes without recording method profiles" in {
    val script = RepoFile.read("scripts/ci/train-worker-aot.sh")
    val training = script.linesIterator.find(_.contains("-XX:AOTCacheOutput")).getOrElse(fail("no training JAVA_OPTS"))
    training should include("-XX:+UnlockDiagnosticVMOptions -XX:-AOTRecordTraining")
  }

  // Without profiles the replay only has to load what booting loads: 60 s archived 15,834 classes,
  // 240 s 15,879 (2026-10-02) — and every second of it delays every worker deploy.
  it should "replay no longer than the classes need" in {
    val step = RepoFile.read(".github/workflows/main.yml").linesIterator.dropWhile(!_.contains("scripts/ci/train-worker-aot.sh")).take(5).mkString("\n")
    val seconds = """08-06-2026 (\d+)""".r.findFirstMatchIn(step).map(_.group(1).toInt).getOrElse(fail("no training seconds"))
    seconds should be <= 60
  }
}

