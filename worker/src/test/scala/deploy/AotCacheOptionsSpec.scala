package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/** Every pod's JAVA_OPTS must let the image's AOT cache map (Dockerfile, `tools.ClassArchiveTraining`).
 *  The cache is trained through the launcher under the options baked into it (infra/jvm/<tier>.options),
 *  so the options must stay the launcher's alone: an overlay naming another collector or
 *  type-speculation setting puts the pod under options the cache was not trained under, and a cache
 *  that does not map fails silently: every class back in metaspace, where worker-pl died at its 128m
 *  cap. The launcher's options come AFTER the overlay's JAVA_OPTS on the java command line (a
 *  worker-us JVM's arguments, 2026-10-03), so for a flag both name the launcher's value is the one
 *  that runs — an overlay cannot switch a launcher flag off. A CDS archive flag is worse: the
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

  /** The option files each tier's launcher bakes into conf/application.ini, in their order — read
   *  off build.sbt's `launcherOptions(...)` call in that tier's project, so the spec checks what the
   *  build bakes rather than a copy of it. */
  private val LauncherFiles: Map[String, Seq[String]] = {
    val Project = """(?m)^lazy val (\w+) = \(project""".r
    val Call    = """launcherOptions\(([^)]*)\)""".r
    val build   = RepoFile.read("build.sbt")
    val starts  = Project.findAllMatchIn(build).map(m => m.start -> m.group(1)).toSeq
    Call.findAllMatchIn(build).filterNot(_.group(1).contains("String*")).map { call =>
      val project = starts.filter(_._1 < call.start).last._2
      project -> "\"([^\"]+)\"".r.findAllMatchIn(call.group(1)).map(_.group(1)).toSeq
    }.toMap
  }

  "build.sbt" should "bake launcher options into both deployed tiers" in {
    LauncherFiles.keySet shouldBe Set("worker", "web")
  }

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

    // At the default SweeperThreshold (15% of the 64m code cache) every ~10 MB of new code asked
    // SerialGC for a FULL collection to unload cold code: 7 in a worker boot, then one every ~90 s.
    if (tier == "worker") it should "let the code cache grow by half before a full GC unloads it" in {
      launcher(tier) should contain ("-XX:SweeperThreshold=50")
    }

    // The JIT was half of a worker boot's CPU, much of it compiling code a boot runs a few thousand
    // times and never again.
    if (tier == "worker") it should "compile a method only after three times the default invocations" in {
      launcher(tier) should contain ("-XX:CompileThresholdScaling=3")
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
}
