package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.sys.process.Process

/**
 * The flags a tier's JVM actually runs with, read back from a real JVM.
 *
 * This spec starts a JVM (the one running the tests -- ProcessConfiguration's java.home, which CI
 * pins to the image's JDK) with a tier's production JAVA_OPTS followed by its launcher options
 * ([[LauncherOptions]]), exactly as the start script combines the overlay with
 * conf/application.ini, and reads back each flag's final value and origin. A later JDK that renames
 * or reinterprets one of these flags fails it, which is the point: a behaviour change the next
 * upgrade must look at, not inherit.
 *
 * Only CPU-independent values are asserted. The flags JDK 27 changed from 25 and the apps take as 27
 * ships them (Jdk27Own) are asserted by origin instead: no option names them, whatever this CPU's
 * value. (Until 2026-10-23 infra/jvm/jdk25-parity.options held 27 to 25's values; each line was
 * measured and given back -- docs/proposals/2026-10-23-dependency-and-flag-audit.md -- and the file
 * retired.)
 */
class LauncherJvmFlagsSpec extends AnyFlatSpec with Matchers {

  /** A tier's production JAVA_OPTS, with the paths only the pod has pointed somewhere harmless. */
  private def productionOptions(manifest: String): Seq[String] =
    """JAVA_OPTS: >-\n\s+(.+)""".r.findFirstMatchIn(RepoFile.read(manifest))
      .getOrElse(fail(s"$manifest has no JAVA_OPTS"))
      .group(1).trim.split("\\s+").toSeq
      .map(_.replaceAll("^(-XX:(?:HeapDumpPath|ErrorFile))=.*", "$1=" + tmp.resolve("x")))

  private lazy val tmp = java.nio.file.Files.createTempDirectory("launcher-jvm-flags")

  private lazy val configuration = settings.ProcessConfiguration.resolve()

  /** Both streams: -XX:+PrintFlagsFinal writes to stdout, -XshowSettings to stderr. */
  private def run(args: Seq[String]): String = {
    val out  = new StringBuilder
    val line = (l: String) => out.synchronized { out.append(l).append('\n'); () }
    val exit = Process(configuration.javaHome.binary("java").toString +: args).!(scala.sys.process.ProcessLogger(line, line))
    withClue(s"java ${args.mkString(" ")}:\n$out") { exit.shouldBe(0) }
    out.toString
  }

  /** Each flag's final value and where it came from (`default`, `ergonomic`, `command line`). */
  private def flagsWithOrigin(args: Seq[String]): Map[String, (String, String)] =
    run(args ++ Seq("-XX:+UnlockDiagnosticVMOptions", "-XX:+PrintFlagsFinal", "-version")).linesIterator
      .flatMap(FlagLine.findFirstMatchIn)
      .map(m => m.group(1) -> (m.group(2), m.group(3)))
      .toMap

  private def finalFlags(args: Seq[String]): Map[String, String] = flagsWithOrigin(args).view.mapValues(_._1).toMap

  /** `     bool UseCompactObjectHeaders   = false   {product} {command line}` */
  private val FlagLine = """^\s*\S+\s+(\w+)\s+:?=\s+(\S*)\s+\{[^}]*\}\s+\{([^}]*)\}""".r

  /** JDK 27 behaviour given back on purpose (2026-10-23): no launcher or overlay names these, so the
   *  JVM runs its own value -- 27's locking spin count, its C2 loop and vectorization optimizations,
   *  its ML-DSA intrinsics and its own CPU's allocation-prefetch ergonomics. Measured on the worker's
   *  fixture boot (docs/proposals/2026-10-23-dependency-and-flag-audit.md). */
  private val Jdk27Own = Seq(
    "FastLockingSpins", "UseDilithiumIntrinsics", "ShortRunningLongLoop", "UseAutoVectorizationPredicate",
    "UseAutoVectorizationSpeculativeAliasingChecks", "InitialRAMPercentage", "AllocatePrefetchDistance",
    "AllocatePrefetchLines",
  )

  /** The web tier's G1 heap-sizing ergonomics, held at what JDK 25.0.4.1 ran with (web.options). */
  private val WebG1 = Map(
    "UseCondCardMark"  -> "false",
    "GCTimeRatio"      -> "12",
    "MinHeapFreeRatio" -> "40",
    "MaxHeapFreeRatio" -> "70",
  )

  private val tiers = Seq(
    ("web",    "infra/kubernetes/web/base/all.yaml",             WebG1),
    ("worker", "infra/kubernetes/worker/overlays/pl/patch.yaml", Map.empty[String, String]),
  )

  "build.sbt" should "bake launcher options into exactly the tiers this spec starts" in {
    LauncherOptions.files.keySet shouldBe tiers.map(_._1).toSet
  }

  "infra/jvm" should "hold no JDK 25 parity file: each tier's launcher options are its own" in {
    Seq("jdk25-parity.options", "jdk25-parity-g1.options").filter(name => RepoFile.exists(s"infra/jvm/$name")) shouldBe empty
  }

  tiers.foreach { case (tier, manifest, expected) =>
    def tierFlags = productionOptions(manifest) ++ LauncherOptions.of(tier)

    if (expected.nonEmpty) s"the $tier JVM" should "hold G1's heap-sizing ergonomics at JDK 25's values" in {
      val flags = finalFlags(tierFlags)
      withClue(s"under $tier's JAVA_OPTS + ${LauncherOptions.files(tier).mkString(" + ")}: ") {
        expected.map { case (flag, _) => flag -> flags.getOrElse(flag, "<absent>") } shouldBe expected
      }
    }

    s"the $tier JVM" should "run JDK 27's own locking, C2 loop/vectorization, intrinsic and prefetch values" in {
      val origins = flagsWithOrigin(tierFlags).view.mapValues(_._2).toMap
      Jdk27Own.map(flag => flag -> origins.get(flag).map(_ != "command line")) shouldBe Jdk27Own.map(_ -> Some(true))
    }

    // The JDK 27 default given back on purpose (2026-10-23): JEP 534's compact object headers,
    // 8 bytes off every object header -- heap the identity model's many small objects get back --
    // and the monitor side table they require (27 ignores -XX:-UseObjectMonitorTable beside them).
    // The AOT cache is trained under the same launcher options, so it maps under the same mode.
    it should "run JDK 27's compact object headers" in {
      val flags = finalFlags(tierFlags)
      Seq("UseCompactObjectHeaders", "UseObjectMonitorTable").map(flag => flag -> flags.get(flag)) shouldBe
        Seq("UseCompactObjectHeaders" -> Some("true"), "UseObjectMonitorTable" -> Some("true"))
    }

    // JEP 527, given back on purpose (2026-10-23): 27's own key-exchange groups, the post-quantum hybrid
    // X25519MLKEM768 first. Every outbound host the sources name handshook under it (103 of 349 chose the
    // hybrid, the rest x25519 or a TLS 1.2 curve, none failed); see the dependency-and-flag audit.
    it should "offer JDK 27's TLS key-exchange groups, the post-quantum hybrid in front" in {
      val tls = run(tierFlags ++ Seq("-XshowSettings:security:tls", "-version"))
      val groups = tls.linesIterator.dropWhile(!_.contains("Enabled Named Groups")).drop(1)
        .takeWhile(_.startsWith("        ")).map(_.trim).toSeq
      groups shouldBe Seq("X25519MLKEM768", "x25519", "secp256r1", "secp384r1", "secp521r1", "x448",
                          "ffdhe2048", "ffdhe3072", "ffdhe4096")
    }
  }

  // The worker's collector is the launcher's, whatever an overlay names: no country's JAVA_OPTS names one since
  // 2026-10-24 (they all carried -XX:+UseSerialGC before), and the launcher's options come after them, so one an
  // overlay brought back would be switched off for the Parallel collector. Two collectors left on refuse to start the
  // JVM ("Multiple garbage collectors selected"), so this starts one under each country's real options. SerialGC paused each worker for every young and every full
  // collection on ONE thread: a full one took 3.1 s on worker-de and 3.5 s on worker-us over their ~420-450 MB live sets.
  /** The worker's parallel GC threads: see infra/jvm/worker.options for the measurement behind it. */
  private val WorkerParallelGcThreads = 4

  private val WorkerOverlays = Seq("pl", "uk", "de", "es", "us").map(cc => cc -> s"infra/kubernetes/worker/overlays/$cc/patch.yaml")

  WorkerOverlays.foreach { case (cc, manifest) =>
    s"the worker-$cc JVM" should "run the Parallel collector on its launcher's threads, not SerialGC" in {
      val flags = finalFlags(productionOptions(manifest) ++ LauncherOptions.of("worker"))
      Seq("UseParallelGC", "UseSerialGC", "UseG1GC", "ParallelGCThreads").map(flag => flag -> flags.get(flag)) shouldBe
        Seq("UseParallelGC" -> Some("true"), "UseSerialGC" -> Some("false"), "UseG1GC" -> Some("false"),
            "ParallelGCThreads" -> Some(WorkerParallelGcThreads.toString))
    }

    it should "leave the collector to the launcher rather than name one in its overlay" in {
      productionOptions(manifest).filter(_.matches("-XX:[+-]Use\\w*GC")) shouldBe empty
    }
  }

  // Does the worker's options file leave the flag set once the JVM has read it. Without a pre-touch the
  // heap is committed at boot and paged in only on first use, so worker-pl's RSS stepped +120 MB minutes
  // into a boot (2026-10-07 22:06, 10-12 01:22: the off-book gap -45 -> +76 MB) and that bounded step
  // read as a climb to JvmResidentNearContainerLimit's predict_linear.
  "the worker JVM" should "page its whole heap in at boot, so its RSS does not step up when the heap is first used" in {
    finalFlags(LauncherOptions.of("worker")).get("AlwaysPreTouch") shouldBe Some("true")
  }
}
