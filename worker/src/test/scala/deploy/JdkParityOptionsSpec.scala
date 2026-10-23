package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.sys.process.Process

/**
 * Holds the JVM the apps run on to what JDK 25 ran them with.
 *
 * infra/jvm/jdk25-parity*.options undo, one line each, the differences a flag-by-flag diff of
 * Temurin 25.0.4.1 against 27 found under the production JAVA_OPTS. This spec starts a real JVM
 * (the one running the tests -- ProcessConfiguration's java.home, which CI pins to the image's JDK) with a tier's production
 * JAVA_OPTS followed by its parity options, exactly as the start script combines
 * conf/application.ini with the overlay, and reads back the final values. Without the options,
 * JDK 27 fails every row below; a later JDK that renames or reinterprets one fails it too, which
 * is the point -- that is a behaviour change the next upgrade must look at, not inherit.
 *
 * Only CPU-independent values are asserted. The flags 27 changed and the apps take as 27 ships them
 * (Jdk27Own) are asserted by origin instead: no option names them, whatever this CPU's value.
 */
class JdkParityOptionsSpec extends AnyFlatSpec with Matchers {

  private def options(name: String): Seq[String] =
    RepoFile.read(s"infra/jvm/$name").linesIterator.map(_.trim)
      .filterNot(line => line.isEmpty || line.startsWith("#")).toSeq

  /** A tier's production JAVA_OPTS, with the paths only the pod has pointed somewhere harmless. */
  private def productionOptions(manifest: String): Seq[String] =
    """JAVA_OPTS: >-\n\s+(.+)""".r.findFirstMatchIn(RepoFile.read(manifest))
      .getOrElse(fail(s"$manifest has no JAVA_OPTS"))
      .group(1).trim.split("\\s+").toSeq
      .map(_.replaceAll("^(-XX:(?:HeapDumpPath|ErrorFile))=.*", "$1=" + tmp.resolve("x")))

  private lazy val tmp = java.nio.file.Files.createTempDirectory("jdk-parity")

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

  /** What JDK 25.0.4.1 ran with, the G1 ergonomics of the web tier: the one tier still held there. */
  private val Jdk25G1 = Map(
    "UseCondCardMark"  -> "false",
    "GCTimeRatio"      -> "12",
    "MinHeapFreeRatio" -> "40",
    "MaxHeapFreeRatio" -> "70",
  )

  private val tiers = Seq(
    ("web",    "infra/kubernetes/web/base/all.yaml",             Seq("jdk25-parity.options", "jdk25-parity-g1.options", "web.options"), Jdk25G1),
    ("worker", "infra/kubernetes/worker/overlays/pl/patch.yaml", Seq("jdk25-parity.options", "worker.options"),          Map.empty[String, String]),
  )

  tiers.foreach { case (tier, manifest, files, expected) =>
    if (expected.nonEmpty) s"the $tier JVM" should "run every flag JDK 27 changed at JDK 25's value" in {
      val flags = finalFlags(productionOptions(manifest) ++ files.flatMap(options))
      withClue(s"under $tier's JAVA_OPTS + ${files.mkString(" + ")}: ") {
        expected.map { case (flag, _) => flag -> flags.getOrElse(flag, "<absent>") } shouldBe expected
      }
    }

    s"the $tier JVM" should "run JDK 27's own locking, C2 loop/vectorization, intrinsic and prefetch values" in {
      val origins = flagsWithOrigin(productionOptions(manifest) ++ files.flatMap(options)).view.mapValues(_._2).toMap
      Jdk27Own.map(flag => flag -> origins.get(flag).map(_ != "command line")) shouldBe Jdk27Own.map(_ -> Some(true))
    }

    // The JDK 27 default given back on purpose (2026-10-23): JEP 534's compact object headers,
    // 8 bytes off every object header -- heap the identity model's many small objects get back --
    // and the monitor side table they require (27 ignores -XX:-UseObjectMonitorTable beside them).
    // The AOT cache is trained under the same launcher options, so it maps under the same mode.
    it should "run JDK 27's compact object headers" in {
      val flags = finalFlags(productionOptions(manifest) ++ files.flatMap(options))
      Seq("UseCompactObjectHeaders", "UseObjectMonitorTable").map(flag => flag -> flags.get(flag)) shouldBe
        Seq("UseCompactObjectHeaders" -> Some("true"), "UseObjectMonitorTable" -> Some("true"))
    }
  }

  // Not a JDK 25 parity row, but the same question asked of the same launcher: does the worker's
  // options file leave the flag set once the JVM has read it. Without a pre-touch the heap is
  // committed at boot and paged in only on first use, so worker-pl's RSS stepped +120 MB minutes into
  // a boot (2026-10-07 22:06, 10-12 01:22: the off-book gap -45 -> +76 MB) and that bounded step read
  // as a climb to JvmResidentNearContainerLimit's predict_linear.
  "the worker JVM" should "page its whole heap in at boot, so its RSS does not step up when the heap is first used" in {
    finalFlags(Seq("jdk25-parity.options", "worker.options").flatMap(options)).get("AlwaysPreTouch") shouldBe Some("true")
  }

  "the TLS client" should "offer JDK 25's key-exchange groups, without the post-quantum hybrid in front" in {
    val tls = run(options("jdk25-parity.options") ++ Seq("-XshowSettings:security:tls", "-version"))
    val groups = tls.linesIterator.dropWhile(!_.contains("Enabled Named Groups")).drop(1)
      .takeWhile(_.startsWith("        ")).map(_.trim).toSeq
    groups shouldBe Seq("x25519", "secp256r1", "secp384r1", "secp521r1", "x448",
                        "ffdhe2048", "ffdhe3072", "ffdhe4096", "ffdhe6144", "ffdhe8192")
  }
}
