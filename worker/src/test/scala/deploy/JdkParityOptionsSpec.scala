package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.sys.process.Process

/**
 * Holds the JVM the apps run on to what JDK 25 ran them with.
 *
 * infra/jvm/jdk25-parity*.options undo, one line each, the differences a flag-by-flag diff of
 * Temurin 25.0.4.1 against 27 found under the production JAVA_OPTS. This spec starts a real JVM
 * (the one running the tests, which CI pins to the image's JDK) with a tier's production
 * JAVA_OPTS followed by its parity options, exactly as the start script combines
 * conf/application.ini with the overlay, and reads back the final values. Without the options,
 * JDK 27 fails every row below; a later JDK that renames or reinterprets one fails it too, which
 * is the point -- that is a behaviour change the next upgrade must look at, not inherit.
 *
 * Only CPU-independent values are asserted: the prefetch ergonomics differ between CI's runners
 * and k3s-worker-1, and the options file pins those to what 25 chose on the node.
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
      .map(_.replaceAll("^(-XX:(?:HeapDumpPath|ErrorFile|SharedArchiveFile))=.*", "$1=" + tmp.resolve("x")))
      .filterNot(_ == "-XX:+AutoCreateSharedArchive")

  private lazy val tmp = java.nio.file.Files.createTempDirectory("jdk-parity")

  /** Both streams: -XX:+PrintFlagsFinal writes to stdout, -XshowSettings to stderr. */
  private def run(args: Seq[String]): String = {
    val out  = new StringBuilder
    val line = (l: String) => out.synchronized { out.append(l).append('\n'); () }
    val exit = Process(s"${System.getProperty("java.home")}/bin/java" +: args).!(scala.sys.process.ProcessLogger(line, line))
    withClue(s"java ${args.mkString(" ")}:\n$out") { exit.shouldBe(0) }
    out.toString
  }

  private def finalFlags(args: Seq[String]): Map[String, String] =
    run(args ++ Seq("-XX:+UnlockDiagnosticVMOptions", "-XX:+PrintFlagsFinal", "-version")).linesIterator
      .flatMap(FlagLine.findFirstMatchIn)
      .map(m => m.group(1) -> m.group(2))
      .toMap

  /** `     bool UseCompactObjectHeaders   = false   {product} {command line}` */
  private val FlagLine = """^\s*\S+\s+(\w+)\s+:?=\s+(\S*)\s+\{""".r

  /** What JDK 25.0.4.1 ran with, for every tier, under these JAVA_OPTS. */
  private val Jdk25Common = Map(
    "UseCompactObjectHeaders"                      -> "false",
    "UseObjectMonitorTable"                        -> "false",
    "FastLockingSpins"                             -> "13",
    "UseDilithiumIntrinsics"                       -> "false",
    "ShortRunningLongLoop"                         -> "false",
    "UseAutoVectorizationPredicate"                -> "false",
    "UseAutoVectorizationSpeculativeAliasingChecks" -> "false",
    "InitialRAMPercentage"                         -> "1.562500",
  )

  /** And the G1 ergonomics that apply to the web tier only. */
  private val Jdk25G1 = Map(
    "UseCondCardMark"  -> "false",
    "GCTimeRatio"      -> "12",
    "MinHeapFreeRatio" -> "40",
    "MaxHeapFreeRatio" -> "70",
  )

  private val tiers = Seq(
    ("web",    "infra/kubernetes/web/base/all.yaml",             Seq("jdk25-parity.options", "jdk25-parity-g1.options"), Jdk25Common ++ Jdk25G1),
    ("worker", "infra/kubernetes/worker/overlays/pl/patch.yaml", Seq("jdk25-parity.options"),                            Jdk25Common),
  )

  tiers.foreach { case (tier, manifest, files, expected) =>
    s"the $tier JVM" should "run every flag JDK 27 changed at JDK 25's value" in {
      val flags = finalFlags(productionOptions(manifest) ++ files.flatMap(options))
      withClue(s"under $tier's JAVA_OPTS + ${files.mkString(" + ")}: ") {
        expected.map { case (flag, _) => flag -> flags.getOrElse(flag, "<absent>") } shouldBe expected
      }
    }
  }

  "the TLS client" should "offer JDK 25's key-exchange groups, without the post-quantum hybrid in front" in {
    val tls = run(options("jdk25-parity.options") ++ Seq("-XshowSettings:security:tls", "-version"))
    val groups = tls.linesIterator.dropWhile(!_.contains("Enabled Named Groups")).drop(1)
      .takeWhile(_.startsWith("        ")).map(_.trim).toSeq
    groups shouldBe Seq("x25519", "secp256r1", "secp384r1", "secp521r1", "x448",
                        "ffdhe2048", "ffdhe3072", "ffdhe4096", "ffdhe6144", "ffdhe8192")
  }
}
