package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Production code reads this process's configuration in ONE place. `Env` binds its sources
 * to the real process (`Env.fromProcess`: environment variables, system properties,
 * `.env.local`), and `settings.ProcessConfiguration` is the one resolver that reads a key
 * through it and turns it into a type of its own — the country, the Mongo address, every
 * credential, every tuning knob. Everything else is HANDED those typed values through its
 * constructor or parameters, so a spec builds them directly and no code deep in the object
 * graph can quietly pick a different answer than its root did, or a String meant for one
 * setting can be passed as another.
 *
 * Two rules, over every module's `src/main` (Scala and Twirl) and the worker's shared
 * `src/fixtures` harness, with comments stripped:
 *
 *  1. Nothing touches the process directly — no `Env.fromProcess`, `sys.env`, `sys.props`,
 *     `System.getenv` / `getProperty` / `setProperty`.
 *  2. Nothing reads a configuration key through an Env — no `env.get("…")`, `env.flag(…)`,
 *     `env.positiveInt(…)` — outside the resolver.
 *
 * Each allow-list names its exception and why; a new one needs a reason as good.
 */
class ProcessAccessLintSpec extends AnyFlatSpec with Matchers {

  private val ProcessAccess =
    """\bEnv\s*\.\s*fromProcess\b|\bsys\s*\.\s*(props|env)\b|\bSystem\s*\.\s*(getenv|getProperty|getProperties|setProperty|clearProperty|setProperties)\b""".r

  private val KeyRead =
    """\b\w*[eE]nv\w*\s*\.\s*(get|flag|positiveInt|positiveLong|currentValue)\b|\.\s*(get|flag|positiveInt|positiveLong|currentValue)\s*\(\s*"[A-Z][A-Z0-9_]+"""".r

  /** file → why it may touch the process directly. */
  private val ProcessAllowed: Map[String, String] = Map(
    "common/src/main/scala/tools/Env.scala" ->
      "Env's process binding: environment variables, then system properties, then .env.local",
    "common/src/main/scala/settings/ProcessConfiguration.scala" ->
      "the ONE resolver: reads the process through Env.fromProcess and yields the typed values roots pass down",
    "common/src/main/scala/tools/ProxyTunnelAuthentication.scala" ->
      "writes a policy the JDK itself reads (jdk.http.auth.tunneling.disabledSchemes); applied once by each main",
    "common/src/main/scala/tools/IssuerCertificateFetching.scala" ->
      "writes a policy the JDK itself reads (com.sun.security.enableAIAcaIssuers); applied once by each main",
  )

  /** file → why it may read an Env key. */
  private val KeyReadAllowed: Map[String, String] = Map(
    "common/src/main/scala/tools/Env.scala" ->
      "Env itself: the lookup every read goes through",
    "common/src/main/scala/settings/ProcessConfiguration.scala" ->
      "the ONE resolver: every configuration key is read here, into a type of its own",
    "common/src/main/scala/services/config/EnvConfigService.scala" ->
      "the /admin/config page: lists every REGISTERED knob's current value by the key the registry holds — keys as data, not one setting read",
  )

  private def sources: Seq[Path] = {
    val root = RepoRoot.dir.toPath
    for {
      directory <- Seq("common/src/main", "testkit/src/main", "worker/src/main", "worker/src/fixtures", "web/src/main", "e2e/src/main")
      dir        = root.resolve(directory)
      if Files.isDirectory(dir)
      file      <- Files.walk(dir).iterator().asScala.toSeq
      name       = file.getFileName.toString
      if name.endsWith(".scala") || name.endsWith(".scala.html")
    } yield file
  }

  /** Source with block, line, Scaladoc and Twirl comments removed, so a comment that NAMES
   *  the forbidden calls (as this project's comments often do) is not mistaken for one. */
  private def code(source: String): String = {
    // A block comment keeps its line breaks, so every reported line number is the file's own.
    def blank(pattern: scala.util.matching.Regex)(text: String): String =
      pattern.replaceAllIn(text, found => scala.util.matching.Regex.quoteReplacement(found.matched.filter(_ == '\n')))
    val withoutBlocks = blank("""(?s)@\*.*?\*@""".r)(blank("""(?s)/\*.*?\*/""".r)(source))
    withoutBlocks.replaceAll("""(?m)(?<![:"])//.*$""", "")
  }

  private def offenders(pattern: scala.util.matching.Regex, allowed: Map[String, String]): Seq[String] = {
    val root = RepoRoot.dir.toPath
    sources.flatMap { path =>
      val relative = root.relativize(path).toString
      if (allowed.contains(relative)) Nil
      else code(Files.readString(path)).linesIterator.zipWithIndex.collect {
        case (line, index) if pattern.findFirstIn(line).isDefined => s"$relative:${index + 1}: ${line.trim}"
      }.toSeq
    }
  }

  "production code" should "read the process's environment and properties only through ProcessConfiguration" in {
    withClue("take the value as a typed constructor/method parameter, resolved by settings.ProcessConfiguration at the " +
      "composition root (AppLoader, WorkerMain, a script's main) and passed down: ") {
      offenders(ProcessAccess, ProcessAllowed) shouldBe empty
    }
  }

  it should "read a configuration key only in ProcessConfiguration" in {
    withClue("add an accessor to settings.ProcessConfiguration returning a type of its own, and take that value " +
      "as a parameter: ") {
      offenders(KeyRead, KeyReadAllowed) shouldBe empty
    }
  }

  "the allow-lists" should "name only files that exist" in {
    (ProcessAllowed.keys ++ KeyReadAllowed.keys).filterNot(path => Files.exists(RepoRoot.dir.toPath.resolve(path))) shouldBe empty
  }
}
