package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Production code reads this process's environment variables and system properties in ONE
 * place. `Env` binds its sources to the real process (`Env.fromProcess`), and
 * [[ProcessConfiguration]] is the one resolver that calls it and turns what it reads into
 * the typed values a composition root hands down — the country, the Mongo address, the
 * commit, the port, the JDK network policies. Everything else is HANDED those values
 * through its constructor or parameters, so a spec builds them directly and no code deep in
 * the object graph can quietly pick a different answer than its root did.
 *
 * Scans every module's `src/main` (Scala and Twirl) with comments stripped. The allow-list
 * names each exception and why; a new one needs a reason as good.
 */
class ProcessAccessLintSpec extends AnyFlatSpec with Matchers {

  private val ProcessAccess =
    """\bEnv\s*\.\s*fromProcess\b|\bsys\s*\.\s*(props|env)\b|\bSystem\s*\.\s*(getenv|getProperty|getProperties|setProperty|clearProperty|setProperties)\b""".r

  /** file → why it may touch the process directly. */
  private val Allowed: Map[String, String] = Map(
    "common/src/main/scala/tools/Env.scala" ->
      "Env's process binding: environment variables, then system properties, then .env.local",
    "common/src/main/scala/tools/ProcessConfiguration.scala" ->
      "the ONE resolver: reads the process through Env.fromProcess and yields the typed values roots pass down",
    "common/src/main/scala/tools/ProxyTunnelAuthentication.scala" ->
      "writes a policy the JDK itself reads (jdk.http.auth.tunneling.disabledSchemes); applied once by each main",
    "common/src/main/scala/tools/IssuerCertificateFetching.scala" ->
      "writes a policy the JDK itself reads (com.sun.security.enableAIAcaIssuers); applied once by each main",
  )

  private def mainSources: Seq[Path] = {
    val root = RepoRoot.dir.toPath
    for {
      module <- Seq("common", "testkit", "worker", "web", "e2e")
      dir     = root.resolve(s"$module/src/main")
      if Files.isDirectory(dir)
      file   <- Files.walk(dir).iterator().asScala.toSeq
      name    = file.getFileName.toString
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

  private def offenders: Seq[String] = {
    val root = RepoRoot.dir.toPath
    mainSources.flatMap { path =>
      val relative = root.relativize(path).toString
      if (Allowed.contains(relative)) Nil
      else code(Files.readString(path)).linesIterator.zipWithIndex.collect {
        case (line, index) if ProcessAccess.findFirstIn(line).isDefined => s"$relative:${index + 1}: ${line.trim}"
      }.toSeq
    }
  }

  "production code" should "read the process's environment and properties only through ProcessConfiguration" in {
    withClue("take the value as a typed constructor/method parameter, resolved by ProcessConfiguration at the " +
      "composition root (AppLoader, WorkerMain, a script's main) and passed down: ") {
      offenders shouldBe empty
    }
  }

  "the allow-list" should "name only files that exist" in {
    Allowed.keys.filterNot(path => Files.exists(RepoRoot.dir.toPath.resolve(path))) shouldBe empty
  }
}
