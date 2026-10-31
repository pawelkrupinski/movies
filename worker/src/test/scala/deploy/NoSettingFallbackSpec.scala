package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/**
 * A MISSING SETTING FAILS; IT NEVER FALLS BACK (simplification-2026-10-31 item 6). Main #2030: the image registry came
 * from repository variables that existed only on the old forge, with ghcr.io as the fallback -- github.com then logged
 * in to ghcr.io with registry.kinowo.net's password. A variable or secret with a default hides that it is unset.
 *
 *  - no `vars.X || …` in a workflow or action: a constant is written inline, a setting is required;
 *  - no `secrets.X || '<value>'`: an unset secret is empty, and the step that needs it says so;
 *  - no `${NAME:-<value>}` default for a registry, password, token, secret or credential in a script.
 *
 * A site where a default is genuinely right says why in a `# allow-fallback: …` comment on the line above (or the same
 * line).
 */
class NoSettingFallbackSpec extends AnyFlatSpec with Matchers {
  private val Roots   = Seq(".github", "scripts", "infra/bin")
  private val Allowed = "allow-fallback:"
  private val Rules = Seq(
    """vars\.[A-Za-z_]+\s*\|\|""".r                                                    -> "a repository variable with a fallback",
    """secrets\.[A-Za-z_]+\s*\|\|\s*'[^']+'""".r                                        -> "a secret with a default value",
    """\$\{(?:[A-Za-z_]*REGISTRY[A-Za-z_]*|[A-Za-z_]*(?:PASSWORD|TOKEN|SECRET|CREDENTIALS?)):-[^}]""".r ->
      "a credential setting with a default"
  )

  private def files(root: String): Seq[Path] =
    if (!Files.isDirectory(Paths.get(root))) Nil
    else {
      val stream = Files.walk(Paths.get(root))
      try stream.iterator.asScala.filter(Files.isRegularFile(_)).toSeq finally stream.close()
    }

  private def scanned(path: String): Boolean =
    !path.contains("/node_modules/") && !path.endsWith("-test.sh") && !path.endsWith("_test.py") &&
      (path.endsWith(".yml") || path.endsWith(".yaml") || path.endsWith(".sh") || path.endsWith(".py") ||
        (path.startsWith("scripts/") && !path.contains(".")) || (path.startsWith("infra/bin/") && !path.contains(".")))

  private[deploy] def offenders(text: String): Seq[(Int, String)] = {
    val lines = text.split("\n", -1).toVector
    lines.indices.flatMap { i =>
      val line    = lines(i)
      val code    = if (line.trim.startsWith("#")) "" else line
      val excused = line.contains(Allowed) || (i > 0 && lines(i - 1).contains(Allowed))
      Rules.collectFirst { case (re, why) if !excused && re.findFirstIn(code).isDefined => (i + 1, s"$why: ${line.trim}") }
    }
  }

  "CI and tool scripts" should "never default a setting that may be unset" in {
    val found = Roots.flatMap(files).map(_.toString).filter(scanned).sorted.flatMap { path =>
      offenders(RepoFile.read(path)).map { case (n, what) => s"$path:$n  $what" }
    }
    withClue(s"Require the setting, write a constant inline, or say why with '# $Allowed …':\n  ${found.mkString("\n  ")}\n") {
      found shouldBe empty
    }
  }

  "The lint" should "see each kind of fallback and honour an allow-fallback reason" in {
    val text =
      """IMAGE_REPO: ${{ vars.IMAGE_REPO || 'ghcr.io/x' }}
        |URL: ${{ secrets.GRAFANA_URL || 'https://g' }}
        |PASS: ${{ inputs.mode == 'record' && secrets.PASS || '' }}
        |token="${REGISTRY_TOKEN:-dummy}"
        |token="${REGISTRY_TOKEN:-}"
        |# allow-fallback: a laptop run has no token and reads anonymously
        |t="${GH_TOKEN:-anon}"
        |# vars.X || 'commented out'
        |bin="${SECRETS_BIN:-/usr/local/bin/secrets}"
        |""".stripMargin
    offenders(text).map(_._1) shouldBe Seq(1, 2, 4)
  }
}
