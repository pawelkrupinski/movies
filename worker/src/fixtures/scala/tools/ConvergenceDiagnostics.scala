package tools

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

/**
 * Where a convergence leg leaves what its log has no room for — the full per-film lists behind a red
 * assertion — and the short account of them for the run's summary page.
 *
 * The leg's workflow uploads [[Directory]] as the job's `convergence-diagnostics-<code>-<phase>` artifact and
 * appends every `.md` in it to `$GITHUB_STEP_SUMMARY` (`.github/actions/convergence-findings`), so a
 * failure names its films on the run page and the artifact holds every one of them.
 */
object ConvergenceDiagnostics {

  val Directory: Path = Paths.get("target", "convergence-diagnostics")

  /** The whole report `name`, as `<name>.txt`. */
  def write(name: String, lines: Seq[String]): Path = put(s"$name.txt", lines.mkString("", "\n", "\n"))

  /** The summary page's account of `name` — `title` over `lines` in a code block — as `<name>.md`. */
  def summarise(name: String, title: String, lines: Seq[String]): Path =
    put(s"$name.md", (s"### $title" +: "" +: "```" +: lines :+ "```").mkString("", "\n", "\n"))

  private def put(file: String, text: String): Path = {
    Files.createDirectories(Directory)
    Files.write(Directory.resolve(file), text.getBytes(UTF_8))
  }
}
