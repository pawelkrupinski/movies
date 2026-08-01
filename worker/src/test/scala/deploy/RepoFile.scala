package deploy

import scala.io.{Codec, Source}

/**
 * Reads a repo-root file as text, for the config-lock specs in this package
 * that guard deploy wiring no running-JVM test layer can reach (Dockerfile
 * CMD, fly*.toml env, Grafana provisioning).
 *
 * Tests run with the repo root as CWD (the fixture specs load
 * `test/resources/...` the same way), so top-level paths resolve directly.
 */
object RepoFile {
  def read(path: String): String = {
    val src = Source.fromFile(path)(using Codec.UTF8)
    try src.mkString
    finally src.close()
  }

  /**
   * One YAML block of `text`: the line whose key is `key`, plus everything
   * indented under it, stopping at the next key at the same indentation.
   *
   * For the workflow specs, which assert on ONE job or ONE top-level section and
   * would otherwise read a neighbouring block's settings as their own — a
   * `needs:` belonging to the job below, a `cancel-in-progress:` from another
   * workflow section. Trailing blank and comment lines are dropped, since a
   * comment at the block's own indentation usually introduces the NEXT key.
   */
  def block(text: String, key: String): String = {
    val lines = text.linesIterator.toVector
    val start = lines.indexWhere(_.trim == s"$key:")
    require(start >= 0, s"no `$key:` line in the file")
    val indent = lines(start).takeWhile(_ == ' ').length
    val body = lines
      .drop(start + 1)
      .takeWhile { line =>
        val trimmed = line.trim
        trimmed.isEmpty || trimmed.startsWith("#") || line.takeWhile(_ == ' ').length > indent
      }
      .reverse
      .dropWhile(line => line.trim.isEmpty || line.trim.startsWith("#"))
      .reverse
    (lines(start) +: body).mkString("\n")
  }

  /** Every `fly*.toml` at the repo root, newest country last — the authoritative deploy set. */
  def flyTomls(): Seq[java.io.File] =
    Option(new java.io.File(".").listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(f => f.getName.startsWith("fly") && f.getName.endsWith(".toml"))
      .sortBy(_.getName)
      .toSeq
}
