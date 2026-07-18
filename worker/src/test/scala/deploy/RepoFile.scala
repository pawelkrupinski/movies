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

  /** Every `fly*.toml` at the repo root, newest country last — the authoritative deploy set. */
  def flyTomls(): Seq[java.io.File] =
    Option(new java.io.File(".").listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(f => f.getName.startsWith("fly") && f.getName.endsWith(".toml"))
      .sortBy(_.getName)
      .toSeq
}
