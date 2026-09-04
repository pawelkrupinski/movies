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

  /** Where the GitOps manifests are checked out — see `infra/bin/fetch-gitops`. */
  private val GitOpsRoot = "infra/kubernetes"

  def read(path: String): String = {
    if (path.startsWith(s"$GitOpsRoot/") && !new java.io.File(path).exists())
      throw new AssertionError(
        s"""$path is missing because the GitOps manifests are no longer in this repository.
           |
           |They live in pawelkrupinski/movies-gitops now — Flux pulls its source on every
           |reconcile, and a shallow clone of THIS repository is 93.5s and 18,806 files to reach
           |36 of them. The specs still read the old paths, because CI checks that repository out
           |right here. Locally:
           |
           |    ./infra/bin/fetch-gitops
           |""".stripMargin)
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

  /** `KINOWO_SCRAPE_FRESHNESS_MINUTES` out of a deploy config, whichever syntax it
   *  is written in: `= '420'` in a fly toml, `: "840"` in a k3s overlay's ConfigMap.
   *  Only the digits are kept, so the quoting style is not part of the contract —
   *  which matters because the newest country has no fly toml at all. */
  def freshnessMinutesIn(text: String): Option[Int] =
    text.linesIterator
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .collectFirst { case s"KINOWO_SCRAPE_FRESHNESS_MINUTES$rest" => rest.filter(_.isDigit) }
      .filter(_.nonEmpty)
      .map(_.toInt)

  /** The cadence a country's worker ACTUALLY deploys with, in minutes.
   *
   *  Read from its k3s overlay, which is the live deploy path — every `main.yml`
   *  WORKER leg is `enabled: false`, and the newest country never had a fly toml. This
   *  is the only place a country's sweep rate exists: `Freshness.defaultScrapeTtl`
   *  reads the env var at runtime and `WorkerWiring` captures it once, so no
   *  `Country` field and no running-JVM test can reach it. */
  def deployedFreshnessMinutes(cc: String): Option[Int] =
    scala.util.Try(read(s"infra/kubernetes/worker/overlays/$cc/patch.yaml"))
      .toOption
      .flatMap(freshnessMinutesIn)

  /** Every workflow file under `.github/workflows/`, sorted by name — the set a
   *  repo-wide rule about what CI is allowed to do has to be checked against.
   *  Enumerated rather than listed in each spec, so a workflow added tomorrow is
   *  covered by the rule the day it lands. */
  def workflows(): Seq[java.io.File] =
    Option(new java.io.File(".github/workflows").listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(f => f.getName.endsWith(".yml") || f.getName.endsWith(".yaml"))
      .sortBy(_.getName)
      .toSeq

  /** Every `fly*.toml` at the repo root, newest country last — the authoritative deploy set. */
  def flyTomls(): Seq[java.io.File] =
    Option(new java.io.File(".").listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(f => f.getName.startsWith("fly") && f.getName.endsWith(".toml"))
      .sortBy(_.getName)
      .toSeq
}
