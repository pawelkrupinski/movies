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

  /** Whether a repo-root file exists — for a rule that a retired file stays retired. */
  def exists(path: String): Boolean = new java.io.File(path).exists()

  /** Every repo-relative file read so far, by path. The repository does not change under a
   *  test run — no spec writes into it; the ones that write, write under a temp directory —
   *  and a dozen specs each re-read `main.yml` and `ci.yml`. Absolute paths (a spec's own temp
   *  files) are read fresh every time. */
  private val cache = new java.util.concurrent.ConcurrentHashMap[String, String]()

  def read(path: String): String =
    if (new java.io.File(path).isAbsolute) readFresh(path)
    else cache.computeIfAbsent(path, readFresh)

  private def readFresh(path: String): String = {
    if (!new java.io.File(path).exists()) failMissing(path)
    val src = Source.fromFile(path)(using Codec.UTF8)
    try src.mkString
    finally src.close()
  }

  /** A path a spec expected and did not find. Under the GitOps root that is almost always a
   *  checkout without `fetch-gitops`, so say that; anywhere else, say which path. */
  private def failMissing(path: String): Nothing =
    if (path.startsWith(s"$GitOpsRoot/") || path == GitOpsRoot)
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
    else throw new java.io.FileNotFoundException(s"$path does not exist (the specs run with the repo root as CWD)")

  /** The entries of `dir` that `keep` accepts, sorted by name — and never an empty answer.
   *  Every caller sweeps a rule over the listing, so a missing or emptied directory would
   *  make that rule pass vacuously; it fails here instead, naming the directory. */
  def listed(dir: String)(keep: java.io.File => Boolean): Seq[java.io.File] = {
    val entries = Option(new java.io.File(dir).listFiles()).getOrElse(failMissing(dir))
    val kept    = entries.filter(keep).sortBy(_.getName).toSeq
    if (kept.isEmpty) throw new AssertionError(s"$dir has no entry this spec sweeps — its rule would pass vacuously")
    kept
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

  /**
   * A workflow's jobs, by name: each job's own block (see [[block]]), for the specs that
   * assert on one job's `needs:`, `if:`, `permissions:` or steps without reading a
   * neighbour's.
   */
  /** A workflow's `matrix:` rows written as flow maps (`- { country: poland, code: pl, … }`),
   *  each as its fields; a value is a bare word (quoted or expression values are not read). */
  /** What build.sbt's `addCommandAlias("<name>", …)` maps `name` to. Read by name rather than
   *  matched as a whole line, so the alias table stays free to align its columns. */
  def commandAlias(name: String): String =
    ("addCommandAlias\\(\"" + name + "\",\\s*\"([^\"]*)\"").r.findFirstMatchIn(read("build.sbt"))
      .getOrElse(throw new AssertionError(s"build.sbt defines no `$name` alias")).group(1)

  def matrixRows(yaml: String): Seq[Map[String, String]] =
    """-\s*\{([^}]*)}""".r
      .findAllMatchIn(block(yaml, "matrix"))
      .map(row => """(\w+):\s*([\w-]+)""".r.findAllMatchIn(row.group(1)).map(f => f.group(1) -> f.group(2)).toMap)
      .toSeq

  def jobs(yml: String): Map[String, String] = {
    val jobsBlock = block(yml, "jobs")
    val Header    = """^(\s+)([A-Za-z][\w-]*):\s*$""".r
    val topIndent = jobsBlock.linesIterator.drop(1)
      .collectFirst { case Header(indent, _) => indent.length }
      .getOrElse(throw new AssertionError("`jobs:` has no job under it"))
    jobsBlock.linesIterator
      .collect { case Header(indent, name) if indent.length == topIndent => name }
      .map(name => name -> block(jobsBlock, name))
      .toMap
  }

  /** `text` without its whole-line `#` comments — for an assertion that some file or job RUNS
   *  something, which a comment naming the same thing would otherwise satisfy after the real
   *  line is gone. */
  def withoutComments(text: String): String =
    text.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")

  /** Where in `items` the first one containing `fragment` sits — failing, never `-1`, when
   *  none does, so an ordering assertion cannot pass on a renamed step. */
  def positionOf(items: Seq[String], fragment: String): Int = {
    val at = items.indexWhere(_.contains(fragment))
    if (at < 0) throw new AssertionError(s"nothing contains `$fragment`, so its order cannot be checked")
    at
  }

  /** Where in `text` `fragment` first occurs — failing, never `-1`, when it does not. */
  def positionOf(text: String, fragment: String): Int = {
    val at = text.indexOf(fragment)
    if (at < 0) throw new AssertionError(s"no `$fragment` in the text, so its order cannot be checked")
    at
  }

  /**
   * The workflow step named `stepName` — its `- name:` line and everything up
   * to the next `- ` item at the same indentation — so a spec asserting on one
   * step's `continue-on-error:` or `run:` cannot read the neighbouring step's.
   */
  def step(yml: String, stepName: String): String = {
    val lines = yml.linesIterator.toVector
    val start = lines.indexWhere(_.trim == s"- name: $stepName")
    require(start >= 0, s"no `- name: $stepName` step in the workflow")
    val indent = lines(start).takeWhile(_ == ' ').length
    // Trailing blank and comment lines are dropped, as `block` drops them: a comment just above
    // the next `- name:` introduces THAT step, and a needle it holds must not pass for this one.
    val body = lines
      .drop(start + 1)
      .takeWhile(l => l.trim.isEmpty || l.trim.startsWith("#") || l.takeWhile(_ == ' ').length > indent)
      .reverse
      .dropWhile(l => l.trim.isEmpty || l.trim.startsWith("#"))
      .reverse
    (lines(start) +: body).mkString("\n")
  }

  /** One `run:` of a workflow or composite action: the shell it runs (comment lines dropped) and
   *  its step's `working-directory`, if it names one. */
  final case class RunStep(script: String, workingDirectory: Option[String])

  /** Every `run:` in `yml` — what a workflow actually EXECUTES, unlike its comments, its `paths:`
   *  triggers or a step's name, which all mention scripts they never run. */
  def runSteps(yml: String): Seq[RunStep] = {
    val lines = yml.linesIterator.toVector
    def indentOf(line: String) = line.takeWhile(_ == ' ').length
    def code(line: String) = line.trim.nonEmpty && !line.trim.startsWith("#")
    lines.indices.flatMap { at =>
      val line = lines(at)
      val key  = line.indexOf("run:")
      val isRun = key >= 0 && { val before = line.take(key).trim; before.isEmpty || before == "-" }
      Option.when(isRun) {
        val inline = line.drop(key + "run:".length).trim
        val script =
          if (inline.startsWith("|") || inline.startsWith(">"))
            lines.drop(at + 1).takeWhile(l => l.trim.isEmpty || indentOf(l) > key).filter(code).map(_.trim).mkString("\n")
          else inline
        // The step is the `- ` item holding this key: from its dash to the next line at or
        // left of the dash.
        val dashAt = (at to 0 by -1).find { i => val t = lines(i).trim; t.startsWith("- ") && indentOf(lines(i)) < key }
          .getOrElse(at)
        val dash = indentOf(lines(dashAt))
        val step = lines.drop(dashAt + 1).takeWhile(l => !code(l) || indentOf(l) > dash)
        val workingDirectory = (lines(dashAt) +: step).map(_.trim.stripPrefix("- "))
          .collectFirst { case s"working-directory: $dir" => dir.trim }
        RunStep(script, workingDirectory)
      }
    }
  }

  /**
   * The `run: |` body of the workflow step named `stepName`, verbatim and
   * de-indented to its own margin — for the specs that RUN the shell CI runs
   * rather than pattern-match it (FluxImageAutomationSpec, CoverageWorkflowSpec).
   */
  def stepScript(yml: String, stepName: String): String = {
    val lines = step(yml, stepName).linesIterator.toVector
    val runAt = lines.indexWhere(_.trim == "run: |")
    require(runAt >= 0, s"the `$stepName` step has no `run: |` body")
    val indent = lines(runAt).takeWhile(_ == ' ').length
    lines
      .drop(runAt + 1)
      .takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > indent)
      .mkString("\n")
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

  /** The per-country thresholds an alert file spells for `metric`, in the unit the metric
   *  carries: every `metric{...country="xx"...} > N` clause -- bare, or bridged over a worker
   *  restart as `last_over_time(metric{...}[10m]) > N` -- keyed by country. The shape
   *  both cadence-derived alerts take (`CinemaScrapeOldestAgeHigh`,
   *  `ChangeStreamMoviesCursorSilent`), because PromQL cannot read a ConfigMap and each
   *  country's literal has to be pinned to the overlay it was derived from. */
  def perCountryThresholds(rules: String, metric: String): Map[String, Long] =
    (java.util.regex.Pattern.quote(metric) + """\{([^}]*)\}(?:\[[^\]]*\]\))?\s*>\s*(\d+)""").r
      .findAllMatchIn(rules)
      .flatMap(m => """country="([a-z]+)"""".r.findFirstMatchIn(m.group(1)).map(_.group(1) -> m.group(2).toLong))
      .toMap

  /** The cadence a country's worker ACTUALLY deploys with, in minutes.
   *
   *  Read from its k3s overlay, which is the live deploy path — every `main.yml`
   *  WORKER leg is `enabled: false`, and the newest country never had a fly toml. This
   *  is the only place a country's sweep rate exists: `Freshness.scrapeTtlFrom`
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
    listed(".github/workflows")(f => f.getName.endsWith(".yml") || f.getName.endsWith(".yaml"))

  /** Every composite action's `action.yml` under `.github/actions/`, sorted by path —
   *  the other half of what CI runs, enumerated for the same reason as [[workflows]]. */
  def compositeActions(): Seq[String] =
    listed(".github/actions")(_.isDirectory)
      .map(dir => s".github/actions/${dir.getName}/action.yml")
      .filter(path => new java.io.File(path).isFile && read(path).contains("using: composite"))

  /** Every file CI runs: the [[workflows]], then the [[compositeActions]] they call. */
  def ciFiles(): Seq[String] = workflows().map(_.getPath) ++ compositeActions()

  /** Every `fly*.toml` at the repo root, newest country last — the authoritative deploy set. */
  def flyTomls(): Seq[java.io.File] =
    listed(".")(f => f.getName.startsWith("fly") && f.getName.endsWith(".toml"))

  /** Every dashboard monitoring-1's Grafana provisions from the apps folder, sorted by name. */
  def dashboards(): Seq[java.io.File] = {
    val dir   = "infra/nix/files/monitoring/grafana/dashboards/apps"
    val files = listed(dir)(_.getName.endsWith(".json"))
    // More than one: a sweep that only ever sees a single dashboard is a sign the glob or the
    // directory moved, not that the fleet shrank to one board.
    if (files.size < 2) throw new AssertionError(s"$dir lists only ${files.map(_.getName)} — expected several dashboards")
    files
  }

  /** Every country that deploys a worker — one k3s overlay directory each, sorted. Read
   *  from the overlays rather than listed, so a country onboarded tomorrow is covered by
   *  every per-country rule the day it lands. */
  def workerOverlayCountries(): Seq[String] =
    listed(s"$GitOpsRoot/worker/overlays")(_.isDirectory).map(_.getName)
}
