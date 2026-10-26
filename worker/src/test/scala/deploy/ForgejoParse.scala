package deploy

/**
 * WHAT FORGEJO EVALUATES A JOB'S OWN KEYS IN, read off its job parser (code.forgejo.org/forgejo/runner
 * act/jobparser/jobparser.go, and services/actions/job_emitter.go's `tryHandleIncompleteMatrix` in Forgejo 15).
 *
 *  - At RUN CREATION every job is parsed with the whole `github` context and the run's `inputs` -- except that the run
 *    has no id yet, so a reusable call's `with:` bakes `github.run_id` in as "" (movies run 408).
 *  - A job whose `strategy.matrix`, `runs-on` or `with:` reads a needed job's output cannot be completed then: it is
 *    marked incomplete and RE-PARSED once its needs are done, with their outputs and NOTHING ELSE -- no `inputs`, no
 *    `github`, no `vars`. A read of any of them there is empty, silently: main's convergence legs went to `measure`
 *    (run 2783) and takes-base, profile and recording inputs were dropped (2026-10-23..26). The jobs a re-parsed
 *    reusable call expands into are created by that same re-parse: their `inputs` are the call's `with:`, their
 *    `github` is empty.
 *  - A job's `if:` is evaluated by the runner, with the whole context.
 *
 * Text, not YAML: the workflow specs read workflows as text (RepoFile), and every rule here is about what a job's own
 * keys READ, which is plain in the text.
 */
object ForgejoParse {

  /** The job-level keys whose values are decisions about the job: whether it runs, where, how long, and with what. */
  val DecisionKeys: Set[String] =
    Set("if", "runs-on", "with", "strategy", "timeout-minutes", "continue-on-error", "concurrency", "environment")

  /** The keys the re-parse evaluates. */
  val ReparsedKeys: Set[String] = Set("strategy", "runs-on", "with")

  private val Expression = """\$\{\{(.*?)}}""".r
  private val Literal    = """'(?:[^']|'')*'""".r
  private val Read       = """(?<![\w.-])(inputs|github|vars|needs|matrix)\.([\w-]+)""".r

  /** A line without its trailing `# comment`: a `#` after whitespace, outside every `${{ }}` and quoted string. */
  def stripComment(line: String): String = {
    var depth, quote = 0
    var i            = 0
    while (i < line.length) {
      if (line.startsWith("${{", i)) depth += 1
      else if (line.startsWith("}}", i) && depth > 0) depth -= 1
      else if (line(i) == '\'' && depth > 0) quote ^= 1
      else if (line(i) == '#' && depth == 0 && quote == 0 && (i == 0 || line(i - 1).isWhitespace))
        return line.take(i).replaceAll("\\s+$", "")
      i += 1
    }
    line
  }

  /** A job's own keys (not its steps'), each with its whole value, continuation lines included, comments dropped. */
  def keys(job: String): Map[String, String] = {
    val lines = job.linesIterator.toVector.drop(1).map(stripComment).filter(_.trim.nonEmpty)
    if (lines.isEmpty) Map.empty
    else {
      val indent = lines.head.takeWhile(_ == ' ').length
      val Key    = """^\s*([\w-]+):\s*(.*)$""".r
      lines
        .foldLeft(Vector.empty[(String, Vector[String])]) { (acc, line) =>
          line match {
            case Key(k, rest) if line.takeWhile(_ == ' ').length == indent => acc :+ (k -> Vector(rest))
            case _ if acc.nonEmpty                                       => acc.init :+ (acc.last._1 -> (acc.last._2 :+ line))
            case _                                                       => acc
          }
        }
        .map((k, v) => k -> v.mkString("\n"))
        .toMap
    }
  }

  /** The contexts `value` reads, as `<context>.<name>` (`github.server_url`, `inputs.tier`, `needs.preflight`). An
   *  `if:` is an expression whole; any other value only inside `${{ }}`. String literals are not reads. */
  def reads(key: String, value: String): Seq[String] = {
    val expressions = if (key == "if") Seq(value) else Expression.findAllMatchIn(value).map(_.group(1)).toSeq
    expressions.flatMap(e => Read.findAllMatchIn(Literal.replaceAllIn(e, "''")).map(m => s"${m.group(1)}.${m.group(2)}"))
  }

  /** Every read in the job's keys named `among`, with the key it is in. */
  def readsIn(job: String, among: Set[String]): Seq[(String, String)] =
    keys(job).toSeq.filter((k, _) => among(k)).flatMap((k, v) => reads(k, v).map(k -> _))

  /** Whether Forgejo re-parses the job once its needs are done (see the object's doc). */
  def reparsed(job: String): Boolean = readsIn(job, ReparsedKeys).exists(_._2.startsWith("needs."))

  /** The local reusable workflow a job calls, if it is a call. */
  def localCall(job: String): Option[String] =
    keys(job).get("uses").map(_.trim).collect { case s"./.github/workflows/$file" => file }

  def hasMatrix(job: String): Boolean = keys(job).get("strategy").exists(_.linesIterator.exists(_.trim.startsWith("matrix:")))

  def needs(job: String): Set[String] = keys(job).get("needs").toSet.flatMap { v =>
    v.replaceAll("[\\[\\]]", " ").split("[,\\s]+").map(_.stripPrefix("-").trim).filter(_.nonEmpty)
  }

  /** A job's steps, each as its own keys (see [[keys]]), in order. */
  def steps(job: String): Seq[Map[String, String]] = keys(job).get("steps").toSeq.flatMap { value =>
    val lines = value.linesIterator.toVector.filter(_.trim.nonEmpty)
    val dash  = lines.filter(_.trim.startsWith("- ")).map(_.takeWhile(_ == ' ').length).minOption
    dash.toSeq.flatMap { at =>
      val starts = lines.indices.filter(i => lines(i).takeWhile(_ == ' ').length == at && lines(i).trim.startsWith("- "))
      starts.zip(starts.drop(1) :+ lines.length).map { (from, until) =>
        val item = lines.slice(from, until)
        keys(("step:" +: (item.head.replaceFirst("- ", "  ") +: item.tail)).mkString("\n"))
      }
    }
  }

  /** Whether a workflow is a reusable one (`on: workflow_call`): its `inputs` are its caller's decisions, handed in. */
  def isCalled(yml: String): Boolean = RepoFile.withoutComments(yml).linesIterator.exists(_.trim == "workflow_call:")
}
