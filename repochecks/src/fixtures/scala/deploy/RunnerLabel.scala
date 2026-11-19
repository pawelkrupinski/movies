package deploy

/**
 * The runner label a workflow asks for, EVALUATED for one forge and one ref: a job's `runs-on:`, or a reusable call's
 * `lane:` / `runner:` that the called workflow's `runs-on:` reads back as `inputs.<name>`.
 *
 * Only the subset of GitHub's expression language those lines use: string literals, `github.server_url`,
 * `github.ref`, `github.run_id`, `github.workflow` (the caller's name, in a reusable workflow), `github.event.workflow_run.<field>` (the run a `workflow_run` reports; unset outside one), `inputs.<name>`, `matrix.<name>` (a matrix job's own entry), `vars.<name>` (a repository variable: MOVIES_CI_RUNNERS), `needs.<job>.outputs.<name>`, `steps.<id>.outputs.<name>` (a plan job's own step, in its `outputs:`), `startsWith(a, b)` (GitHub's: ignoring case), `format(fmt, args...)` (`{N}` placeholders), `fromJSON(s)` of a string or an array of strings (a job asking for several labels, read back as them joined by `,`), `==`, `!=`, `!`, `&&`, `||` and parentheses. Anything else fails, naming the
 * expression, rather than being read as some label -- a spec that guessed would pass on a line it never understood.
 */
object RunnerLabel {

  val GitHub  = "https://github.com"
  val Main    = "refs/heads/main"

  /** Where a workflow runs: the forge (`github.server_url`), the ref, the inputs a caller passed, the outputs of
   *  the jobs it needs, keyed `<job>.<name>`, the plan job's own steps' outputs, keyed `<step>.<name>` (unset: none, as a
   *  skipped step leaves it), the repository's variables (unset: none), the running workflow's name and,
   *  for a `workflow_run`, the fields of the run that started it (`head_branch`, `name`). */
  final case class Context(
      server: String,
      ref: String,
      inputs: Map[String, String] = Map.empty,
      outputs: Map[String, String] = Map.empty,
      vars: Map[String, String] = Map.empty,
      workflow: String = "",
      workflowRun: Map[String, String] = Map.empty,
      steps: Map[String, String] = Map.empty,
      runId: String = "1",
      // a matrix job's own entry (`matrix.<name>`): a job whose runs-on reads one is evaluated once per entry
      matrix: Map[String, String] = Map.empty
  )

  private enum Value {
    case Str(s: String)
    case Bool(b: Boolean)
    case Labels(all: Seq[String])
    case Null
  }
  import Value.*

  private def truthy(v: Value): Boolean = v match {
    case Str(s)    => s.nonEmpty
    case Bool(b)   => b
    case Labels(_) => true
    case Null      => false
  }

  private def text(v: Value): String = v match {
    case Str(s)      => s
    case Bool(b)     => b.toString
    case Labels(all) => all.mkString(",")
    case Null        => ""
  }

  private val Expression = """\$\{\{(.*)}}""".r
  private val Token      = """'(?:[^']|'')*'|==|!=|&&|\|\||!|\(|\)|[A-Za-z_][\w.-]*|\S""".r

  /** `value` as the runner sees it in `ctx`: a bare word as written, an expression evaluated. */
  def apply(value: String, ctx: Context): String = value.trim match {
    case Expression(expr) => text(new Parser(expr, ctx).whole())
    case word             => word
  }

  /** Recursive descent over the tokens, GitHub's precedence: `||` < `&&` < `!` < `==`/`!=`. `&&` and `||` yield an
   *  operand, not a boolean, which is what makes `cond && 'a' || 'b'` a choice. */
  private final class Parser(expr: String, ctx: Context) {
    private val tokens = Token.findAllIn(expr).toVector
    private var at     = 0

    private def fail(why: String): Nothing =
      throw new AssertionError(s"runner expression `$expr`: $why (RunnerLabel reads only the subset its doc names)")

    private def peek: Option[String] = tokens.lift(at)

    private def quoted(json: String): Boolean = json.length >= 2 && json.startsWith("\"") && json.endsWith("\"")

    /** A call's parenthesised, comma-separated arguments. */
    private def arguments(name: String): Seq[Value] = {
      if (next() != "(") fail(s"`$name` without `(`")
      val args = Seq.newBuilder[Value]
      args += or()
      while (peek.contains(",")) { next(); args += or() }
      if (next() != ")") fail(s"unbalanced `$name(`")
      args.result()
    }

    private def next(): String = {
      val t = peek.getOrElse(fail("ends early"))
      at += 1
      t
    }

    def whole(): Value = {
      val v = or()
      if (peek.nonEmpty) fail(s"unexpected `${peek.get}`")
      v
    }

    private def or(): Value = {
      var v = and()
      while (peek.contains("||")) { next(); val r = and(); v = if (truthy(v)) v else r }
      v
    }

    private def and(): Value = {
      var v = unary()
      while (peek.contains("&&")) { next(); val r = unary(); v = if (truthy(v)) r else v }
      v
    }

    private def unary(): Value =
      if (peek.contains("!")) { next(); Bool(!truthy(unary())) }
      else comparison()

    private def comparison(): Value = {
      val l = primary()
      peek match {
        case Some(op @ ("==" | "!=")) =>
          next()
          val same = text(l).equalsIgnoreCase(text(primary()))
          Bool(if (op == "==") same else !same)
        case _ => l
      }
    }

    private def primary(): Value = next() match {
      case "("                                => val v = or(); if (next() != ")") fail("unbalanced `(`"); v
      case lit if lit.startsWith("'")         => Str(lit.drop(1).dropRight(1).replace("''", "'"))
      case "github.server_url"                => Str(ctx.server)
      case "github.ref"                       => Str(ctx.ref)
      case "github.run_id"                    => Str(ctx.runId)
      case "github.workflow"                  => Str(ctx.workflow)
      case s"github.event.workflow_run.$field" => ctx.workflowRun.get(field).fold(Null)(Str(_))
      case s"inputs.$name"                    => ctx.inputs.get(name).fold(Null)(Str(_))
      case s"matrix.$name"                    => ctx.matrix.get(name).fold(Null)(Str(_))
      case s"vars.$name"                      => Str(ctx.vars.getOrElse(name, ""))
      case s"needs.$job.outputs.$name"        => ctx.outputs.get(s"$job.$name").fold(Null)(Str(_))
      case s"steps.$step.outputs.$name"       => ctx.steps.get(s"$step.$name").fold(Null)(Str(_))
      case b @ ("true" | "false")             => Bool(b == "true")
      case "startsWith"                       =>
        if (next() != "(") fail("`startsWith` without `(`")
        val whole = text(or())
        if (next() != ",") fail("`startsWith` takes two arguments")
        val prefix = text(or())
        if (next() != ")") fail("unbalanced `startsWith(`")
        Bool(whole.toLowerCase.startsWith(prefix.toLowerCase))
      case "format"                           =>
        val args = arguments("format").map(text)
        if (args.isEmpty) fail("`format` without a format")
        Str(args.tail.zipWithIndex.foldLeft(args.head) { case (acc, (arg, i)) => acc.replace(s"{$i}", arg) })
      case "fromJSON"                         =>
        arguments("fromJSON").map(text) match {
          case Seq("")                                              => Null // an output left unset
          case Seq(json) if quoted(json)                            => Str(json.drop(1).dropRight(1))
          case Seq(json) if json.startsWith("[") && json.endsWith("]") =>
            val items = json.drop(1).dropRight(1).split(",").map(_.trim).toSeq
            if (!items.forall(quoted)) fail(s"fromJSON of `$json`: not an array of strings")
            Labels(items.map(_.drop(1).dropRight(1)))
          case other => fail(s"fromJSON of `${other.mkString(", ")}`: not a string or an array of strings")
        }
      case other                              => fail(s"`$other` is outside the subset")
    }
  }
}
