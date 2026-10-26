package deploy

/**
 * The runner label a workflow asks for, EVALUATED for one forge and one ref: a job's `runs-on:`, or a reusable call's
 * `lane:` / `runner:` that the called workflow's `runs-on:` reads back as `inputs.<name>`.
 *
 * Only the subset of GitHub's expression language those lines use: string literals, `github.server_url`,
 * `github.ref`, `inputs.<name>`, `needs.<job>.outputs.<name>`, `==`, `!=`, `!`, `&&`, `||` and parentheses. Anything else fails, naming the
 * expression, rather than being read as some label -- a spec that guessed would pass on a line it never understood.
 */
object RunnerLabel {

  val GitHub  = "https://github.com"
  val Forgejo = "https://forgejo.internal"
  val Main    = "refs/heads/main"
  val Branch  = "refs/heads/measure/agent/0000000-test"

  /** Where a workflow runs: the forge (`github.server_url`), the ref, the inputs a caller passed, and the outputs of
   *  the jobs it needs, keyed `<job>.<name>`. */
  final case class Context(
      server: String,
      ref: String,
      inputs: Map[String, String] = Map.empty,
      outputs: Map[String, String] = Map.empty
  )

  private enum Value {
    case Str(s: String)
    case Bool(b: Boolean)
    case Null
  }
  import Value.*

  private def truthy(v: Value): Boolean = v match {
    case Str(s)  => s.nonEmpty
    case Bool(b) => b
    case Null    => false
  }

  private def text(v: Value): String = v match {
    case Str(s)  => s
    case Bool(b) => b.toString
    case Null    => ""
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
      case s"inputs.$name"                    => ctx.inputs.get(name).fold(Null)(Str(_))
      case s"needs.$job.outputs.$name"        => ctx.outputs.get(s"$job.$name").fold(Null)(Str(_))
      case b @ ("true" | "false")             => Bool(b == "true")
      case other                              => fail(s"`$other` is outside the subset")
    }
  }
}
