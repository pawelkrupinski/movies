package deploy

/**
 * The runner label a workflow asks for, EVALUATED for one forge and one ref: a job's `runs-on:`, or a reusable call's
 * `lane:` / `runner:` that the called workflow's `runs-on:` reads back as `inputs.<name>`.
 *
 * Only the subset of GitHub's expression language those lines use: string literals, `github.server_url`,
 * `github.ref`, `inputs.<name>`, `vars.<name>` (a repository variable: MOVIES_CI_RUNNERS), `needs.<job>.outputs.<name>`,
 * `matrix.<name>`, `fromJson(<a JSON object of strings>)` and indexing it (`[...]`: a leg's `row-runners` by its row's
 * phase), `==`, `!=`, `!`, `&&`, `||` and parentheses. Anything else fails, naming the
 * expression, rather than being read as some label -- a spec that guessed would pass on a line it never understood.
 */
object RunnerLabel {

  val GitHub  = "https://github.com"
  val Main    = "refs/heads/main"

  /** Where a workflow runs: the forge (`github.server_url`), the ref, the inputs a caller passed, the outputs of
   *  the jobs it needs, keyed `<job>.<name>`, the repository's variables (unset: none) and the matrix row's values. */
  final case class Context(
      server: String,
      ref: String,
      inputs: Map[String, String] = Map.empty,
      outputs: Map[String, String] = Map.empty,
      vars: Map[String, String] = Map.empty,
      matrix: Map[String, String] = Map.empty
  )

  private enum Value {
    case Str(s: String)
    case Bool(b: Boolean)
    case Obj(fields: Map[String, String])
    case Null
  }
  import Value.*

  private def truthy(v: Value): Boolean = v match {
    case Str(s)  => s.nonEmpty
    case Bool(b) => b
    case Obj(_)  => true
    case Null    => false
  }

  private def text(v: Value): String = v match {
    case Str(s)  => s
    case Bool(b) => b.toString
    case Obj(f)  => play.api.libs.json.Json.stringify(play.api.libs.json.Json.toJson(f))
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
      val l = indexed()
      peek match {
        case Some(op @ ("==" | "!=")) =>
          next()
          val same = text(l).equalsIgnoreCase(text(indexed()))
          Bool(if (op == "==") same else !same)
        case _ => l
      }
    }

    /** A primary and any `[key]` after it: an object's field, Null when it has none. */
    private def indexed(): Value = {
      var v = primary()
      while (peek.contains("[")) {
        next()
        val key = or()
        if (next() != "]") fail("unbalanced `[`")
        v = v match {
          case Obj(f) => f.get(text(key)).fold(Null)(Str(_))
          case Null   => Null
          case other  => fail(s"indexing `$other`, not an object")
        }
      }
      v
    }

    private def fromJson(): Value = {
      if (next() != "(") fail("`fromJson` without `(`")
      val arg = or()
      if (next() != ")") fail("unbalanced `fromJson(`")
      // an input no caller passed (the spec's context names none): nothing to read, as the default stands in for it
      if (arg == Null) return Null
      play.api.libs.json.Json.parse(text(arg)) match {
        case o: play.api.libs.json.JsObject =>
          Obj(o.value.toMap.map((k, v) => k -> v.asOpt[String].getOrElse(fail(s"fromJson: `$k` is not a string"))))
        case other => fail(s"fromJson of `$other`, not an object of strings")
      }
    }

    private def primary(): Value = next() match {
      case "("                                => val v = or(); if (next() != ")") fail("unbalanced `(`"); v
      case "fromJson"                         => fromJson()
      case s"matrix.$name"                    => ctx.matrix.get(name).fold(Null)(Str(_))
      case lit if lit.startsWith("'")         => Str(lit.drop(1).dropRight(1).replace("''", "'"))
      case "github.server_url"                => Str(ctx.server)
      case "github.ref"                       => Str(ctx.ref)
      case s"inputs.$name"                    => ctx.inputs.get(name).fold(Null)(Str(_))
      case s"vars.$name"                      => Str(ctx.vars.getOrElse(name, ""))
      case s"needs.$job.outputs.$name"        => ctx.outputs.get(s"$job.$name").fold(Null)(Str(_))
      case b @ ("true" | "false")             => Bool(b == "true")
      case other                              => fail(s"`$other` is outside the subset")
    }
  }
}
