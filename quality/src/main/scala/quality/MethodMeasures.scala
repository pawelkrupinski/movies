package quality

import scala.meta.*
import scala.meta.classifiers.XtensionClassifiable
import scala.meta.inputs.Input
import scala.meta.parsers.{Parse, Parsed}
import scala.meta.tokens.Token

/**
 * EVERY METHOD'S MEASURES, read from a Scala source with scalameta: its COGNITIVE complexity (SonarSource's
 * definition, "Cognitive Complexity", G. Ann Campbell, 2017), its CYCLOMATIC complexity (McCabe), the INNER DEFS it
 * holds (CLAUDE.md: "refactor past two inner methods") and its length in lines.
 *
 * A method is a `def` with a body that no other `def` encloses. What a method holds -- its nested defs, its lambdas,
 * the members of a class it builds -- is folded into it, as Sonar folds nested functions and lambdas: they add NESTING,
 * not methods of their own.
 *
 * Cognitive complexity, Sonar's three rules in Scala's terms:
 *  - a STRUCTURAL increment, +1 plus the nesting level, for each `if` (not an `else if`), `match`, partial-function
 *    literal (`{ case ... }`, a match by another name), `catch`, `while`, `do`, and `for` (with or without `yield`);
 *  - a HYBRID increment, +1 with no nesting, for each `else if` and `else`;
 *  - a FUNDAMENTAL increment, +1, for each SEQUENCE of like boolean operators: `a && b && c` is one, `a && b || c` two;
 *  - NESTING rises inside the branches of every structure above, and inside each lambda and nested def (which add
 *    no increment of their own).
 * Sonar's recursion and labelled-jump increments are not counted: Scala has no `goto` or labelled `continue`, and a
 * call graph is out of a parser's reach.
 *
 * Cyclomatic complexity: 1, plus one for each `if`, loop, `for`, `case` of a match, partial function or catch (a last
 * unguarded `case _ =>` is the default branch and adds none, as Java's `default:`), `case` or `for` guard, and `&&` /
 * `||`.
 */
object MethodMeasures {

  /** One method: where it is (`owner` its enclosing classes and objects, dotted), and what it measures. */
  final case class Method(owner: String, name: String, startLine: Int, endLine: Int, cognitive: Int, cyclomatic: Int, innerDefs: Int) {
    def qualifiedName: String = if (owner.isEmpty) name else s"$owner.$name"
    def lines: Int = endLine - startLine + 1
  }

  /** A source's methods, or why it could not be parsed. */
  def of(path: String, text: String): Either[String, Seq[Method]] =
    Parse.parseSource(Input.VirtualFile(path, text), dialects.Scala3) match {
      case Parsed.Success(source) => Right(methods(source))
      case error: Parsed.Error    => Left(s"$path:${error.pos.startLine + 1}: ${error.message}")
    }

  /** The methods of a parsed tree, in source order. */
  def methods(tree: Tree): Seq[Method] = {
    val found = Seq.newBuilder[Method]
    def visit(t: Tree, owners: List[String]): Unit = t match {
      case d: Defn.Def => found += measure(d, owners.reverse.mkString("."))
      case c: Defn.Class  => c.children.foreach(visit(_, c.name.value :: owners))
      case c: Defn.Trait  => c.children.foreach(visit(_, c.name.value :: owners))
      case c: Defn.Object => c.children.foreach(visit(_, c.name.value :: owners))
      case c: Defn.Enum   => c.children.foreach(visit(_, c.name.value :: owners))
      case g: Defn.Given  => g.children.foreach(visit(_, (if (g.name.value.isEmpty) "given" else g.name.value) :: owners))
      case p: Pkg.Object  => p.children.foreach(visit(_, p.name.value :: owners))
      case other          => other.children.foreach(visit(_, owners))
    }
    visit(tree, Nil)
    found.result()
  }

  private def measure(d: Defn.Def, owner: String): Method = {
    val score = new Score
    score.walk(d.body, 0)
    Method(owner, d.name.value, d.pos.startLine + 1, d.pos.endLine + 1, score.cognitive, 1 + score.cyclomatic, score.innerDefs)
  }

  private val LogicalOperators = Set("&&", "||")

  /** The walk over one method's body: its three counts. */
  private final class Score {
    var cognitive = 0
    var cyclomatic = 0
    var innerDefs = 0
    private var templateDepth = 0

    def walk(t: Tree, nesting: Int): Unit = t match {
      case i: Term.If => ifChain(i, nesting, elseIf = false)
      case m: Term.Match =>
        structural(nesting)
        walk(m.expr, nesting)
        cases(m.casesBlock.cases, nesting + 1)
      case pf: Term.PartialFunction =>
        structural(nesting)
        cases(pf.cases, nesting + 1)
      case tr: Term.Try =>
        walk(tr.expr, nesting)
        tr.catchClause.foreach { block => structural(nesting); cases(block.cases, nesting + 1) }
        tr.finallyp.foreach(walk(_, nesting))
      case tr: Term.TryWithHandler =>
        walk(tr.expr, nesting)
        structural(nesting)
        walk(tr.catchp, nesting + 1)
        tr.finallyp.foreach(walk(_, nesting))
      case w: Term.While =>
        loop(nesting)
        walk(w.expr, nesting)
        walk(w.body, nesting + 1)
      case d: Term.Do =>
        loop(nesting)
        walk(d.body, nesting + 1)
        walk(d.expr, nesting)
      case f: Term.For =>
        loop(nesting)
        f.enumsBlock.enums.foreach(walk(_, nesting))
        walk(f.body, nesting + 1)
      case f: Term.ForYield =>
        loop(nesting)
        f.enumsBlock.enums.foreach(walk(_, nesting))
        walk(f.body, nesting + 1)
      case g: Enumerator.Guard =>
        cyclomatic += 1
        walk(g.cond, nesting)
      case op: Term.ApplyInfix if LogicalOperators(op.op.value) => logical(op, nesting)
      case f: Term.Function           => walk(f.body, nesting + 1)
      case f: Term.ContextFunction    => walk(f.body, nesting + 1)
      case f: Term.PolyFunction       => walk(f.body, nesting + 1)
      case f: Term.AnonymousFunction  => walk(f.body, nesting + 1)
      case d: Defn.Def =>
        if (templateDepth == 0) innerDefs += 1
        walk(d.body, nesting + 1)
      case tpl: Template =>
        templateDepth += 1
        tpl.children.foreach(walk(_, nesting))
        templateDepth -= 1
      case other => other.children.foreach(walk(_, nesting))
    }

    private def structural(nesting: Int): Unit = cognitive += 1 + nesting
    private def loop(nesting: Int): Unit = { structural(nesting); cyclomatic += 1 }

    private def ifChain(i: Term.If, nesting: Int, elseIf: Boolean): Unit = {
      cognitive += (if (elseIf) 1 else 1 + nesting)
      cyclomatic += 1
      walk(i.cond, nesting)
      walk(i.thenp, nesting + 1)
      i.elsep match {
        case next: Term.If => ifChain(next, nesting, elseIf = true)
        case other if hasElse(i) =>
          cognitive += 1
          walk(other, nesting + 1)
        case _ => ()
      }
    }

    /** Whether the `if` has an `else` in the text (scalameta gives one without it a synthetic `()`). */
    private def hasElse(i: Term.If): Boolean = i.tokens.exists(t => t.is[Token.KwElse] && t.start >= i.thenp.pos.end)

    private def cases(all: Seq[Case], nesting: Int): Unit = all.zipWithIndex.foreach { case (c, index) =>
      val isDefault = index == all.size - 1 && c.cond.isEmpty && c.pat.is[Pat.Wildcard]
      if (!isDefault) cyclomatic += 1
      c.cond.foreach { guard => cyclomatic += 1; walk(guard, nesting) }
      walk(c.body, nesting)
    }

    /** A boolean expression: +1 for each sequence of like operators, read left to right; its operands walked. */
    private def logical(root: Term.ApplyInfix, nesting: Int): Unit = {
      val operators = Seq.newBuilder[String]
      def flatten(t: Term): Unit = t match {
        case op: Term.ApplyInfix if LogicalOperators(op.op.value) && op.argClause.values.size == 1 =>
          flatten(op.lhs)
          operators += op.op.value
          flatten(op.argClause.values.head)
        case operand => walk(operand, nesting)
      }
      flatten(root)
      val sequence = operators.result()
      cyclomatic += sequence.size
      cognitive += sequence.indices.count(i => i == 0 || sequence(i) != sequence(i - 1))
    }
  }
}
