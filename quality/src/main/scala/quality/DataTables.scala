package quality

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.meta.*
import scala.meta.inputs.Input
import scala.meta.parsers.{Parse, Parsed}

/**
 * THE DATA TABLES (CLAUDE.md, "Code quality ratchets"; test/resources/quality-data-tables.tsv): the hand-written tables
 * kept as Scala literals (the owner, 2026-11-22: never moved to a TSV to shrink them) that the length limits do not
 * hold. A table is a member `val` of an `object` whose right-hand side is DATA, read from the code: literals (strings,
 * interpolated strings, numbers), names and selections, applications of constructors and collections (`Seq(...)`,
 * `Map(...)`, `Placeholder(...)`), tuples and the operators that build them (`->`, `+`, `++`). A `def`, a branch
 * (`if`, `match`, a partial function, `try`), a loop, a lambda or a block anywhere in it makes it no table: logic can't
 * hide in one. Each table must be declared (a row of the tsv, its why linted by scripts/ci/quality_ratchet.py); a
 * declared table read as data is measured by [[QualityReport]] as no method, and its lines are not its source's.
 */
object DataTables {

  /** The declared table file, under the repository root. */
  val DeclaredPath: String = "test/resources/quality-data-tables.tsv"

  /** A declared table as read from its source: its lines, or where it stops being data (`offence`, a 1-based line;
   *  `found` false when the source holds no member val of an object by its key). */
  final case class Verdict(key: String, startLine: Int, endLine: Int, offence: Option[Int], found: Boolean = true) {
    def isData: Boolean = found && offence.isEmpty
    def lines: Int = endLine - startLine + 1
  }

  /** The declared table keys (`<path>:<Owner.member>`) of the tsv under `root`; none when it is absent. */
  def declared(root: Path): Set[String] = {
    val file = root.resolve(DeclaredPath)
    if (!Files.isRegularFile(file)) Set.empty
    else new String(Files.readAllBytes(file), StandardCharsets.UTF_8).linesIterator
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map(_.split("\t")).collect { case cols if cols.length >= 2 && cols(0) == "table" => cols(1) }.toSet
  }

  /** The verdict of each key of `keys` naming a member val of an object in the source at `path` (a key no member
   *  answers has none: [[QualityReport.dataTables]] says it is not found). */
  def of(path: String, text: String, keys: Set[String]): Seq[Verdict] = {
    val wanted = keys.filter(_.startsWith(s"$path:"))
    if (wanted.isEmpty) Nil
    else Parse.parseSource(Input.VirtualFile(path, text), dialects.Scala3) match {
      case Parsed.Success(source) => members(source).collect {
          case (name, v) if wanted(s"$path:$name") => Verdict(s"$path:$name", v.pos.startLine + 1, v.pos.endLine + 1, offence(v.rhs))
        }
      case _: Parsed.Error => Nil
    }
  }

  /** Every single-name member val of an object, by its dotted owner and name. */
  private def members(tree: Tree): Seq[(String, Defn.Val)] = {
    def visit(t: Tree, owners: List[String]): Seq[(String, Defn.Val)] = t match {
      case o: Defn.Object => o.templ.body.stats.flatMap {
          case v: Defn.Val => v.pats match {
              case List(p: Pat.Var) => Seq(((o.name.value :: owners).reverse.mkString(".") + "." + p.name.value) -> v)
              case _                => Nil
            }
          case other => visit(other, o.name.value :: owners)
        }
      case c: Defn.Class => c.children.flatMap(visit(_, c.name.value :: owners))
      case c: Defn.Trait => c.children.flatMap(visit(_, c.name.value :: owners))
      case other         => other.children.flatMap(visit(_, owners))
    }
    visit(tree, Nil)
  }

  /** The first line (1-based) of `rhs` that is no data; None when all of it is. */
  def offence(rhs: Tree): Option[Int] = rhs match {
    case _: Lit | _: Term.Name | _: Name | _: Type | _: Term.ArgClause | _: Type.ArgClause => children(rhs)
    case _: Term.Select | _: Term.Apply | _: Term.ApplyType | _: Term.Tuple | _: Term.Interpolate          => children(rhs)
    case _: Term.Assign | _: Term.Repeated                                                             => children(rhs)
    case op: Term.ApplyInfix if DataOperators(op.op.value)                                             => children(rhs)
    case _                                                                                             => Some(rhs.pos.startLine + 1)
  }

  /** The operators that build data: a pair, a concatenation of strings or collections. */
  private val DataOperators = Set("->", "+", "++", ":+", "+:")

  private def children(t: Tree): Option[Int] = t.children.iterator.map(offence).collectFirst { case Some(line) => line }
}
