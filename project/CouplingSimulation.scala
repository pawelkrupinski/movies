package kinowo.build

/**
 * A MOVE'S COUPLING, BEFORE THE MOVE (scripts/coupling-report --simulate <moves.tsv>, `sbt couplingSimulate`): the
 * coupling report's own graph with classes moved to other packages -- or folded into another class, as its nested type
 * -- and layer-table rows added, measured by the report's own functions: each package's types, A, Ca, Ce, I and D,
 * the baseline rows that change (a cycle edge, a layer or rank violation, a Ce, a distance, a fan-out), and every
 * affected class's held fan-out. No code changes: a split's numbers are known before its first edit.
 *
 * The moves file's rows (`#` lines and blank ones comments):
 *   <class or pattern><TAB><package>        the class (every class a `*` pattern names) moves to that package
 *   <class><TAB>into <class>                the class folds into the other one: its uses and its users become that
 *                                           class's, so its fan-out is the UNION of the two (the split rule's union)
 *   layer|rank|allow<TAB>...                a row of test/resources/coupling-layers.tsv's shape, added (a `rank` or
 *                                           `layer` row for a package or prefix the table names replaces its row)
 * A moved class keeps its exemption rows (renamed with it).
 *
 * Compiled twice, as Coupling.scala is: into the build (Scala 2.12) and into common's tests (Scala 3, CouplingSpec).
 */
object CouplingSimulation {
  import Coupling.{Graph, Layers}

  /** The moves file: each class's new full name (a move or a fold: the class it folds into), and the layer rows added. */
  final case class Moves(renames: Map[String, String], folds: Set[String], layerRows: Seq[String])

  object Moves {
    def parse(lines: Seq[String], g: Graph): Moves = {
      val rows = lines.zipWithIndex.filterNot { case (l, _) => l.trim.isEmpty || l.startsWith("#") }.map { case (l, i) => (l.split("\t", -1).toSeq, i + 1) }
      val layerRows = rows.collect { case (cols, _) if Set("layer", "rank", "allow")(cols.head) => cols.mkString("\t") }
      val moves = rows.filterNot { case (cols, _) => Set("layer", "rank", "allow")(cols.head) }.flatMap { case (cols, n) =>
        if (cols.size != 2 || cols(0).trim.isEmpty || cols(1).trim.isEmpty)
          throw new IllegalArgumentException(s"moves line $n: not `<class><TAB><package>`, `<class><TAB>into <class>` or a layer-table row")
        val (pattern, to) = (cols(0).trim, cols(1).trim)
        val named = g.types.map(_.name).filter(c => c == pattern || (pattern.contains("*") && Coupling.Exemptions.matches(pattern, c)))
        if (named.isEmpty) throw new IllegalArgumentException(s"moves line $n: no production class is `$pattern`")
        if (to.startsWith("into ")) {
          val host = to.stripPrefix("into ").trim
          if (!g.byName.contains(host)) throw new IllegalArgumentException(s"moves line $n: no production class is `$host` to fold into")
          named.map(c => (c, host, true))
        } else named.map(c => (c, to + "." + c.substring(c.lastIndexOf('.') + 1), false))
      }
      val clash = moves.groupBy(_._1).collect { case (c, ms) if ms.map(_._2).distinct.size > 1 => c }
      if (clash.nonEmpty) throw new IllegalArgumentException(s"moves: ${clash.toSeq.sorted.mkString(", ")} moved twice")
      Moves(moves.map(m => m._1 -> m._2).toMap, moves.collect { case (c, _, true) => c }.toSet, layerRows)
    }
  }

  /** `name` -- a top-level class, an object of one (`X$`) or a type nested in one (`X.Inner`) -- under its moved class's
   *  new name (a fold's: nested in the class it folds into, `Host.X`), unchanged when its class does not move. */
  def renamed(moves: Moves)(name: String): String =
    moves.renames.keys.filter(c => name == c || name.startsWith(c + ".") || name.startsWith(c + "$")).toSeq.sortBy(-_.length).headOption
      .fold(name) { c =>
        val to = if (moves.folds(c)) renamed(moves)(moves.renames(c)) + "." + c.substring(c.lastIndexOf('.') + 1) else moves.renames(c)
        to + name.substring(c.length)
      }

  /** A top-level class's name after the moves: a folded one's is its host's. */
  def topOf(moves: Moves)(name: String): String = if (moves.folds(name)) renamed(moves)(moves.renames(name)) else renamed(moves)(name)

  /** The graph after the moves: every name renamed, a folded class's edges its host's (its own type gone: it is nested). */
  def moved(g: Graph, moves: Moves): Graph = {
    val r: String => String = renamed(moves)
    val top: String => String = topOf(moves)
    def edges(es: Set[(String, String)]): Set[(String, String)] = es.map { case (a, b) => (top(a), top(b)) }.filter { case (a, b) => a != b }
    Graph(g.types.filterNot(t => moves.folds(t.name)).map(t => t.copy(name = r(t.name))), edges(g.edges), g.data.map(r),
      g.collaboratorWhy.map { case (k, v) => r(k) -> v }, g.coupledEdges.map(edges),
      g.supertypes.toSeq.filterNot(e => moves.folds(e._1)).map { case (k, v) => top(k) -> (v.map(top) - top(k)) }.toMap, g.units)
  }

  /** The layer table with `rows` added: a `rank` row replaces the row ranking its package, a `layer` row the row of its
   *  prefix. */
  def layered(table: Seq[String], rows: Seq[String]): Layers = {
    def key(line: String): Option[(String, String)] = line.split("\t", -1).toSeq match {
      case Seq(kind, _, name, _) if kind == "rank" || kind == "layer" => Some((kind, name))
      case _ => None
    }
    val replaced = rows.flatMap(key).toSet
    Layers.parse(table.filterNot(l => key(l).exists(replaced)) ++ rows)
  }

  /** The exemptions with every moved class's rows renamed. */
  def exempted(e: Coupling.Exemptions, moves: Moves): Coupling.Exemptions =
    Coupling.Exemptions(e.rows.map(row => if (row.pattern.contains("*")) row else row.copy(pattern = renamed(moves)(row.pattern))))

  /** The markdown report of `moves` over `g` (its layer table `table`, its exemptions `e`). */
  def report(g: Graph, table: Seq[String], e: Coupling.Exemptions, moves: Moves): String = {
    val before = g.copy(units = Layers.parse(table).units)
    val layersAfter = layered(table, moves.layerRows)
    val after = moved(g, moves).copy(units = layersAfter.units)
    val eAfter = exempted(e, moves)
    val (pb, pa) = (Coupling.packages(before).map(p => p.name -> p).toMap, Coupling.packages(after).map(p => p.name -> p).toMap)
    val touched = (moves.renames.keys.map(before.packageOf) ++ moves.renames.keys.map(c => after.packageOf(renamed(moves)(c))) ++
      moves.layerRows.flatMap(_.split("\t", -1).lift(2))).toSet
    // a package whose numbers move: one a class left or entered, or any whose Ca, Ce or types changed
    val changed = (pb.keySet ++ pa.keySet).toSeq.sorted.filter(p => touched(p) || pb.get(p).map(x => (x.types, x.ca, x.ce, x.abstracts)) != pa.get(p).map(x => (x.types, x.ca, x.ce, x.abstracts)))
    def cell(p: Option[Coupling.PackageStats])(f: Coupling.PackageStats => String): String = p.fold("-")(f)
    def pair(p: String)(f: Coupling.PackageStats => String): String = {
      val (b, a) = (cell(pb.get(p))(f), cell(pa.get(p))(f))
      if (b == a) a else s"$b -> $a"
    }
    val rowsBefore = Coupling.baselineRows(before, layers = Some(Layers.parse(table)), exemptions = e).filterNot(_.startsWith("limit\t"))
    val rowsAfter = Coupling.baselineRows(after, layers = Some(layersAfter), exemptions = eAfter).filterNot(_.startsWith("limit\t"))
    def keyed(rows: Seq[String]): Map[(String, String), String] = rows.map(_.split("\t", -1)).map(c => (c(0), c(1)) -> c(2)).toMap
    val (kb, ka) = (keyed(rowsBefore), keyed(rowsAfter))
    val rowChanges = (kb.keySet ++ ka.keySet).toSeq.sorted.collect {
      case k if kb.get(k) != ka.get(k) => s"- `${k._1}` `${k._2}`: ${kb.getOrElse(k, "(none)")} -> ${ka.getOrElse(k, "(none)")}"
    }
    val (fb, fa) = (Coupling.heldFanOut(before, e), Coupling.heldFanOut(after, eAfter))
    // the moved and folded classes, the classes they fold into, and any other class whose held fan-out moves
    val affected = (moves.renames.keys.toSeq ++ moves.folds.map(moves.renames) ++
      fb.keys.filter(c => fa.get(topOf(moves)(c)) != fb.get(c))).distinct.sorted
    val out = Seq.newBuilder[String]
    out += "# Coupling simulation"
    out += ""
    out += s"${moves.renames.size - moves.folds.size} classes moved, ${moves.folds.size} folded into another, ${moves.layerRows.size} layer-table rows added. " +
      "Measured by the coupling report's own functions over its graph; no code changed."
    out += ""
    out += "## Packages that change"
    out += ""
    out += "| package | types | abstract | Ca | Ce | Ce pkgs | I | A | D |"
    out += "|---|---|---|---|---|---|---|---|---|"
    changed.foreach { p =>
      out += s"| `$p` | ${pair(p)(_.types.toString)} | ${pair(p)(_.abstracts.toString)} | ${pair(p)(_.ca.toString)} | ${pair(p)(_.ce.toString)} | " +
        s"${pair(p)(_.cePackages.toString)} | ${pair(p)(x => Coupling.f2(x.instability))} | ${pair(p)(x => Coupling.f2(x.abstractness))} | ${pair(p)(x => Coupling.f2(x.distance))} |"
    }
    out += ""
    out += s"## Baseline rows that change (${rowChanges.size})"
    out += ""
    out += "The ratchet's rows (`kind key value`, limits aside) before -> after: a new `cycle-edge`, `layer-violation` or " +
      "`rank-violation` row is refused outright; a `ce` rise needs its trailer; a `distance` past its frozen row and ceiling, or a new one, is refused."
    out += ""
    if (rowChanges.isEmpty) out += "None."
    out ++= rowChanges
    out += ""
    val cyclesAfter = Coupling.cycles(after)
    out += s"## Package cycles after (${cyclesAfter.size})"
    out += ""
    if (cyclesAfter.isEmpty) out += "None."
    cyclesAfter.foreach { c =>
      out += s"- ${c.packages.map(p => s"`$p`").mkString(", ")}"
      c.edges.foreach { case ((a, b), refs) => out += s"  - `$a -> $b`: ${refs.size} refs (${refs.take(5).map { case (x, y) => s"`$x` -> `$y`" }.mkString(", ")}${if (refs.size > 5) ", ..." else ""})" }
    }
    out += ""
    out += "## Affected classes' fan-out (held: collaborators, a contract one, the union of a fold)"
    out += ""
    out += "| class | after | fan-out | limit |"
    out += "|---|---|---|---|"
    affected.foreach { b =>
      val a = topOf(moves)(b)
      val (x, y) = (fb.get(b).fold("-")(_.toString), fa.get(a).fold("-")(_.toString))
      val shown = if (moves.folds(b)) s"(into `$a`)" else s"`$a`"
      out += s"| `$b` | $shown | ${if (x == y) y else s"$x -> $y"} | ${eAfter.fanOutLimit(a, Coupling.DefaultFanOutLimit).fold("exempt")(_.toString)} |"
    }
    out.result().mkString("\n") + "\n"
  }
}
