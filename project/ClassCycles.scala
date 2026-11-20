package kinowo.build

/**
 * THE CLASS CYCLES OF ONE PACKAGE (scripts/class-cycles, `sbt couplingClassCycles`): before a package is split, which of
 * its classes cannot be pulled apart -- the strongly connected components of the class graph inside it and its
 * sub-packages ([[Coupling.components]] over the coupling report's own graph, every known edge, a data type's too: the
 * package cycles a split would make read the same edges) -- and, for a proposed order of groups of its classes (bottom
 * up), every class edge pointing UP that order: what the split would have to cut first.
 *
 * Compiled twice, as Coupling.scala is: into the build (Scala 2.12) and into common's tests (Scala 3, CouplingSpec).
 */
object ClassCycles {

  /** The classes of `pkg` and of its sub-packages, and the known edges between two of them. */
  def within(g: Coupling.Graph, pkg: String): (Seq[String], Set[(String, String)]) = {
    def inside(name: String): Boolean = name.startsWith(pkg + ".")
    (g.types.map(_.name).filter(inside).sorted, g.known.filter { case (a, b) => inside(a) && inside(b) })
  }

  /** A proposed order over groups of a package's classes, bottom (first) up: `groups` in order, and each class's group --
   *  the first whose patterns match it (a class name, full or relative to the package, `*` any run of characters:
   *  `Listing,Evidence`, `*Measures`, `agreement.*`). A class no group matches stands outside the order. */
  final case class Order(groups: Seq[(String, Seq[String])]) {
    def groupOf(pkg: String, name: String): Option[String] =
      groups.find(_._2.exists(p => Coupling.Exemptions.matches(p, name) || Coupling.Exemptions.matches(pkg + "." + p, name))).map(_._1)
    def rankOf(group: String): Int = groups.indexWhere(_._1 == group)
  }

  object Order {
    /** `a<b<c`, each group a comma-separated list of patterns and named by them (`Listing,Evidence<*Measures<*`). */
    def inline(spec: String): Order = Order(spec.split('<').map(_.trim).filter(_.nonEmpty).map(g => g -> g.split(',').map(_.trim).filter(_.nonEmpty).toSeq).toSeq)

    /** A table of `<class><TAB><group>` rows (`#` lines and blank ones comments), the groups ordered bottom up by their
     *  number when every group is one, else by their first row. */
    def table(lines: Seq[String]): Order = {
      val rows = lines.filterNot(l => l.trim.isEmpty || l.startsWith("#")).map { l =>
        val cols = l.split("\t", -1)
        if (cols.length < 2 || cols(0).trim.isEmpty || cols(1).trim.isEmpty) throw new IllegalArgumentException(s"not `<class><TAB><group>`: $l")
        (cols(0).trim, cols(1).trim)
      }
      val names = rows.map(_._2).distinct
      val ordered = if (names.nonEmpty && names.forall(n => n.nonEmpty && n.forall(_.isDigit))) names.sortBy(_.toInt) else names
      Order(ordered.map(n => n -> rows.collect { case (c, `n`) => c }))
    }
  }

  /** Every class edge from a group to a higher one, by group pair (lower group first), each with its class references. */
  def upward(g: Coupling.Graph, pkg: String, order: Order): Seq[((String, String), Seq[(String, String)])] = {
    val (_, edges) = within(g, pkg)
    val group: Map[String, String] = edges.toSeq.flatMap { case (a, b) => Seq(a, b) }.distinct.flatMap(c => order.groupOf(pkg, c).map(c -> _)).toMap
    edges.toSeq.collect { case (a, b) if group.contains(a) && group.contains(b) && order.rankOf(group(b)) > order.rankOf(group(a)) =>
      (group(a), group(b)) -> (a, b)
    }.groupBy(_._1).toSeq.map { case (k, v) => k -> v.map(_._2).sorted }.sortBy { case ((a, b), _) => (order.rankOf(a), order.rankOf(b)) }
  }

  private def short(pkg: String)(name: String): String = name.stripPrefix(pkg + ".")

  /** The markdown report: the package's components, then (given an order) each group's classes and the upward edges. */
  def report(g: Coupling.Graph, pkg: String, order: Option[Order]): String = {
    val (nodes, edges) = within(g, pkg)
    val s: String => String = short(pkg)
    val sccs = Coupling.components(nodes, edges).sortBy(c => (-c.size, c.head))
    val out = Seq.newBuilder[String]
    out += s"# Class cycles in `$pkg`"
    out += ""
    out += s"${nodes.size} classes, ${edges.size} class->class edges between them (the coupling report's graph, every edge)."
    out += ""
    out += s"## Strongly connected components of 2+ classes (${sccs.size}${if (sccs.isEmpty) "" else s"; largest ${sccs.head.size}"})"
    out += ""
    if (sccs.isEmpty) out += "None: the classes can be ordered as they stand."
    sccs.foreach(c => out += s"- ${c.size} classes: ${c.map(s).mkString(" ")}")
    order.foreach { o =>
      val up = upward(g, pkg, o)
      out += ""
      out += s"## Upward edges under ${o.groups.map(_._1).mkString(" < ")} (${up.map(_._2.size).sum})"
      out += ""
      o.groups.foreach { case (name, _) =>
        val members = nodes.filter(c => o.groupOf(pkg, c).contains(name))
        out += s"- group `$name` (${members.size}): ${members.map(s).mkString(" ")}"
      }
      out += s"- outside the order (${nodes.count(c => o.groupOf(pkg, c).isEmpty)}): ${nodes.filter(c => o.groupOf(pkg, c).isEmpty).map(s).mkString(" ")}"
      out += ""
      if (up.isEmpty) out += "None: the order holds."
      up.foreach { case ((a, b), refs) =>
        out += s"### `$a` -> `$b`: ${refs.size} references"
        refs.foreach { case (x, y) => out += s"- ${s(x)} -> ${s(y)}" }
      }
    }
    out.result().mkString("\n") + "\n"
  }
}
