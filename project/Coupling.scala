package kinowo.build

/**
 * COUPLING, MEASURED FROM THE COMPILER'S RECORD (`sbt couplingReport`, scripts/coupling-report): the production code's
 * class graph -- every top-level class of a module's `src/main`, and the class->class dependencies Zinc records between
 * them (a name used, a type inferred, a parent, an implicit; `project/CouplingAnalysis.scala` reads them) -- folded into
 * the numbers the coupling ratchet (scripts/ci/coupling_ratchet.py) holds every change to:
 *
 *  - MODULES: the cross-module edges, weighted by the class->class references across them;
 *  - PACKAGES (a class's package cut to its top two segments: `services.identity`, `models`): afferent coupling Ca (the
 *    classes outside the package depending on one inside), efferent coupling Ce (the classes outside it that its classes
 *    depend on; and, held by the ratchet, the outside PACKAGES they depend on -- splitting a trait into roles, which raises
 *    the class count without a new dependency, leaves it), instability I = Ce / (Ca + Ce), abstractness A (traits and abstract classes / all its types) and the
 *    distance from the main sequence D = |A + I - 1|; and the package CYCLES, the strongly connected components of the
 *    package graph, with every package edge inside one (its weight, and the class references that make it);
 *  - CLASSES: fan-in and fan-out (distinct production classes referencing it / referenced by it);
 *  - SPLIT PACKAGES: a (full) package whose classes live in two or more modules.
 *
 * A nested class counts as its top-level class (`Outer.Inner` is `Outer`): a class's own pieces are no coupling.
 *
 * Compiled twice: into the build (Scala 2.12) and into common's tests (Scala 3, CouplingSpec) -- so it is written in the
 * syntax both accept.
 */
object Coupling {

  /** A top-level production class: its name, its module, whether it is a trait or an abstract class. */
  final case class Type(name: String, module: String, isAbstract: Boolean)

  /** The graph: every type, and the dependencies between them (`from` uses `to`; both types' names). */
  final case class Graph(types: Seq[Type], edges: Set[(String, String)]) {
    val byName: Map[String, Type] = types.map(t => t.name -> t).toMap
    /** The edges between two distinct known types. */
    val known: Set[(String, String)] = edges.filter { case (from, to) => from != to && byName.contains(from) && byName.contains(to) }
  }

  /** A class's full package: its name less the last segment (`<root>` for none). */
  def fullPackageOf(name: String): String = { val i = name.lastIndexOf('.'); if (i < 0) "<root>" else name.substring(0, i) }

  /** A class's package as the report groups it: the full package cut to its top two segments. */
  def packageOf(name: String): String = {
    val full = fullPackageOf(name)
    if (full == "<root>") full else full.split('.').take(2).mkString(".")
  }

  /** A class's name without the `$` Zinc records a member of an object under (`clients.TmdbClient$.Page` is
   *  `clients.TmdbClient.Page`): an object and its class are one type. */
  def normalize(name: String): String = name.split('.').map(_.stripSuffix("$")).mkString(".")

  /** The top-level class of each name in `names` (one source's classes, normalized): a name whose prefix is another of
   *  them is nested. */
  def topLevel(names: Set[String]): Map[String, String] = {
    def outer(name: String): String = {
      val i = name.lastIndexOf('.')
      if (i < 0) name else { val prefix = name.substring(0, i); if (names(prefix)) outer(prefix) else name }
    }
    names.map(n => n -> outer(n)).toMap
  }

  // ── modules ──

  /** Cross-module edges: (from module, to module) -> the class->class references across them. */
  def moduleEdges(g: Graph): Map[(String, String), Int] =
    g.known.toSeq.map { case (from, to) => (g.byName(from).module, g.byName(to).module) }.filter { case (a, b) => a != b }
      .groupBy(identity).map { case (k, v) => k -> v.size }

  // ── packages ──

  final case class PackageStats(name: String, types: Int, abstracts: Int, ca: Int, ce: Int, cePackages: Int, modules: Seq[String]) {
    def instability: Double = if (ca + ce == 0) 0.0 else ce.toDouble / (ca + ce)
    def abstractness: Double = if (types == 0) 0.0 else abstracts.toDouble / types
    def distance: Double = math.abs(abstractness + instability - 1)
  }

  def packages(g: Graph): Seq[PackageStats] = {
    val pkg = g.types.map(t => t.name -> packageOf(t.name)).toMap
    val cross = g.known.toSeq.filter { case (from, to) => pkg(from) != pkg(to) }
    val ce = cross.groupBy { case (from, _) => pkg(from) }.map { case (p, es) => p -> es.map(_._2).toSet.size }
    val ca = cross.groupBy { case (_, to) => pkg(to) }.map { case (p, es) => p -> es.map(_._1).toSet.size }
    val cePackages = cross.groupBy { case (from, _) => pkg(from) }.map { case (p, es) => p -> es.map(e => pkg(e._2)).toSet.size }
    g.types.groupBy(t => pkg(t.name)).toSeq.map { case (p, ts) =>
      PackageStats(p, ts.size, ts.count(_.isAbstract), ca.getOrElse(p, 0), ce.getOrElse(p, 0), cePackages.getOrElse(p, 0), ts.map(_.module).distinct.sorted)
    }.sortBy(_.name)
  }

  /** Package edges: (from package, to package) -> the class->class references making it. */
  def packageEdges(g: Graph): Map[(String, String), Seq[(String, String)]] =
    g.known.toSeq.map { case e @ (from, to) => (packageOf(from), packageOf(to)) -> e }.filter { case ((a, b), _) => a != b }
      .groupBy(_._1).map { case (k, v) => k -> v.map(_._2).sorted }

  /** Every class->class dependency, one `<from package>-><to package><TAB><from class>-><to class>` line each (a
   *  package's own, `p->p`, too: what a move out of it must take along), sorted: the evidence behind the rows
   *  (scripts/coupling-report --out keeps it as edges.tsv; the ratchet names a refused edge's references from it). */
  def edgeRows(g: Graph): Seq[String] =
    g.known.toSeq.map { case (x, y) => s"${packageOf(x)}->${packageOf(y)}\t$x->$y" }.sorted

  /** The strongly connected components of a directed graph with two or more nodes (Tarjan), each sorted, sorted. */
  def components(nodes: Seq[String], edges: Set[(String, String)]): Seq[Seq[String]] = {
    val next = edges.toSeq.groupBy(_._1).map { case (k, v) => k -> v.map(_._2).sorted }
    var index = 0
    val indexOf = scala.collection.mutable.Map.empty[String, Int]
    val low = scala.collection.mutable.Map.empty[String, Int]
    val stack = scala.collection.mutable.ArrayBuffer.empty[String]
    val onStack = scala.collection.mutable.Set.empty[String]
    val out = Seq.newBuilder[Seq[String]]
    def visit(v: String): Unit = {
      indexOf(v) = index; low(v) = index; index += 1
      stack += v; onStack += v
      next.getOrElse(v, Nil).foreach { w =>
        if (!indexOf.contains(w)) { visit(w); low(v) = math.min(low(v), low(w)) }
        else if (onStack(w)) low(v) = math.min(low(v), indexOf(w))
      }
      if (low(v) == indexOf(v)) {
        val members = Seq.newBuilder[String]
        var w = ""
        while (w != v) { w = stack.remove(stack.size - 1); onStack -= w; members += w }
        val all = members.result()
        if (all.size > 1) out += all.sorted
      }
    }
    (nodes ++ edges.toSeq.flatMap { case (a, b) => Seq(a, b) }).distinct.sorted.foreach(v => if (!indexOf.contains(v)) visit(v))
    out.result().sortBy(_.mkString(" "))
  }

  /** A package cycle: its packages, and every package edge inside it with the class references making it. */
  final case class Cycle(packages: Seq[String], edges: Seq[((String, String), Seq[(String, String)])])

  def cycles(g: Graph): Seq[Cycle] = {
    val edges = packageEdges(g)
    components(packages(g).map(_.name), edges.keySet).map { members =>
      val in = members.toSet
      Cycle(members, edges.toSeq.filter { case ((a, b), _) => in(a) && in(b) }.sortBy { case ((a, b), refs) => (refs.size, a, b) })
    }
  }

  // ── layers ──

  /** THE LAYERING (test/resources/coupling-layers.tsv, [[Layers.parse]]): every package's layer, bottom (0) up, the
   *  order of the packages inside a layer (their ranks, bottom (0) up), and the few package edges allowed to point up
   *  anyway, each with its why. A package may depend only on its own layer or one below, and inside an ORDERED layer
   *  (one any `rank` row names) only on its own rank or a lower one; a package no row places is a violation too, so a
   *  new package takes a place before it lands. With the package graph acyclic, the ranks are its order made explicit:
   *  an edge that would close a cycle is an upward edge first, named with the direction allowed. */
  final case class Layers(rows: Seq[(String, Int)], allowed: Set[(String, String)], ranks: Map[String, Int] = Map.empty) {
    /** A package's layer: the longest `prefix` row naming it (`services` names `services.movies`), None for none. */
    def layerOf(pkg: String): Option[Int] =
      rows.filter { case (prefix, _) => pkg == prefix || pkg.startsWith(prefix + ".") }.sortBy(-_._1.length).headOption.map(_._2)
    /** A package's rank inside its layer: its own `rank` row (the package itself, never a prefix), None for none. */
    def rankOf(pkg: String): Option[Int] = ranks.get(pkg)
    /** Whether `layer` orders its packages: some `rank` row names a package of it. */
    def ordered(layer: Int): Boolean = ranks.keys.exists(p => layerOf(p).contains(layer))
  }

  object Layers {
    /** The table's rows: `layer<TAB><n><TAB><package prefix><TAB><why>`, `rank<TAB><n><TAB><package><TAB><why>` and
     *  `allow<TAB><from>-><to><TAB><why>`; `#` lines and blank lines are comments. A row of any other shape, an `allow`
     *  without its why, a package ranked twice or a ranked package no `layer` row places is an error. */
    def parse(lines: Seq[String]): Layers = {
      val layers = Seq.newBuilder[(String, Int)]
      val allowed = Set.newBuilder[(String, String)]
      val ranks = scala.collection.mutable.LinkedHashMap.empty[String, Int]
      lines.zipWithIndex.foreach { case (line, index) =>
        val n: Int = index + 1
        val cols: Array[String] = line.split("\t", -1)
        def bad(why: String): Nothing = throw new IllegalArgumentException(s"coupling-layers.tsv line $n: $why")
        if (line.trim.isEmpty || line.startsWith("#")) ()
        else if (cols(0) == "layer" && cols.length == 4) {
          if (cols(1).isEmpty || !cols(1).forall(_.isDigit)) bad(s"layer `${cols(1)}` is not a number")
          layers += ((cols(2), cols(1).toInt))
        } else if (cols(0) == "rank" && cols.length == 4) {
          if (cols(1).isEmpty || !cols(1).forall(_.isDigit)) bad(s"rank `${cols(1)}` is not a number")
          if (cols(3).trim.isEmpty) bad(s"the rank of ${cols(2)} has no why")
          if (ranks.contains(cols(2))) bad(s"${cols(2)} is ranked twice")
          ranks(cols(2)) = cols(1).toInt
        } else if (cols(0) == "allow" && cols.length == 3) {
          if (cols(2).trim.isEmpty) bad(s"the allowed edge ${cols(1)} has no why")
          val ends: Array[String] = cols(1).split("->", -1)
          if (ends.length != 2 || ends(0).isEmpty || ends(1).isEmpty) bad(s"`${cols(1)}` is not <from>-><to>")
          allowed += ((ends(0), ends(1)))
        } else bad("not a `layer` or `allow` row")
      }
      val table = Layers(layers.result(), allowed.result(), ranks.toMap)
      ranks.keys.filter(p => table.layerOf(p).isEmpty).foreach(p => throw new IllegalArgumentException(s"coupling-layers.tsv: $p is ranked but no `layer` row places it"))
      table
    }
  }

  /** The package edges the layering forbids, with the class references making each: an edge into a higher layer that
   *  no `allow` row names, and (as `<package>->?`) every package no `layer` row names. */
  def layerViolations(g: Graph, layers: Layers): Seq[((String, String), Seq[(String, String)])] = {
    val unlayered: Seq[((String, String), Seq[(String, String)])] =
      packages(g).map(_.name).filter(p => layers.layerOf(p).isEmpty).map(p => ((p, "?"), Seq.empty[(String, String)]))
    def up(a: String, b: String): Boolean = {
      val from: Option[Int] = layers.layerOf(a)
      val to: Option[Int] = layers.layerOf(b)
      from.isDefined && to.isDefined && to.get > from.get && !layers.allowed((a, b))
    }
    val upward: Seq[((String, String), Seq[(String, String)])] = packageEdges(g).toSeq.filter { case ((a, b), _) => up(a, b) }
    (unlayered ++ upward).sortBy { case ((a, b), _) => (a, b) }
  }

  /** The package edges the order inside a layer forbids, with the class references making each: inside an ordered
   *  layer, an edge to a package of a higher rank that no `allow` row names, and (as `<package>->?`) every package of it
   *  no `rank` row names. */
  def rankViolations(g: Graph, layers: Layers): Seq[((String, String), Seq[(String, String)])] = {
    val unranked: Seq[((String, String), Seq[(String, String)])] = packages(g).map(_.name)
      .filter(p => layers.layerOf(p).exists(layers.ordered) && layers.rankOf(p).isEmpty).map(p => ((p, "?"), Seq.empty[(String, String)]))
    def up(a: String, b: String): Boolean = {
      val (la, lb, ra, rb) = (layers.layerOf(a), layers.layerOf(b), layers.rankOf(a), layers.rankOf(b))
      la.isDefined && la == lb && ra.isDefined && rb.isDefined && rb.get > ra.get && !layers.allowed((a, b))
    }
    val upward: Seq[((String, String), Seq[(String, String)])] = packageEdges(g).toSeq.filter { case ((a, b), _) => up(a, b) }
    (unranked ++ upward).sortBy { case ((a, b), _) => (a, b) }
  }

  // ── classes ──

  /** Per class: (fan-in, fan-out) over distinct production classes. */
  def fans(g: Graph): Map[String, (Int, Int)] = {
    val out = g.known.groupBy(_._1).map { case (k, v) => k -> v.size }
    val in  = g.known.groupBy(_._2).map { case (k, v) => k -> v.size }
    g.types.map(t => t.name -> ((in.getOrElse(t.name, 0), out.getOrElse(t.name, 0)))).toMap
  }

  /** Split packages: a full package whose classes live in 2+ modules -> (module, class count) per module. */
  def splitPackages(g: Graph): Seq[(String, Seq[(String, Int)])] =
    g.types.groupBy(t => fullPackageOf(t.name)).toSeq.map { case (p, ts) =>
      p -> ts.groupBy(_.module).toSeq.map { case (m, in) => m -> in.size }.sortBy(_._1)
    }.filter(_._2.size > 1).sortBy(_._1)

  // ── the baseline ──

  /** The fan-out at and above which a class has a row of its own (below it, a class is held to the limit). */
  val DefaultFanOutLimit = 30
  /** The outside packages a package with no row may depend on. */
  val DefaultNewPackageCeLimit = 8

  /** The ratchet's rows (TAB-separated `kind key value`, sorted): what scripts/ci/coupling_ratchet.py compares. */
  def baselineRows(g: Graph, fanOutLimit: Int = DefaultFanOutLimit, newPackageCeLimit: Int = DefaultNewPackageCeLimit,
                   layers: Option[Layers] = None): Seq[String] = {
    val limits = Seq(s"limit\tfanout\t$fanOutLimit", s"limit\tnew-package-ce\t$newPackageCeLimit")
    val modules = moduleEdges(g).toSeq.sorted.map { case ((a, b), n) => s"module-edge\t$a->$b\t$n" }
    val cycleEdges = cycles(g).flatMap(_.edges).map { case ((a, b), refs) => s"cycle-edge\t$a->$b\t${refs.size}" }.sorted
    val ce = packages(g).map(p => s"ce\t${p.name}\t${p.cePackages}")
    val fanout = fans(g).toSeq.collect { case (c, (_, out)) if out >= fanOutLimit => s"fanout\t$c\t$out" }.sorted
    val layerRows = layers.toSeq.flatMap(l => layerViolations(g, l)).map { case ((a, b), refs) => s"layer-violation\t$a->$b\t${refs.size}" }
    val rankRows = layers.toSeq.flatMap(l => rankViolations(g, l)).map { case ((a, b), refs) => s"rank-violation\t$a->$b\t${refs.size}" }
    limits ++ modules ++ cycleEdges ++ ce ++ fanout ++ layerRows ++ rankRows
  }

  val BaselineHeader: Seq[String] = Seq(
    "# THE COUPLING BASELINE (scripts/coupling-report --update writes it; scripts/ci/coupling_ratchet.py holds every push to it).",
    "# Production code only (*/src/main), from Zinc's class-dependency record (project/Coupling.scala). One row a fact,",
    "# `kind<TAB>key<TAB>value`:",
    "#   limit        fanout / new-package-ce: a class without a fanout row stays below the first, a package without a ce row",
    "#                at or below the second",
    "#   module-edge  <from>-><to>, the class references across it: a new pair is a new cross-module edge kind",
    "#   cycle-edge   <from>-><to>, a package edge inside a package cycle (a strongly connected component): never held here --",
    "#                the package graph has no cycle, and the ratchet fails on any such row, in the code or in this file",
    "#   ce           <package>, its efferent coupling in packages (distinct outside packages its classes use): it may not rise",
    "#   fanout       <class>, its fan-out (distinct production classes it uses), at or above the limit: it may not rise",
    "#   layer-violation <from>-><to>, a package edge into a higher layer (test/resources/coupling-layers.tsv): never",
    "#                held here -- the ratchet fails on any (cut it, or allow it in the layer table with its why)",
    "#   rank-violation <from>-><to>, inside an ordered layer an edge to a higher-ranked package, or <package>->? for a",
    "#                package of it with no `rank` row: never held here either -- the ratchet fails on any",
    "# A row added or raised lands only in a commit carrying `Coupling: <why>` (CLAUDE.md, Design rules); a fall is",
    "# recorded by --update with no trailer. Module-edge weights are information: only the pair is held.")

  // ── the human report ──

  private def f2(d: Double): String = { val r = math.round(d * 100); s"${r / 100}.${"%02d".format(r % 100)}" }
  private def table(header: Seq[String], rows: Seq[Seq[String]]): Seq[String] =
    Seq(header.mkString("| ", " | ", " |"), header.map(_ => "---").mkString("|", "|", "|")) ++ rows.map(_.mkString("| ", " | ", " |"))

  /** The markdown summary. `declared`: each module's direct Compile dependencies (build.sbt's dependsOn). */
  def report(g: Graph, declared: Map[String, Set[String]], top: Int = 20, layers: Option[Layers] = None): String = {
    val modules = g.types.map(_.module).distinct.sorted
    val medges = moduleEdges(g)
    val pkgs = packages(g)
    val cyc = cycles(g)
    val fan = fans(g)
    val out = Seq.newBuilder[String]
    out += "# Coupling report"
    out += ""
    out += s"Production code (`*/src/main`): ${g.types.size} top-level classes in ${pkgs.size} packages across ${modules.size} modules, " +
      s"${g.known.size} class->class dependencies (${medges.values.sum} of them across modules)."
    out += ""
    out += "## Modules"
    out += ""
    out += "Rows depend on columns; a cell is the class->class references across that edge."
    out += ""
    out ++= table("from \\ to" +: modules, modules.map(a => a +: modules.map(b => if (a == b) "." else medges.get((a, b)).map(_.toString).getOrElse(""))))
    val transitive = medges.keySet.toSeq.sorted.filterNot { case (a, b) => declared.getOrElse(a, Set.empty)(b) }
    val unused = declared.toSeq.flatMap { case (a, bs) => bs.toSeq.map(a -> _) }.filter { case (a, b) => modules.contains(a) && modules.contains(b) && !medges.contains((a, b)) }.sorted
    out += ""
    out += "Unexpected edges:"
    out += ""
    if (transitive.isEmpty && unused.isEmpty) out += "- none"
    transitive.foreach { case (a, b) => out += s"- `$a -> $b` (${medges((a, b))} refs) is used but not declared: it reaches `$b` only through another module's dependsOn" }
    unused.foreach { case (a, b) => out += s"- `$a -> $b` is declared (dependsOn) but no production class uses it" }
    out += ""
    out += "## Packages"
    out += ""
    out += "Ca: outside classes using the package; Ce: outside classes it uses (Ce pkgs: outside packages, what the ratchet holds); I = Ce/(Ca+Ce); A = abstract types / types; D = |A+I-1|. Sorted by D, then Ce."
    out += ""
    out ++= table(Seq("package", "modules", "types", "abstract", "Ca", "Ce", "Ce pkgs", "I", "A", "D"),
      pkgs.sortBy(p => (-math.round(p.distance * 1000), -p.ce, p.name)).map(p =>
        Seq(s"`${p.name}`", p.modules.mkString(" "), p.types.toString, p.abstracts.toString, p.ca.toString, p.ce.toString, p.cePackages.toString, f2(p.instability), f2(p.abstractness), f2(p.distance))))
    out += ""
    out += s"## Package cycles (${cyc.size})"
    out += ""
    if (cyc.isEmpty) out += "None."
    cyc.zipWithIndex.foreach { case (c, i) =>
      out += s"${i + 1}. ${c.packages.map(p => s"`$p`").mkString(", ")} -- ${c.edges.size} edges inside it, lightest first (the cheapest to cut):"
      c.edges.foreach { case ((a, b), refs) =>
        val sample = refs.take(3).map { case (x, y) => s"`$x` -> `$y`" }.mkString(", ")
        out += s"   - `$a -> $b`: ${refs.size} refs (${sample}${if (refs.size > 3) ", ..." else ""})"
      }
    }
    layers.foreach { l =>
      val bad = layerViolations(g, l)
      val badRank = rankViolations(g, l)
      def sample(refs: Seq[(String, String)]): String =
        refs.take(3).map { case (x, y) => s"`$x` -> `$y`" }.mkString(", ") + (if (refs.size > 3) ", ..." else "")
      def place(p: String): String = l.layerOf(p).map(n => l.rankOf(p).fold(s"$n")(r => s"$n.$r")).getOrElse("?")
      out += ""
      out += s"## Layers (${bad.size + badRank.size} violations)"
      out += ""
      out += "Each package's layer and, inside an ordered layer, its rank (test/resources/coupling-layers.tsv: `layer.rank`), bottom " +
        "up; a package depends only on its own layer or below, and inside an ordered layer only on its own rank or below."
      out += ""
      out ++= table(Seq("layer", "packages"), pkgs.groupBy(p => place(p.name)).toSeq
        .sortBy { case (n, _) => n.split('.').map(x => if (x == "?") Int.MaxValue else x.toInt).toSeq match { case Seq(a) => (a, -1); case Seq(a, b) => (a, b); case _ => (Int.MaxValue, 0) } }
        .map { case (n, ps) => Seq(n, ps.map(p => s"`${p.name}`").sorted.mkString(" ")) })
      bad.foreach { case ((a, b), refs) =>
        out += (if (b == "?") s"- `$a` has no layer" else s"- `$a -> $b` points up: ${refs.size} refs (${sample(refs)})")
      }
      badRank.foreach { case ((a, b), refs) =>
        out += (if (b == "?") s"- `$a` has no rank in its layer (${place(a)})"
                else s"- `$a -> $b` points up inside layer ${place(a).takeWhile(_ != '.')} (rank ${place(a)} -> ${place(b)}; the allowed direction is `$b -> $a`): ${refs.size} refs (${sample(refs)})")
      }
    }
    out += ""
    def ranked(title: String, key: ((Int, Int)) => Int): Unit = {
      out += title
      out += ""
      out ++= table(Seq("class", "module", "fan-in", "fan-out"),
        fan.toSeq.sortBy { case (c, f) => (-key(f), c) }.take(top).map { case (c, (in, o)) => Seq(s"`$c`", g.byName(c).module, in.toString, o.toString) })
      out += ""
    }
    ranked(s"## Top $top classes by fan-in", _._1)
    ranked(s"## Top $top classes by fan-out", _._2)
    ranked(s"## God hubs\n\nHigh fan-in AND high fan-out: the top $top by the smaller of the two.", f => math.min(f._1, f._2))
    out += "## Split packages"
    out += ""
    val split = splitPackages(g)
    if (split.isEmpty) out += "None."
    split.foreach { case (p, ms) => out += s"- `$p`: ${ms.map { case (m, n) => s"$m ($n)" }.mkString(", ")}" }
    out += ""
    out.result().mkString("\n")
  }
}
