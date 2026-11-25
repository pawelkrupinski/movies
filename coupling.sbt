// COUPLING (project/Coupling.scala, scripts/coupling-report): `couplingReport <rows out> [<markdown out> [<edges out>
// [<fanouts out> [<collaborators out>]]]]` writes the production code's coupling rows -- the shape of test/resources/coupling-baseline.tsv,
// which scripts/ci/coupling_ratchet.py compares them with -- and, given more paths, the human report, every class
// reference, every class's held fan-out (what a `Coupling: split of <X>` claim sums), and every type's verdict, data or
// collaborator (only a collaborator counts in a fan-out). Every deployed
// module's Compile analysis (testkit, itkit, repochecks and e2e are test code), and the layering it must keep
// (test/resources/coupling-layers.tsv: every package's layer, an upward package edge a `layer-violation` row), and the
// exemptions from the fan-out and distance limits (test/resources/coupling-exemptions.tsv).
def couplingModule(name: String): ProjectReference = LocalProject(name)
lazy val couplingUnits = Def.task {
  Seq(
    "geodata" -> (couplingModule("geodata") / Compile / compile).value, "base" -> (couplingModule("base") / Compile / compile).value,
    "http" -> (couplingModule("http") / Compile / compile).value, "common" -> (couplingModule("common") / Compile / compile).value,
    "identity" -> (couplingModule("identity") / Compile / compile).value, "scrapers" -> (couplingModule("scrapers") / Compile / compile).value,
    "worker" -> (couplingModule("worker") / Compile / compile).value, "web" -> (couplingModule("web") / Compile / compile).value)
}
// each module's direct Compile dependencies (build.sbt's dependsOn: a mapping into `compile`)
lazy val couplingDeclared = Def.setting {
  def into(dep: ClasspathDep[ProjectRef]): Boolean =
    dep.configuration.forall(_.split(';').map(_.trim).exists(m => m == "compile" || m == "compile->compile" || m == "compile->default(compile)"))
  def declared(p: ResolvedProject): Set[String] = p.dependencies.filter(into).map(_.project.project).toSet
  Map(
    "geodata" -> declared((couplingModule("geodata") / thisProject).value), "base" -> declared((couplingModule("base") / thisProject).value),
    "http" -> declared((couplingModule("http") / thisProject).value), "common" -> declared((couplingModule("common") / thisProject).value),
    "identity" -> declared((couplingModule("identity") / thisProject).value), "scrapers" -> declared((couplingModule("scrapers") / thisProject).value),
    "worker" -> declared((couplingModule("worker") / thisProject).value), "web" -> declared((couplingModule("web") / thisProject).value))
}

lazy val couplingReport = inputKey[Unit]("the production code's coupling: couplingReport <rows out> [<markdown out> [<edges out> [<fanouts out> [<collaborators out>]]]]")

// the graph every coupling task measures: the production class graph with its deeper packages measured on their own,
// the layer table's lines and the exemptions
lazy val couplingGraph = Def.task {
  val layerLines = IO.readLines(baseDirectory.value / "test" / "resources" / "coupling-layers.tsv")
  val layers = kinowo.build.Coupling.Layers.parse(layerLines)
  // a package of three or more segments with a layer-table row of its own is measured as a package of its own
  val graph = CouplingAnalysis.graph(baseDirectory.value, couplingUnits.value, fileConverter.value).copy(units = layers.units)
  val exemptions = kinowo.build.Coupling.Exemptions.parse(IO.readLines(baseDirectory.value / "test" / "resources" / "coupling-exemptions.tsv"))
  (graph, layerLines, exemptions)
}

couplingReport := {
  val args = Def.spaceDelimited("<rows out> [<markdown out> [<edges out> [<fanouts out> [<collaborators out>]]]]").parsed
  require(args.size >= 1 && args.size <= 5, "couplingReport <rows out> [<markdown out> [<edges out> [<fanouts out> [<collaborators out>]]]]")
  val started = System.nanoTime
  val (graph, layerLines, exemptions) = couplingGraph.value
  val layers = Some(kinowo.build.Coupling.Layers.parse(layerLines))
  IO.writeLines(file(args(0)), kinowo.build.Coupling.BaselineHeader ++ kinowo.build.Coupling.baselineRows(graph, layers = layers, exemptions = exemptions))
  if (args.size >= 2) IO.write(file(args(1)), kinowo.build.Coupling.report(graph, couplingDeclared.value, layers = layers, exemptions = exemptions))
  if (args.size >= 3) IO.writeLines(file(args(2)), kinowo.build.Coupling.edgeRows(graph))
  if (args.size >= 4) IO.writeLines(file(args(3)), kinowo.build.Coupling.fanOutRows(graph))
  if (args.size == 5) IO.writeLines(file(args(4)), kinowo.build.Coupling.collaboratorRows(graph))
  streams.value.log.info(s"couplingReport: ${graph.types.size} classes (${graph.data.size} data), ${graph.known.size} dependencies (${graph.coupled.size} on collaborators), " +
    s"${(System.nanoTime - started) / 1000000} ms after the compiles, to ${args.mkString(" and ")}")
}

// `couplingClassCycles <package> <markdown out> [<order>]` (scripts/class-cycles, project/ClassCycles.scala): the strongly
// connected components of the class graph inside a package and its sub-packages, and given an order of groups of its
// classes -- `a<b<c`, or a `<class><TAB><group>` table's path -- every class edge pointing up it.
lazy val couplingClassCycles = inputKey[Unit]("couplingClassCycles <package> <markdown out> [<order: a<b<c, or a class<TAB>group table>]")

couplingClassCycles := {
  val args = Def.spaceDelimited("<package> <markdown out> [<order>]").parsed
  require(args.size == 2 || args.size == 3, "couplingClassCycles <package> <markdown out> [<order: a<b<c, or a class<TAB>group table>]")
  val (graph, _, _) = couplingGraph.value
  val order = args.lift(2).map(o => if (file(o).isFile) kinowo.build.ClassCycles.Order.table(IO.readLines(file(o))) else kinowo.build.ClassCycles.Order.inline(o))
  IO.write(file(args(1)), kinowo.build.ClassCycles.report(graph, args(0), order))
}

// `couplingSimulate <moves.tsv> <markdown out>` (scripts/coupling-report --simulate, project/CouplingSimulation.scala):
// the coupling a set of class moves, folds and layer rows would leave, measured over today's graph with no code change.
lazy val couplingSimulate = inputKey[Unit]("couplingSimulate <moves.tsv> <markdown out>")

couplingSimulate := {
  val args = Def.spaceDelimited("<moves.tsv> <markdown out>").parsed
  require(args.size == 2, "couplingSimulate <moves.tsv> <markdown out>")
  val (graph, layerLines, exemptions) = couplingGraph.value
  val moves = kinowo.build.CouplingSimulation.Moves.parse(IO.readLines(file(args(0))), graph)
  IO.write(file(args(1)), kinowo.build.CouplingSimulation.report(graph, layerLines, exemptions, moves))
}

// `couplingExemptionSlack <markdown out>` (scripts/coupling-report --exemption-slack, project/ExemptionSlack.scala): for
// every `class` exemption row, the collaborators its class could give up, read from its class files' methods, and its
// estimated fan-out after that cut.
lazy val couplingExemptionSlack = inputKey[Unit]("couplingExemptionSlack <markdown out>")

couplingExemptionSlack := {
  val args = Def.spaceDelimited("<markdown out>").parsed
  require(args.size == 1, "couplingExemptionSlack <markdown out>")
  val (graph, _, exemptions) = couplingGraph.value
  val classes = exemptions.rows.filter(_.kind == "class").map(_.pattern).filter(graph.byName.contains).distinct
  val held = kinowo.build.Coupling.heldTargets(graph)
  // the exempt classes' class files, and their collaborators' (what each hands out)
  val (files, topOfBinary) = CouplingAnalysis.classFiles(baseDirectory.value, couplingUnits.value, fileConverter.value,
    classes.toSet ++ classes.flatMap(c => held.getOrElse(c, Set.empty[String])))
  val topOf: String => Option[String] = b => kinowo.build.Coupling.foldBinary(b, topOfBinary)
  def own(cls: String) = files.getOrElse(cls, Nil).filter(c => c.name == cls || c.name == cls + "$")
  val slacks = classes.map { cls =>
    val targets = held.getOrElse(cls, Set.empty[String])
    kinowo.build.ExemptionSlack.of(cls, targets, files.getOrElse(cls, Nil), kinowo.build.ExemptionSlack.target(cls, targets, graph, topOf),
      (provider, collaborator) => kinowo.build.ExemptionSlack.handsOut(own(provider), topOf, collaborator))
  }
  IO.write(file(args(0)), kinowo.build.ExemptionSlack.report(slacks, t => graph.collaboratorWhy.getOrElse(t, "")))
}
