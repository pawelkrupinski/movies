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

couplingReport := {
  val args = Def.spaceDelimited("<rows out> [<markdown out> [<edges out> [<fanouts out> [<collaborators out>]]]]").parsed
  require(args.size >= 1 && args.size <= 5, "couplingReport <rows out> [<markdown out> [<edges out> [<fanouts out> [<collaborators out>]]]]")
  val started = System.nanoTime
  val graph = CouplingAnalysis.graph(baseDirectory.value, couplingUnits.value, fileConverter.value)
  val layers = Some(kinowo.build.Coupling.Layers.parse(IO.readLines(baseDirectory.value / "test" / "resources" / "coupling-layers.tsv")))
  val exemptions = kinowo.build.Coupling.Exemptions.parse(IO.readLines(baseDirectory.value / "test" / "resources" / "coupling-exemptions.tsv"))
  IO.writeLines(file(args(0)), kinowo.build.Coupling.BaselineHeader ++ kinowo.build.Coupling.baselineRows(graph, layers = layers, exemptions = exemptions))
  if (args.size >= 2) IO.write(file(args(1)), kinowo.build.Coupling.report(graph, couplingDeclared.value, layers = layers, exemptions = exemptions))
  if (args.size >= 3) IO.writeLines(file(args(2)), kinowo.build.Coupling.edgeRows(graph))
  if (args.size >= 4) IO.writeLines(file(args(3)), kinowo.build.Coupling.fanOutRows(graph, exemptions))
  if (args.size == 5) IO.writeLines(file(args(4)), kinowo.build.Coupling.collaboratorRows(graph))
  streams.value.log.info(s"couplingReport: ${graph.types.size} classes (${graph.data.size} data), ${graph.known.size} dependencies (${graph.coupled.size} on collaborators), " +
    s"${(System.nanoTime - started) / 1000000} ms after the compiles, to ${args.mkString(" and ")}")
}
