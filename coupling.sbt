// COUPLING (project/Coupling.scala, scripts/coupling-report): `couplingReport <rows out> [<markdown out>]` writes the
// production code's coupling rows -- the shape of test/resources/coupling-baseline.tsv, which
// scripts/ci/coupling_ratchet.py compares them with -- and, given a second path, the human report. Every deployed
// module's Compile analysis (testkit, itkit, repochecks and e2e are test code).
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

lazy val couplingReport = inputKey[Unit]("the production code's coupling: couplingReport <rows out> [<markdown out>]")

couplingReport := {
  val args = Def.spaceDelimited("<rows out> [<markdown out>]").parsed
  require(args.size == 1 || args.size == 2, "couplingReport <rows out> [<markdown out>]")
  val started = System.nanoTime
  val graph = CouplingAnalysis.graph(baseDirectory.value, couplingUnits.value)
  IO.writeLines(file(args(0)), kinowo.build.Coupling.BaselineHeader ++ kinowo.build.Coupling.baselineRows(graph))
  if (args.size == 2) IO.write(file(args(1)), kinowo.build.Coupling.report(graph, couplingDeclared.value))
  streams.value.log.info(s"couplingReport: ${graph.types.size} classes, ${graph.known.size} dependencies, " +
    s"${(System.nanoTime - started) / 1000000} ms after the compiles, to ${args.mkString(" and ")}")
}
