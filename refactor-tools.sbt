// THE TOUCHED-CODE TOOLS' sbt task (their plugins: project/refactor-tools.sbt). A file of its own:
// scripts/lib/touched_tree.py lays it, with the plugins and the project/ sources it reads (AnalysisUnits, TouchedSpecs,
// UsedNamesAccess), over an older commit's build to measure a range there.
//
// THE SPECS THAT REACH THE TOUCHED METHODS (project/TouchedSpecs.scala; scripts/mutation-check, scripts/coverage-touched):
// `touchedSpecs <touched.tsv> <out>` reads scripts/touched_methods.py's rows (path, key, first, last) and writes
// `<module> <spec class>` lines.
lazy val touchedSpecs = inputKey[Unit]("the specs that reach the touched methods: touchedSpecs <touched.tsv> <out>")

touchedSpecs := {
  val args = Def.spaceDelimited("<touched.tsv> <out>").parsed
  require(args.size == 2, "touchedSpecs <touched.tsv> <out>")
  val touched = IO.readLines(file(args(0))).filter(_.trim.nonEmpty).map(_.split('\t')).map { row =>
    // the key's method: `<path>:<Owner.method>`, a repeated name `#2`
    row(0) -> row(1).substring(row(1).lastIndexOf(':') + 1).split('.').last.takeWhile(_ != '#')
  }
  val lines = TouchedSpecs.select(baseDirectory.value, AnalysisUnits.units.value, AnalysisUnits.tests.value, touched)
  IO.writeLines(file(args(1)), lines)
  streams.value.log.info(s"touchedSpecs: ${lines.size} spec(s) to ${args(1)}")
}

// SCALAFIX OVER EVERY SOURCE SET (scripts/scalafix-organize, scripts/scalafix-move; .scalafix.conf): sbt-scalafix gives
// Compile and Test a `scalafix` task; these are the other configurations holding sources (integration, page, fixtures).
inScope(Scope(Select(LocalProject("worker")), Zero, Zero, Zero))(
  scalafixConfigSettings(Configuration.of("IntegrationTest", "it")) ++ scalafixConfigSettings(Configuration.of("Fixtures", "fixtures")))
inScope(Scope(Select(LocalProject("web")), Zero, Zero, Zero))(
  scalafixConfigSettings(Configuration.of("IntegrationTest", "it")) ++ scalafixConfigSettings(Configuration.of("PageTest", "page")))
Seq("http", "scrapers", "repochecks").flatMap(m => inScope(Scope(Select(LocalProject(m)), Zero, Zero, Zero))(
  scalafixConfigSettings(Configuration.of("Fixtures", "fixtures"))))
