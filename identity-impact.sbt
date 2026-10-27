// The test classes a change can affect (project/TestImpact.scala): `identityImpact <changed-paths file> <out file>` writes
// `ALL <why>` lines, or `<module> <test class>` lines -- what scripts/ci/identity_impact.py narrows the hook's identity
// area to. Every module's main, fixtures and test analyses: a spec reaches main classes of the modules it depends on.
// A file of its own (build.sbt's modules are in scope), so that scripts/identity-impact-check lays it over an older
// commit to replay the selection there.
def impactModule(name: String): ProjectReference = LocalProject(name)
lazy val ImpactFixtures = Configuration.of("Fixtures", "fixtures")
lazy val identityImpact = inputKey[Unit]("the test classes a change can affect: identityImpact <changed paths> <out>")

identityImpact := {
  val args = Def.spaceDelimited("<changed paths> <out>").parsed
  require(args.size == 2, "identityImpact <changed-paths file> <out file>")
  val units = Seq(
    TestImpact.Unit("geodata", (impactModule("geodata") / Compile / compile).value), TestImpact.Unit("geodata", (impactModule("geodata") / Test / compile).value),
    TestImpact.Unit("common", (impactModule("common") / Compile / compile).value), TestImpact.Unit("common", (impactModule("common") / Test / compile).value),
    TestImpact.Unit("identity", (impactModule("identity") / Compile / compile).value), TestImpact.Unit("identity", (impactModule("identity") / Test / compile).value),
    TestImpact.Unit("testkit", (impactModule("testkit") / Compile / compile).value), TestImpact.Unit("testkit", (impactModule("testkit") / Test / compile).value),
    TestImpact.Unit("scrapers", (impactModule("scrapers") / Compile / compile).value), TestImpact.Unit("scrapers", (impactModule("scrapers") / Test / compile).value),
    TestImpact.Unit("worker", (impactModule("worker") / Compile / compile).value),
    TestImpact.Unit("worker", (impactModule("worker") / ImpactFixtures / compile).value), TestImpact.Unit("worker", (impactModule("worker") / Test / compile).value),
    TestImpact.Unit("web", (impactModule("web") / Compile / compile).value), TestImpact.Unit("web", (impactModule("web") / Test / compile).value),
    TestImpact.Unit("e2e", (impactModule("e2e") / Compile / compile).value), TestImpact.Unit("e2e", (impactModule("e2e") / Test / compile).value))
  val tests = Map(
    "geodata" -> (impactModule("geodata") / Test / definedTestNames).value, "common" -> (impactModule("common") / Test / definedTestNames).value,
    "identity" -> (impactModule("identity") / Test / definedTestNames).value, "testkit" -> (impactModule("testkit") / Test / definedTestNames).value,
    "scrapers" -> (impactModule("scrapers") / Test / definedTestNames).value, "worker" -> (impactModule("worker") / Test / definedTestNames).value,
    "web" -> (impactModule("web") / Test / definedTestNames).value, "e2e" -> (impactModule("e2e") / Test / definedTestNames).value)
  // the identity stamps' digested paths (identity's resourceGenerators)
  val stamps = (impactModule("identity") / Compile / managedResources).value.filter(_.getName.endsWith("-sources.txt"))
  val lines  = TestImpact.select(baseDirectory.value, units, tests, stamps, IO.readLines(file(args(0))).map(_.trim).filter(_.nonEmpty))
  IO.writeLines(file(args(1)), lines)
  // and every test class, for a check that runs what the selection left out (scripts/identity-impact-check)
  IO.writeLines(file(args(1) + ".tests"), tests.toSeq.sortBy(_._1).flatMap { case (m, names) => names.sorted.map(n => s"$m $n") })
  streams.value.log.info(s"identityImpact: ${lines.size} line(s) to ${args(1)}")
}
