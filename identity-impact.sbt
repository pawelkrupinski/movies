// The test classes a change can affect (project/TestImpact.scala): `identityImpact <changed-paths file> <out file>` writes
// `ALL <why>` lines, or `<module> <test class>` lines -- what scripts/ci/identity_impact.py narrows the hook's identity
// area to. Every module's main, fixtures and test analyses: a spec reaches main classes of the modules it depends on.
// A file of its own (build.sbt's modules are in scope), so that scripts/identity-impact-check lays it over an older
// commit to replay the selection there.
def impactModule(name: String): ProjectReference = LocalProject(name)
lazy val ImpactFixtures = Configuration.of("Fixtures", "fixtures")
lazy val impactUnits = Def.task {
  Seq(
    TestImpact.Unit("geodata", (impactModule("geodata") / Compile / compile).value), TestImpact.Unit("geodata", (impactModule("geodata") / Test / compile).value),
    TestImpact.Unit("common", (impactModule("common") / Compile / compile).value), TestImpact.Unit("common", (impactModule("common") / Test / compile).value),
    TestImpact.Unit("identity", (impactModule("identity") / Compile / compile).value), TestImpact.Unit("identity", (impactModule("identity") / Test / compile).value),
    TestImpact.Unit("testkit", (impactModule("testkit") / Compile / compile).value), TestImpact.Unit("testkit", (impactModule("testkit") / Test / compile).value),
    TestImpact.Unit("scrapers", (impactModule("scrapers") / Compile / compile).value), TestImpact.Unit("scrapers", (impactModule("scrapers") / Test / compile).value),
    TestImpact.Unit("worker", (impactModule("worker") / Compile / compile).value),
    TestImpact.Unit("worker", (impactModule("worker") / ImpactFixtures / compile).value), TestImpact.Unit("worker", (impactModule("worker") / Test / compile).value),
    TestImpact.Unit("web", (impactModule("web") / Compile / compile).value), TestImpact.Unit("web", (impactModule("web") / Test / compile).value),
    TestImpact.Unit("e2e", (impactModule("e2e") / Compile / compile).value), TestImpact.Unit("e2e", (impactModule("e2e") / Test / compile).value))
}
lazy val impactTests = Def.task {
  Map(
    "geodata" -> (impactModule("geodata") / Test / definedTestNames).value, "common" -> (impactModule("common") / Test / definedTestNames).value,
    "identity" -> (impactModule("identity") / Test / definedTestNames).value, "testkit" -> (impactModule("testkit") / Test / definedTestNames).value,
    "scrapers" -> (impactModule("scrapers") / Test / definedTestNames).value, "worker" -> (impactModule("worker") / Test / definedTestNames).value,
    "web" -> (impactModule("web") / Test / definedTestNames).value, "e2e" -> (impactModule("e2e") / Test / definedTestNames).value)
}
// the identity stamps' digested paths (identity's resourceGenerators)
lazy val impactStamps = Def.task((impactModule("identity") / Compile / managedResources).value.filter(_.getName.endsWith("-sources.txt")))

lazy val identityImpact = inputKey[Unit]("the test classes a change can affect: identityImpact <changed paths> <out>")

identityImpact := {
  val args = Def.spaceDelimited("<changed paths> <out>").parsed
  require(args.size == 2, "identityImpact <changed-paths file> <out file>")
  val units  = impactUnits.value
  val tests  = impactTests.value
  val stamps = impactStamps.value
  val lines  = TestImpact.select(baseDirectory.value, units, tests, stamps, IO.readLines(file(args(0))).map(_.trim).filter(_.nonEmpty))
  IO.writeLines(file(args(1)), lines)
  // and every test class, for a check that runs what the selection left out (scripts/identity-impact-check)
  IO.writeLines(file(args(1) + ".tests"), tests.toSeq.sortBy(_._1).flatMap { case (m, names) => names.sorted.map(n => s"$m $n") })
  streams.value.log.info(s"identityImpact: ${lines.size} line(s) to ${args(1)}")
}

// THE POPULATION WALK'S KEY (PopulationMonotonicitySpec's CacheKey, the pre-push hook's identity check): `identityWalkDigest
// <out>` writes the SHA-256 (project/SourceDigest.scala: a source as its code) of every path a change to which would let
// the test-impact selection reach the walk (TestImpact.reach from the spec's source) -- less the slices, each of which
// the spec keys by its own content, and the known-flips list, which the spec judges the walk's rows by but the walk never
// reads -- and the paths to `<out>.paths`. Equal digests: the walk's code and data are the same, and a slice of the same
// content walks to the same rows. Nothing (an empty file) on a working tree with changes or untracked files: what is
// walked there is not what any commit holds.
lazy val identityWalkDigest = inputKey[Unit]("the population walk's code and data digest: identityWalkDigest <out>")

identityWalkDigest := {
  val args = Def.spaceDelimited("<out>").parsed
  require(args.size == 1, "identityWalkDigest <out>")
  val repo    = baseDirectory.value
  val units   = impactUnits.value
  val stamps  = impactStamps.value
  val out     = file(args(0))
  val dirty   = scala.sys.process.Process(Seq("git", "status", "--porcelain", "--untracked-files=all"), repo).!!.trim
  if (dirty.nonEmpty) { IO.write(out, ""); streams.value.log.info("identityWalkDigest: the working tree has changes: no digest") }
  else {
    val tracked = scala.sys.process.Process(Seq("git", "ls-files", "-z"), repo).!!.split('\u0000').toSeq.filter(_.nonEmpty)
    val slices  = Seq("test/resources/fixtures/drift-slices/", "test/resources/fixtures/identity-slices/")
    val paths   = TestImpact.reach(repo, units, stamps, tracked,
      Set("worker/src/test/scala/services/identity/PopulationMonotonicitySpec.scala"),
      p => slices.exists(p.startsWith) || p == "test/resources/fixtures/identity-population-flips-known.tsv")
    IO.write(out, kinowo.build.SourceDigest.of(paths, path => IO.readBytes(repo / path)))
    IO.writeLines(file(args(0) + ".paths"), paths)
    streams.value.log.info(s"identityWalkDigest: ${paths.size} path(s) to ${args(0)}")
  }
}
