import sbt._
import sbt.Keys._

/**
 * Every module's main, fixtures and test compile analyses, and its discovered test classes: what the refactoring tools'
 * tasks read (refactor-tools.sbt: `deadCode`, `touchedSpecs`). The same units as identity-impact.sbt's `impactUnits`,
 * kept apart on purpose: scripts/identity-impact-check lays that file, alone, over commits that predate this one.
 */
object AnalysisUnits {
  private val Fixtures = Configuration.of("Fixtures", "fixtures")
  private def m(name: String): ProjectReference = LocalProject(name)

  val units = Def.task {
    Seq(
      TestImpact.Unit("geodata", (m("geodata") / Compile / compile).value), TestImpact.Unit("geodata", (m("geodata") / Test / compile).value),
      TestImpact.Unit("base", (m("base") / Compile / compile).value), TestImpact.Unit("base", (m("base") / Test / compile).value),
      TestImpact.Unit("http", (m("http") / Compile / compile).value), TestImpact.Unit("http", (m("http") / Test / compile).value),
      TestImpact.Unit("common", (m("common") / Compile / compile).value), TestImpact.Unit("common", (m("common") / Test / compile).value),
      TestImpact.Unit("identity", (m("identity") / Compile / compile).value), TestImpact.Unit("identity", (m("identity") / Test / compile).value),
      TestImpact.Unit("testkit", (m("testkit") / Compile / compile).value), TestImpact.Unit("testkit", (m("testkit") / Test / compile).value),
      TestImpact.Unit("scrapers", (m("scrapers") / Compile / compile).value), TestImpact.Unit("scrapers", (m("scrapers") / Test / compile).value),
      TestImpact.Unit("worker", (m("worker") / Compile / compile).value),
      TestImpact.Unit("worker", (m("worker") / Fixtures / compile).value), TestImpact.Unit("worker", (m("worker") / Test / compile).value),
      TestImpact.Unit("web", (m("web") / Compile / compile).value), TestImpact.Unit("web", (m("web") / Test / compile).value),
      TestImpact.Unit("e2e", (m("e2e") / Compile / compile).value), TestImpact.Unit("e2e", (m("e2e") / Test / compile).value),
      TestImpact.Unit("repochecks", (m("repochecks") / Fixtures / compile).value),
      TestImpact.Unit("repochecks", (m("repochecks") / Test / compile).value))
  }

  private val IntegrationTest = Configuration.of("IntegrationTest", "it")
  private val PageTest        = Configuration.of("PageTest", "page")

  /** [[units]] and every other source set that can call production code -- the integration and page specs, the http
   *  and scrapers fixtures: what the dead-code finder counts as a caller (test-side). */
  val callers = Def.task {
    units.value ++ Seq(
      TestImpact.Unit("http", (m("http") / Fixtures / compile).value), TestImpact.Unit("scrapers", (m("scrapers") / Fixtures / compile).value),
      TestImpact.Unit("worker", (m("worker") / IntegrationTest / compile).value),
      TestImpact.Unit("web", (m("web") / IntegrationTest / compile).value), TestImpact.Unit("web", (m("web") / PageTest / compile).value))
  }

  val tests = Def.task {
    Map(
      "geodata" -> (m("geodata") / Test / definedTestNames).value, "common" -> (m("common") / Test / definedTestNames).value,
      "base" -> (m("base") / Test / definedTestNames).value, "http" -> (m("http") / Test / definedTestNames).value,
      "identity" -> (m("identity") / Test / definedTestNames).value, "testkit" -> (m("testkit") / Test / definedTestNames).value,
      "scrapers" -> (m("scrapers") / Test / definedTestNames).value, "worker" -> (m("worker") / Test / definedTestNames).value,
      "web" -> (m("web") / Test / definedTestNames).value, "e2e" -> (m("e2e") / Test / definedTestNames).value,
      "repochecks" -> (m("repochecks") / Test / definedTestNames).value)
  }
}
