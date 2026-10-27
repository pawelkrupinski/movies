package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `testUnit` runs every module's unit specs in one kept sbt server. Unforked, what each run's specs left behind
 * stayed in that server — ~260 MB of heap and ~200 MB of metaspace per worker run, measured 2026-10-19 — until a
 * later run died with "sbt server disconnected". So each module's unit specs run in a forked, heap-capped JVM
 * (project/ForkedTests.scala). `TestJvmContract` checks it from inside worker's and web's JVMs; this checks that
 * no module `testUnitNoE2e` runs was left out, a new one included.
 */
class UnitTestForkSpec extends AnyFlatSpec with Matchers {
  private lazy val buildSbt = RepoFile.read("build.sbt")

  /** A project's definition in build.sbt: from its `lazy val` to the next one. */
  private def project(name: String): String = {
    val from = buildSbt.indexOf(s"lazy val $name = (project")
    withClue(s"build.sbt defines no project $name: ")(from should be >= 0)
    val to = buildSbt.indexOf("\nlazy val ", from + 1)
    buildSbt.substring(from, if (to < 0) buildSbt.length else to)
  }

  /** Play forks web's Test JVM on its own (PlayScala's default), in web/: it takes the options alone. */
  private val ForkedByPlay = Set("web")

  "every module testUnit runs" should "run its unit specs in a forked JVM with a capped heap" in {
    val modules = RepoFile.commandAlias("testUnitNoE2e").stripPrefix("all ").split("\\s+").filter(_.nonEmpty)
      .map(_.stripSuffix("/Test/test")).toSeq
    modules should contain allElementsOf ForkedByPlay
    modules.foreach { module =>
      val wiring = if (ForkedByPlay(module)) """ForkedTests\.options\(Test, heap = "\d+[mg]"\)""" else """ForkedTests\.settings\(Test, heap = "\d+[mg]"\)"""
      withClue(s"$module: ")(project(module) should include regex wiring)
    }
  }

  it should "run one at a time under the sbt server's JVM slot, and two at once only on a CI runner" in {
    buildSbt.linesIterator.map(_.trim).toSeq should contain("ForkedTests.forkSlots")
    val forked = RepoFile.read("project/ForkedTests.scala")
    forked should include("Tags.limit(Tags.ForkedTestGroup, forksAtOnce)")
    forked should include("""contains("true")) 2 else 1)""")
  }
}
