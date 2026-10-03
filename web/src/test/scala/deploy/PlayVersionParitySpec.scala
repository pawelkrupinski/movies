package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Play arrives twice: the web app gets it from the sbt plugin (project/plugins.sbt), and
 * `common` declares the library itself at `playVersion` (project/Dependencies.scala). Two
 * numbers bumped by hand -- or by Scala Steward one PR at a time -- drift, and then eviction
 * quietly runs the web app on a Play its plugin was not built for, while `common`'s specs
 * compile against another.
 */
class PlayVersionParitySpec extends AnyFlatSpec with Matchers {
  private val Plugin = """addSbtPlugin\("org\.playframework"\s*%\s*"sbt-plugin"\s*%\s*"([^"]+)"\)""".r

  "the Play sbt plugin" should "be the Play version the library dependency pins" in {
    val plugin = Plugin.findFirstMatchIn(RepoFile.read(RepoFile.locate("project/plugins.sbt"))).map(_.group(1))
      .getOrElse(fail("no Play sbt-plugin in project/plugins.sbt"))
    plugin shouldBe RepoFile.declaredVersion("playVersion")
  }
}
