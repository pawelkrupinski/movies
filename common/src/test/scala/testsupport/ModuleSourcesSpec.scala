package testsupport

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files

class ModuleSourcesSpec extends AnyFlatSpec with Matchers {

  private val declared: Set[String] =
    """\(project in file\("([^".]+)"\)\)""".r.findAllMatchIn(Files.readString(ModuleSources.root.resolve("build.sbt"))).map(_.group(1)).toSet

  "the source walk" should "reach every module build.sbt declares, and nothing else" in {
    declared should contain allOf ("common", "identity", "scrapers", "testkit", "worker", "web", "e2e")
    ModuleSources.configDirectories().map(dir => ModuleSources.named(dir).takeWhile(_ != '/')).toSet shouldBe declared
  }

  it should "leave the test-support modules out of production" in {
    val modules = ModuleSources.productionScalaFiles.map(file => ModuleSources.named(file).takeWhile(_ != '/')).toSet
    modules should contain allOf ("common", "identity", "scrapers", "worker", "web")
    modules should not contain "testkit"
  }
}
