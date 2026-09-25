package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * Unit specs share one JVM per module and run IN PARALLEL, so a spec that switches a JVM-wide
 * default switches it for every suite running beside it. TextNormalizationSpec once set a Turkish
 * default locale for one test: measured with a bystander thread, 739 of its 43.6 million
 * default-locale `toLowerCase` calls came back with a dotless `ı` while that test looped. A spec
 * that needs another default runs its code in a [[ChildJvm]] started with it instead.
 */
class JvmDefaultsLintSpec extends AnyFlatSpec with Matchers {

  private val Mutators = """\b(Locale|TimeZone)\s*\.\s*setDefault\s*\(""".r

  private def testSources: Seq[Path] = {
    val root = RepoRoot.dir.toPath
    for {
      module <- Seq("common", "testkit", "worker", "web", "e2e")
      layer  <- Seq("test", "it", "page")
      dir     = root.resolve(s"$module/src/$layer")
      if Files.isDirectory(dir)
      file   <- Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".scala")).toSeq
      if file.getFileName.toString != "JvmDefaultsLintSpec.scala"
    } yield file
  }

  "specs" should "never change the JVM's default locale or time zone" in {
    val offenders = testSources.filter(path => Mutators.findFirstIn(Files.readString(path)).isDefined)
    withClue("run the code under that default in a ChildJvm started with it (-Duser.language=…, -Duser.timezone=…): ") {
      offenders.map(RepoRoot.dir.toPath.relativize(_).toString) shouldBe empty
    }
  }
}
