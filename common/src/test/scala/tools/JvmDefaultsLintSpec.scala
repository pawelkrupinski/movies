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
 *
 * System properties are the same kind of global. A spec that set `KINOWO_COUNTRY` to boot one
 * country's wiring set it for every web suite beside it, and one that cleared a property it never
 * restored left the next suite reading a value nobody chose. The code under test takes a typed
 * value instead — a `Country`, a `MongoAddress`, an `Env` built over a map — which the spec
 * constructs, and only a composition root resolves that value from the process. A setting the
 * JDK itself reads is exercised in a [[ChildJvm]] started with it.
 */
class JvmDefaultsLintSpec extends AnyFlatSpec with Matchers {

  private val Mutators = """\b(Locale|TimeZone)\s*\.\s*setDefault\s*\(""".r

  /** `System.setProperty` / `clearProperty` / `setProperties`, and `sys.props` written through
   *  any of its mutating forms (`+=`, `-=`, `++=`, `--=`, `update`, `put`, `remove`, `clear`,
   *  `sys.props(key) = value`). Reads are fine: they change nothing another suite sees. */
  private val PropertyMutators =
    """\bSystem\s*\.\s*(setProperty|clearProperty|setProperties)\s*\(|\bsys\s*\.\s*props\s*(\+\+?=|--?=|\.\s*(update|put|remove|clear|addOne|subtractOne)\b|\([^()]*\)\s*=(?!=))""".r

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

  they should "never set or clear a JVM system property" in {
    val offenders = testSources.filter(path => PropertyMutators.findFirstIn(Files.readString(path)).isDefined)
    withClue("hand the code under test a typed value (a Country, a MongoAddress, an Env.of(...)) resolved from the " +
      "process only at its composition root; for a property the JDK itself reads, run the code in a ChildJvm " +
      "started with it (-Dname=value): ") {
      offenders.map(RepoRoot.dir.toPath.relativize(_).toString).sorted shouldBe empty
    }
  }
}
