package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/** The scrapers' recording and writing tools (`Record*`, `Write*`, `MeasureStartup`) are started by `runMain`, so each
 *  needs a `main(Array[String])` the JVM finds: a `def main(args)(using X)` compiles to `main(String[], X)` and starts
 *  nothing. The worker's ToolCommandLinesSpec holds its own programs and the scrapers' fixtures config (both on its
 *  classpath) to the same; the scrapers' test tree is on this module's classpath only, so its programs are held here. */
class ToolMainsSpec extends AnyFlatSpec with Matchers {

  private val Root          = Paths.get("scrapers/src/test/scala")
  private val Package       = """(?m)^package ([\w.]+)""".r
  private val Object        = """(?m)^object (\w+)""".r
  private val MainDefinition = """(?m)^\s*(?:override\s+)?def main\(""".r

  /** The source with its comment lines dropped, so a `def main(` named in a scaladoc is no program. */
  private def code(source: String): String =
    source.linesIterator.filterNot(line => line.trim.startsWith("*") || line.trim.startsWith("/*") || line.trim.startsWith("//")).mkString("\n")

  "every program under the scrapers' test tree" should "have a main the JVM can start" in {
    val programs: Seq[(Path, String)] = Files.walk(Root).iterator.asScala.toSeq
      .filter(p => p.toString.endsWith(".scala") && !p.toString.endsWith("Spec.scala"))
      .map(p => p -> Files.readString(p))
      .filter { case (_, source) => MainDefinition.findFirstIn(code(source)).isDefined }
    programs.size should be > 20
    val unstartable = programs.collect { case (path, source) if {
      val prefix = Package.findFirstMatchIn(source).fold("")(_.group(1) + ".")
      !Object.findAllMatchIn(source).exists(o => scala.util.Try(Class.forName(prefix + o.group(1)).getMethod("main", classOf[Array[String]])).isSuccess)
    } => path.toString }
    unstartable shouldBe empty
  }
}
