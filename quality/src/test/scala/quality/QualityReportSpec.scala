package quality

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The rows the ratchets read: only the deployed modules' src/main is measured, and a method past a limit is a row
 *  (a new one, which quality_ratchet.py fails without a baseline row), a method within every limit none. */
class QualityReportSpec extends AnyFlatSpec with Matchers {

  private def withRoot(files: (String, String)*)(f: Path => Unit): Unit = {
    val root = Files.createTempDirectory("quality-report")
    files.foreach { case (path, text) =>
      val file = root.resolve(path)
      Files.createDirectories(file.getParent)
      Files.write(file, text.getBytes(StandardCharsets.UTF_8))
    }
    f(root)
  }

  /** A method nesting `depth` ifs: cognitive 1 + 2 + ... + depth, cyclomatic depth + 1. */
  private def nestedIfs(name: String, depth: Int): String =
    s"  def $name(x: Int): Int = {\n" + (1 to depth).map(i => s"${"  " * i}if (x > $i) {\n").mkString +
      s"${"  " * (depth + 1)}x\n" + (depth to 1 by -1).map(i => s"${"  " * i}} else 0\n").mkString + "  }\n"

  "the rows" should "hold the limits, and each production method past one; a test source is not measured" in {
    withRoot(
      "worker/src/main/scala/a/Hairy.scala" -> s"package a\nobject Hairy {\n${nestedIfs("deep", 5)}  def flat(x: Int): Int = x\n}\n",
      "worker/src/test/scala/a/HairySpec.scala" -> s"package a\nobject HairySpec {\n${nestedIfs("deep", 5)}}\n",
      "quality/src/main/scala/a/Tool.scala" -> s"package a\nobject Tool {\n${nestedIfs("deep", 5)}}\n",
    ) { root =>
      val sources = QualityReport.productionSources(root)
      sources.map(_.path) shouldBe Seq("worker/src/main/scala/a/Hairy.scala")
      QualityReport.rows(sources, QualityReport.measure(sources)) shouldBe Seq(
        "limit\tcognitive\t15", "limit\tcyclomatic\t10", "limit\tinner-defs\t2", "limit\tmethod-lines\t50",
        "limit\tfile-lines\t800",
        // five nested ifs: 1+2+3+4+5 for the ifs, one per else
        "cognitive\tworker/src/main/scala/a/Hairy.scala:Hairy.deep\t20",
      )
    }
  }

  "a method past 50 lines and a source past 800" should "each be a length row; a method of 50 lines, a source of 800, none" in {
    def longMethod(name: String, lines: Int) = s"  def $name: Int = {\n" + "    println(1)\n" * (lines - 3) + "    1\n  }\n"
    val long = s"package a\nobject Long {\n${longMethod("long", 51)}${longMethod("fits", 50)}" + "  // pad\n" * 697 + "}\n"
    val fits = s"package a\nobject Fits {\n" + "  // pad\n" * 797 + "}\n"
    withRoot("web/src/main/scala/a/Long.scala" -> long, "web/src/main/scala/a/Fits.scala" -> fits) { root =>
      val sources = QualityReport.productionSources(root)
      sources.map(_.text.linesIterator.size) shouldBe Seq(800, 801)
      QualityReport.rows(sources, QualityReport.measure(sources)).filterNot(_.startsWith("limit")) shouldBe Seq(
        "method-lines\tweb/src/main/scala/a/Long.scala:Long.long\t51",
        "file-lines\tweb/src/main/scala/a/Long.scala\t801",
      )
    }
  }

  "a method of a name already seen in its source" should "be keyed by its ordinal" in {
    val methods = MethodMeasures.of("A.scala", "object A { def f(x: Int) = x; def f(x: String) = x }").toOption.get
    QualityReport.keyed("A.scala", methods).map(_.key) shouldBe Seq("A.scala:A.f", "A.scala:A.f#2")
  }

  "a source scalameta cannot parse" should "fail the measure, naming it" in {
    val error = intercept[IllegalStateException](QualityReport.measure(Seq(QualityReport.SourceFile("Broken.scala", "object {"))))
    error.getMessage should include("Broken.scala:1")
  }
}
