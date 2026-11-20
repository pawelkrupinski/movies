package quality

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** Each method's span, keyed as the quality ratchets key it: what scripts/touched_methods.py meets a diff's hunks with. */
class MethodSpansSpec extends AnyFlatSpec with Matchers {

  private val source =
    """package example
      |
      |object Genres {
      |  def parse(text: String): Seq[String] = {
      |    def clean(s: String) = s.trim
      |    text.split(',').map(clean).toSeq
      |  }
      |
      |  def parse(texts: Seq[String]): Seq[String] =
      |    texts.flatMap(parse)
      |
      |  final class Reader {
      |    def read(s: String): Int = s.length
      |  }
      |}
      |""".stripMargin

  "a source's spans" should "be its outermost defs, keyed by owner, a repeated name numbered, with their first and last lines" in {
    MethodSpans.of("scrapers/src/main/scala/example/Genres.scala", source).fold(fail(_), identity).map(s => (s.key, s.startLine, s.endLine)) shouldBe Seq(
      ("scrapers/src/main/scala/example/Genres.scala:Genres.parse", 4, 7),
      ("scrapers/src/main/scala/example/Genres.scala:Genres.parse#2", 9, 10),
      ("scrapers/src/main/scala/example/Genres.scala:Genres.Reader.read", 13, 13))
  }

  it should "name a source it cannot parse" in {
    MethodSpans.of("Broken.scala", "object Broken { def x( = 1 }").left.map(_.takeWhile(_ != ':')) shouldBe Left("Broken.scala")
  }

  "the tool" should "write one row a method of each source it is given" in {
    val root = Files.createTempDirectory("method-spans")
    try {
      Files.createDirectories(root.resolve("lib"))
      Files.write(root.resolve("lib/Genres.scala"), source.getBytes(StandardCharsets.UTF_8))
      val out = root.resolve("spans.tsv")
      MethodSpans.main(Array("--root", root.toString, "--out", out.toString, "lib/Genres.scala"))
      new String(Files.readAllBytes(out), StandardCharsets.UTF_8).linesIterator.toSeq shouldBe Seq(
        "lib/Genres.scala\tlib/Genres.scala:Genres.parse\t4\t7",
        "lib/Genres.scala\tlib/Genres.scala:Genres.parse#2\t9\t10",
        "lib/Genres.scala\tlib/Genres.scala:Genres.Reader.read\t13\t13")
    } finally Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
  }
}
