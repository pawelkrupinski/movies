package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/** FilmwebDiff's arguments: the weekly workflow's line reads, and what it cannot read stops it before any fetch. */
class FilmwebDiffArgumentsSpec extends AnyFlatSpec with Matchers {

  private val slugs = models.Country.Poland.cities.map(_.slug).toSet

  "FilmwebDiff" should "read the weekly workflow's own command line: everything, no window end" in {
    val workflow = Files.readString(Paths.get(".github/workflows/filmweb-diff.yml"), StandardCharsets.UTF_8)
    val args = """runMain tools\.FilmwebDiff ([^"]*)"""".r.findFirstMatchIn(workflow).map(_.group(1).trim.split("\\s+").toSeq).get
    FilmwebDiff.parse(args, slugs) shouldBe
      Right(FilmwebDiff.Options(None, "filmweb-diff.txt", "filmweb-diff.csv", "filmweb-diff.json"))
  }

  it should "read the paths in any order, default what is left out, and restrict to the cities named" in {
    FilmwebDiff.parse(Seq("out.csv", "poznan"), slugs) shouldBe Right(FilmwebDiff.Options(csv = "out.csv", cities = Set("poznan")))
    FilmwebDiff.parse(Nil, slugs) shouldBe Right(FilmwebDiff.Options())
  }

  it should "read a leading days count as a window end, and `all` as none" in {
    FilmwebDiff.parse(Seq("14", "out.txt"), slugs) shouldBe Right(FilmwebDiff.Options(daysAhead = Some(14), txt = "out.txt"))
    FilmwebDiff.parse(Seq("all", "out.txt"), slugs) shouldBe Right(FilmwebDiff.Options(daysAhead = None, txt = "out.txt"))
  }

  it should "refuse a city, path or days count it cannot read rather than diff something else" in {
    // the old reading dropped `pozan` and diffed every city; took `out.txt` given first for the days and lost it
    FilmwebDiff.parse(Seq("3", "pozan"), slugs).isLeft shouldBe true
    FilmwebDiff.parse(Seq("three", "out.txt"), slugs).isLeft shouldBe true
    FilmwebDiff.parse(Seq("out.txt"), slugs) shouldBe Right(FilmwebDiff.Options(txt = "out.txt"))
    FilmwebDiff.parse(Seq("a.txt", "b.txt"), slugs).isLeft shouldBe true
    FilmwebDiff.parse(Seq("3", "/tmp/report"), slugs).isLeft shouldBe true
  }
}
