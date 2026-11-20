package models

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.security.MessageDigest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Pins every hand-declared venue (`case object`) exactly as `Source.all` sees it -- its name, class, display and pill
 * names, slug, opening and hash -- and the order of `Source.all` and `Cinema.byCity`, against a checked-in table
 * (`test/resources/fixtures/models/cinema-declarations.tsv`): a change that only moves the declarations between files
 * (2026-11-20: `Cinema.scala` split by region) must leave it alike. The table as it reads today is written to `common/target/cinema-declarations.actual.tsv` on every run; a
 * change that MEANS to alter a venue copies it over the checked-in one.
 */
class CinemaDeclarationsSpec extends AnyFlatSpec with Matchers {
  private def digest(lines: Seq[String]): String =
    MessageDigest.getInstance("SHA-256").digest(lines.mkString("\n").getBytes(StandardCharsets.UTF_8))
      .map(b => f"$b%02x").mkString

  private def actual: Seq[String] = {
    val declared = Source.all.collect { case cinema: Cinema if cinema.getClass.getName.endsWith("$") => cinema }
    val rows = declared.map { c =>
      Seq(c.toString, c.getClass.getName, c.displayName, c.pillName, c.slug, c.opening.toString, c.hashCode.toString)
        .mkString("\t")
    }
    Seq(
      s"# Source.all order\t${digest(Source.all.map(_.displayName))}",
      s"# Cinema.byCity order\t${digest(Cinema.byCity.map { case (city, cinemas) => s"$city\t${cinemas.map(_.displayName).mkString("\t")}" })}",
    ) ++ rows
  }

  "every hand-declared venue" should "keep its name, fields and place in the roster's order" in {
    val lines = actual
    val target = Paths.get("common/target/cinema-declarations.actual.tsv")
    Files.createDirectories(target.getParent)
    Files.write(target, (lines.mkString("\n") + "\n").getBytes(StandardCharsets.UTF_8))
    val expected = Files.readAllLines(Paths.get("test/resources/fixtures/models/cinema-declarations.tsv")).toArray.toSeq
    lines.size should be > 480
    lines.diff(expected) shouldBe empty
    expected.diff(lines) shouldBe empty
    lines shouldBe expected
  }
}
