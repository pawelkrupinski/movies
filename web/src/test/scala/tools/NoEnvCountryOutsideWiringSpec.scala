package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/**
 * The web reads the deployment's country from the environment ONCE, in the composition
 * root (the `modules` package: `Wiring.country`, `AppLoader`), and hands it to every component
 * that differs by country.
 *
 * A `Country.fromEnv` anywhere else is a second, private answer to "which country is
 * this?" that a test wiring cannot override: `CountryIsolationMatrixSpec` boots the UK
 * wiring in a process whose environment still says Poland, and a template reading the
 * environment rendered Poland's brand, og:locale and JSON-LD into the UK's landing.
 * Unset, `fromEnv` IS Poland, so the leak is silent in every single-country test.
 *
 * Scala and Twirl sources alike; comment lines (`//`, `*`, `@*`) are ignored so a
 * comment may still name the method historically.
 */
class NoEnvCountryOutsideWiringSpec extends AnyFlatSpec with Matchers {

  // Forked tests run in `web/`, unforked ones at the repository root.
  private val webMain: Path =
    Seq(Paths.get("web/src/main"), Paths.get("src/main")).find(Files.isDirectory(_))
      .getOrElse(fail("web/src/main not found from " + Paths.get("").toAbsolutePath))

  private val CompositionRoot = webMain.resolve("scala/modules")
  private val EnvCountry      = """\bCountry\.fromEnv\b""".r

  private def isComment(line: String): Boolean = {
    val t = line.trim
    t.startsWith("//") || t.startsWith("*") || t.startsWith("/*") || t.startsWith("@*")
  }

  private lazy val offenders: Seq[String] =
    Seq(webMain.resolve("scala"), webMain.resolve("twirl")).filter(Files.isDirectory(_)).flatMap { root =>
      Files.walk(root).iterator.asScala
        .filter(p => p.toString.endsWith(".scala") || p.toString.endsWith(".html"))
        .filterNot(_.startsWith(CompositionRoot))
        .toSeq
    }.sortBy(_.toString).flatMap { path =>
      new String(Files.readAllBytes(path), StandardCharsets.UTF_8).linesIterator.zipWithIndex.collect {
        case (line, i) if !isComment(line) && EnvCountry.findFirstIn(line).isDefined =>
          s"${webMain.relativize(path)}:${i + 1}: ${line.trim}"
      }
    }

  "Web code outside the composition root" should "take its country from the wiring, never the environment" in {
    withClue("These lines read the deployment's country from the environment. Thread it from " +
      "`Wiring.country` through the controller instead:\n" + offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }
}
