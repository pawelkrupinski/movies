package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._

/**
 * Every stage asks the ONE constraint model, `ListingConstraints`, whether two pieces of
 * listing evidence may be one film — never a `MixedFilmDetector` veto directly.
 *
 * Five stages once composed the same predicates five ways (the TMDB candidate veto, the
 * decoration veto, the Faust containment refusal, the staging fold's denying venue, the
 * convergence harness's wrong-merge check), so a rule narrowed in one stage kept firing in the
 * others, and the identity resolver had no single rule set to draw its edges from. A new call
 * site that asks a predicate directly is the drift this guards against: route it through
 * `ListingConstraints` (adding the constraint there, with its reason, if it is new).
 */
class ListingConstraintsRoutingSpec extends AnyFlatSpec with Matchers {

  private val Roots = Seq("common/src/main", "worker/src/main", "web/src/main", "worker/src/fixtures")

  /** The files allowed to name the predicates: where they are defined, and the model itself. */
  private val Owners = Set(
    "common/src/main/scala/services/movies/MixedFilmDetector.scala",
    "common/src/main/scala/services/movies/ListingConstraints.scala")

  private val Vetoes = Seq("deniesFilm", "listingDeniesFilm", "wouldAddASecondFilm", "describeDifferentFilms")
  private val Direct = s"""MixedFilmDetector\\s*\\.\\s*(${Vetoes.mkString("|")})\\b""".r

  private def sources(root: String): Seq[Path] = {
    val dir = Paths.get(root)
    if (!Files.isDirectory(dir)) Nil
    else {
      val walk = Files.walk(dir)
      try walk.iterator().asScala.filter(_.toString.endsWith(".scala")).toSeq finally walk.close()
    }
  }

  /** Code, not commentary: a doc comment citing the predicate by name is fine. */
  private def isComment(line: String): Boolean = {
    val t = line.trim
    t.startsWith("//") || t.startsWith("*") || t.startsWith("/*")
  }

  "production code" should "ask ListingConstraints, never a MixedFilmDetector veto directly" in {
    val offenders = Roots.flatMap(sources).filterNot(p => Owners.contains(p.toString)).flatMap { path =>
      Files.readAllLines(path).asScala.zipWithIndex.collect {
        case (line, i) if !isComment(line) && Direct.findFirstIn(line).isDefined => s"$path:${i + 1}: ${line.trim}"
      }
    }
    withClue("route these through ListingConstraints:\n" + offenders.mkString("\n") + "\n")(offenders shouldBe empty)
  }

  it should "find the files it guards" in {
    // A positive control: a moved tree would make the guard pass vacuously.
    Roots.flatMap(sources).map(_.toString) should contain allOf (
      "common/src/main/scala/services/movies/FilmCanonicalizer.scala",
      "worker/src/main/scala/services/movies/MovieService.scala",
      "worker/src/fixtures/scala/tools/ServedCorpusInvariants.scala")
  }
}
