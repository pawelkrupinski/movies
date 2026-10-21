package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{MainRoots, TestRoots, code, read, scalaFiles}

/**
 * The identity model reads a venue's films one way, wherever they come from: `services.identity.ModelRead`, prod's lean
 * read. A replay building its listings straight from a recording's rows (`Listing.corpus`, `Listing.all`) reads them
 * WHOLE, and a rule reading what the lean read drops measures cured while dead in prod — four were, 10-15 to 10-21
 * (17da3656b). Outside `ModelRead` itself, only a read of the take-up's own (already lean) listings may build them so.
 */
class ModelReadOnlySpec extends AnyFlatSpec with Matchers {

  private val WholeRead = """\bListing\.(corpus|all)\(""".r

  /** file → why it may: its input is already the lean read. */
  private val Allowed = Map(
    "identity/src/main/scala/services/identity/ModelRead.scala" -> "the one read",
    "identity/src/main/scala/services/identity/IdentityModel.scala" -> "Listing.corpus's own definition",
    "worker/src/main/scala/modules/WorkerWiring.scala" -> "the take-up's listings, read lean by IdentityListingIntake.identities")

  "a listing set the identity model or a measurement resolves" should "be read through ModelRead, never whole off a recording" in {
    val roots = MainRoots ++ TestRoots.filterNot(Set("common/src/test", "identity/src/test", "scrapers/src/test", "web/src/test"))
    val found = scalaFiles(roots).filterNot(p => p.toString.endsWith("Spec.scala") || Allowed.keySet(p.toString)).flatMap { path =>
      read(path).linesIterator.zipWithIndex.collect { case (line, i) if WholeRead.findFirstIn(code(line)).isDefined => s"$path:${i + 1}: ${line.trim}" }
    }
    withClue("read through services.identity.ModelRead (lean, as prod), or say here why the input is already lean:\n" + found.mkString("\n"))(
      found shouldBe empty)
  }
}
