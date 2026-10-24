package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.compiletime.testing.typeCheckErrors

object ConstructByTypeSpec {
  trait Handler
  final class LoudHandler extends Handler
  final class Listing
  final class Detail

  // The shape of Play's generated router: a primary constructor ending in a prefix, and a shorter one without it.
  final class Router(val handler: Handler, val listing: Listing, val detail: Detail, val prefix: String) {
    def this(handler: Handler, listing: Listing, detail: Detail) = this(handler, listing, detail, "/")
  }
}

class ConstructByTypeSpec extends AnyFlatSpec with Matchers {
  import ConstructByTypeSpec.*

  "ConstructByType" should "hand every argument to the parameter of its type, whatever order they come in" in {
    val handler = new LoudHandler
    val listing = new Listing
    val detail  = new Detail
    val router  = ConstructByType[Router](detail, handler, listing)
    (router.handler, router.listing, router.detail, router.prefix) shouldBe ((handler, listing, detail, "/"))
  }

  it should "use the longer constructor when an argument only it takes is given" in {
    ConstructByType[Router](new Listing, "/uk", new Detail, new LoudHandler).prefix shouldBe "/uk"
  }

  it should "leave the parameters nothing fits null when asked to construct partially" in {
    val listing = new Listing
    val router  = ConstructByType.partially[Router](listing)
    (router.handler, router.listing, router.detail, router.prefix) shouldBe ((null, listing, null, "/"))
  }

  it should "refuse to compile a missing, an ambiguous or an unused argument" in {
    typeCheckErrors("ConstructByType[ConstructByTypeSpec.Router](new ConstructByTypeSpec.Listing)")
      .map(_.message).mkString should include("no argument of type")
    typeCheckErrors("""ConstructByType[ConstructByTypeSpec.Router](new ConstructByTypeSpec.Listing, new ConstructByTypeSpec.Listing,
      new ConstructByTypeSpec.Detail, new ConstructByTypeSpec.LoudHandler)""").map(_.message).mkString should include(
      "more than one argument fits")
    typeCheckErrors("ConstructByType.partially[ConstructByTypeSpec.Router](new ConstructByTypeSpec.Listing, 42)")
      .map(_.message).mkString should include("no parameter takes")
  }
}
