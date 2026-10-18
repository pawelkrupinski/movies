package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A member read by name is its value where the ref's code has it and None where it does not — never a compile error
 *  (the compile into old refs itself: scripts/identity-slice-old-ref-test.sh). */
class OlderRefsSpec extends AnyFlatSpec with Matchers {
  private final case class Billing(title: String, properTitle: Option[String])
  private final class Rules { def peel(title: String): String = title.stripSuffix(" (18)"); val facts: String = "none" }

  "a field read by name" should "be its value where the class has it" in {
    OlderRefs.field(Billing("NT Live: Hamlet", None), "title") shouldBe Some("NT Live: Hamlet")
    OlderRefs.optionalText(Billing("NT Live: Hamlet", Some("National Theatre Live: Hamlet")), "properTitle") shouldBe
      Some("National Theatre Live: Hamlet")
  }

  it should "be None where it holds none or the class has no such field" in {
    OlderRefs.optionalText(Billing("Lalka", None), "properTitle") shouldBe None
    OlderRefs.field(Billing("Lalka", None), "searchTitle") shouldBe None
    OlderRefs.optionalText(Billing("Lalka", None), "title") shouldBe None // not optional text
  }

  "a member called by name" should "be called, with its arguments, where the class has it" in {
    OlderRefs.call(new Rules, "peel", "Obsession (18)") shouldBe Some("Obsession")
    OlderRefs.call(new Rules, "facts") shouldBe Some("none")
  }

  it should "be None where the class has no such member, or none taking that many arguments" in {
    OlderRefs.call(new Rules, "titleFacts", "Lalka") shouldBe None
    OlderRefs.call(new Rules, "peel") shouldBe None
  }
}
