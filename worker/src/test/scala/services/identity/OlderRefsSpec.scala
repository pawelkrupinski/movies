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

  // A tool's wrapper over the lookups, written in the oldest ref's terms, overrides no lookup a newer ref added: passed by
  // name to its source, the agreement's synopsis evidence reads the recording's overviews through it.
  "a wrapper's lookups" should "pass the members named to its source, and answer the rest itself" in {
    val words  = ProseWords.ofDescription(Seq("Binti meets Elias in Belgium."))
    val source = new IdentityLookups {
      def hasDetail(listing: Listing) = false
      def detail(listing: Listing) = Answer.Known(None)
      def candidates(query: CandidateQuery) = Answer.Known(Nil)
      def film(tmdbId: Int) = Answer.Unknown
      override def overview(tmdbId: Int) = Answer.Known(Some(words))
    }
    val wrapper = new IdentityLookups {
      def hasDetail(listing: Listing) = source.hasDetail(listing)
      def detail(listing: Listing) = source.detail(listing)
      def candidates(query: CandidateQuery) = source.candidates(query)
      def film(tmdbId: Int) = Answer.Known(None)   // its own answer
    }
    wrapper.overview(577285) shouldBe Answer.Known(None)
    val passing = OlderRefs.passing(classOf[IdentityLookups], wrapper, source, OlderRefs.NewerLookups)
    passing.overview(577285) shouldBe Answer.Known(Some(words))
    passing.film(577285) shouldBe Answer.Known(None)
  }

  // The wiring object the slice reads its agreement policy from was IdentityCutoverWiring before 2026-10-18: a slice
  // dropped into either kind of ref reaches it by name, and gets the same answers the worker wires.
  "the projection wiring" should "be found under this ref's name and answer as the worker's agreement does" in {
    OlderRefs.projectionWiring shouldBe modules.wiring.IdentityProjectionWiring
    OlderRefs.call(OlderRefs.projectionWiring, "identities", "pl") shouldBe Some(modules.wiring.IdentityProjectionWiring.identities("pl"))
    OlderRefs.call(OlderRefs.projectionWiring, "listedOn", "de") shouldBe Some(None)
  }
}
