package services.titlerules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** Unit tests for the `{{NAME}}` placeholder substitution — the literal-safe
 *  fixpoint expansion that lets rules reference shared regex snippets. */
class PlaceholderExpanderSpec extends AnyFlatSpec with Matchers {
  import PlaceholderExpander.expand

  "expand" should "substitute every occurrence of a known token" in {
    expand("{{SEP}}DKF{{SEP}}end", Map("SEP" -> "X")) shouldBe "XDKFXend"
  }

  it should "leave a pattern with no token untouched" in {
    expand("(?i)^Klub: ", Map("SEP" -> "X")) shouldBe "(?i)^Klub: "
  }

  it should "leave an UNKNOWN token verbatim (so the rule then fails to compile and surfaces)" in {
    expand("{{NOPE}}foo", Map("SEP" -> "X")) shouldBe "{{NOPE}}foo"
  }

  it should "treat the expansion literally — a `$1` or `\\` in the snippet is not a regex replacement" in {
    expand("a{{X}}b", Map("X" -> """\s*$1""")) shouldBe """a\s*$1b"""
  }

  it should "expand a placeholder that references another, to a fixpoint" in {
    expand("{{A}}", Map("A" -> "{{B}}-", "B" -> "z")) shouldBe "z-"
  }

  it should "not hang on a cycle — the depth cap leaves a residual token" in {
    val out = expand("{{A}}", Map("A" -> "{{B}}", "B" -> "{{A}}"), maxDepth = 5)
    out should (include ("{{A}}") or include ("{{B}}"))   // bounded, not an infinite loop
  }

  it should "no-op with an empty placeholder map" in {
    expand("{{SEP}}x", Map.empty) shouldBe "{{SEP}}x"
  }

  // The hardcoded SEP separator is the whole point: one token covering every
  // banner separator the rules used to spell out by hand.
  "the default SEP placeholder" should "expand to a class matching every separator variant, spaces optional" in {
    val sep: Regex = ("^" + expand("{{SEP}}") + "$").r   // expand() defaults to TitleRulePlaceholders.all
    for (s <- Seq(":", " : ", "|", " | ", "-", " - ", "–", "—", "/", "_", ": "))
      withClue(s"separator '$s' should match: ") { sep.matches(s) shouldBe true }
    // A non-separator run is not a separator.
    sep.matches("x") shouldBe false
  }
}
