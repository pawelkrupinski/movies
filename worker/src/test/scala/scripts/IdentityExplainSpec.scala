package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** `scripts/identity-explain`'s arguments and listing selection ([[IdentityExplain]]): substrings, case and accents aside. */
class IdentityExplainSpec extends AnyFlatSpec with Matchers {

  "identity-explain" should "read a country and its selectors, the capture and labels it is handed" in {
    IdentityExplain.parse(List("DE", "--venue", "cinestar", "--title", "rückkehr", "--fixture", "/c/de.json.gz", "--labels", "/c/identity-judgements.tsv",
      "--no-resolve", "--limit", "3")) shouldBe
      IdentityExplain.Options("de", Some("cinestar"), Some("rückkehr"), Some(Path.of("/c/de.json.gz")), Some(Path.of("/c/identity-judgements.tsv")),
        resolve = false, limit = 3)
    an[RuntimeException] should be thrownBy IdentityExplain.parse(List("de", "--colour"))
  }

  it should "fold case and accents, so a plain-ASCII selector finds the billing" in {
    IdentityExplain.folded("Herr der Ringe - Rückkehr des Königs (ext.)") should include("ruckkehr des konigs")
    IdentityExplain.folded("Łódź") shouldBe "lodz"
  }
}
