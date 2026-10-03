package views

import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Cinema names reach the inline config `<script>` as JSON; a name carrying
 *  `</script>` must not be able to close the element early. */
class SharedJsConfigViewSpec extends AnyFlatSpec with Matchers {

  private given city: models.City = models.Country.Poland.cities.head

  "the inline JS config" should "keep a cinema name containing </script> inside its script element" in {
    val hostile = "Kino</script><script>alert(1)</script>"
    val html    = views.html._sharedJsConfig(Seq(hostile), Map(hostile -> "pill"), Set.empty).body
    html should not include "</script><script>alert(1)"
    // Still the same string once the browser parses the literal.
    html should include ("Kino\\u003c/script>\\u003cscript>alert(1)\\u003c/script>")
  }
}
