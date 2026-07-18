package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.MsiClient

/** Kino Wybrzeże (RCK Kołobrzeg, an MSI portal) glues a constant `KINO WYBRZEŻE`
 *  venue label onto every title, with the screening's format word buried just
 *  before it. `MsiClient.cleanTitleStripSuffix` strips that label so the bare
 *  film resolves. The venue is INCONSISTENT about the separator — most rows use
 *  a dash ("DRUGIE ŻYCIE-KINO WYBRZEŻE") but some use `=`
 *  ("SUPERGIRL NAPISY=KINO WYBRZEŻE"), and the dash-only class let the `=` form
 *  through to a no-match key in prod. */
class MsiClientVenueSuffixSpec extends AnyFlatSpec with Matchers {

  private val venue   = "KINO WYBRZEŻE"
  private val strip   = MsiClient.cleanTitleStripSuffix(venue)

  "MsiClient.cleanTitleStripSuffix" should "strip the venue label glued on with '=' just like the dash form" in {
    // The real prod no-match row that the dash-only separator class missed.
    val viaEq   = strip("SUPERGIRL NAPISY=KINO WYBRZEŻE")._1
    val viaDash = strip("SUPERGIRL NAPISY-KINO WYBRZEŻE")._1
    viaEq shouldBe viaDash
    viaEq should not include "WYBRZEŻE"
    viaEq should not include "NAPISY"        // the buried format word is recovered, not left in the title
  }

  it should "keep stripping the existing dash / en-dash / em-dash separators" in {
    Seq("DRUGIE ŻYCIE-KINO WYBRZEŻE", "DRUGIE ŻYCIE – KINO WYBRZEŻE").foreach { raw =>
      withClue(s"strip('$raw'): ")(strip(raw)._1 should not include "WYBRZEŻE")
    }
  }

  it should "leave a title without the venue label untouched" in {
    // No '= / - KINO WYBRZEŻE' suffix → the strip is a no-op and just delegates
    // to cleanTitle (the '=' widening must not eat an unrelated title).
    strip("Top Gun: Maverick") shouldBe MsiClient.cleanTitle("Top Gun: Maverick")
  }
}
