package clients.helios

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.pl.HeliosNuxt

/** Helios bakes a screen-format/version tail into some Nuxt titles
 *  ("Babystar - 2D NAP"). The 2D/NAP it implies is already parsed into
 *  `Showtime.format` from each screening's `release`, so `cleanTitle` peels the
 *  tail off the title (it loses nothing) and the dub/napisy variants collapse
 *  onto one bare-film row. */
class HeliosNuxtFormatStripSpec extends AnyFlatSpec with Matchers {

  "HeliosNuxt.cleanTitle" should "strip a trailing '- 2D NAP' screen-format tag" in {
    HeliosNuxt.cleanTitle("Babystar - 2D NAP") shouldBe "Babystar"
  }

  it should "leave a plain title untouched" in {
    HeliosNuxt.cleanTitle("Babystar") shouldBe "Babystar"
  }
}
