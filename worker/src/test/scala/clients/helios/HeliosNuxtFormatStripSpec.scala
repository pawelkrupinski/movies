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

  // Helios's strand tags are single-space literals ("- Salon Kultury Helios"),
  // but the live NUXT sometimes emits a DOUBLE space after the dash ("Odyseja -
  //  Salon Kultury Helios"), which the end-anchored literal never matched — the
  // whole Helios chain served that film as a separate "Odyseja - Salon Kultury
  // Helios" row instead of merging onto "Odyseja". The strip must tolerate
  // irregular whitespace around the separator and tag words.
  it should "strip the 'Salon Kultury Helios' strand tag even with a doubled space" in {
    HeliosNuxt.cleanTitle("Odyseja -  Salon Kultury Helios") shouldBe "Odyseja"
  }

  it should "strip the 'Salon Kultury Helios' strand tag with a single space" in {
    HeliosNuxt.cleanTitle("Odyseja - Salon Kultury Helios") shouldBe "Odyseja"
  }
}
