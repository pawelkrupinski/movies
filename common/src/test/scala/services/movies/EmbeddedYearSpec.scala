package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The pure title→year extraction. A DELIMITED year (bracketed anywhere, or a
 *  trailing separator form) is read; a BARE year is never — it's title content.
 *  Fixed `max = 2027` so `Year.now()` drift can't flip the future-year cases. */
class EmbeddedYearSpec extends AnyFlatSpec with Matchers {
  private val max = 2027
  private def y(ts: String*) = EmbeddedYear.ofAll(ts, max)

  "EmbeddedYear" should "read a parenthesised year anywhere in the title" in {
    y("Generał (1926) 4K")            shouldBe Some(1926)
    y("KINO LETNIE 2026: Miś (1981)") shouldBe Some(1981) // bare 2026 ignored, (1981) read
    y("Requiem dla snu (2000)")       shouldBe Some(2000)
    y("Following (1998) - seans")     shouldBe Some(1998) // year is mid-title, not trailing
  }

  it should "read the other bracket shapes: square, curly, angle" in {
    y("Milczenie owiec [1991]") shouldBe Some(1991)
    y("Osiem i pół {1963}")     shouldBe Some(1963)
    y("Lawa <1989>")            shouldBe Some(1989)
  }

  it should "read a trailing year after a separator (comma / hyphen / en- / em-dash)" in {
    y("Pan Tadeusz - 1999")      shouldBe Some(1999)
    y("Pan Tadeusz, 1999")       shouldBe Some(1999)
    y("Pan Tadeusz – 1999") shouldBe Some(1999) // en dash
    y("Pan Tadeusz — 1999") shouldBe Some(1999) // em dash
  }

  it should "REFUSE a bare year — it is title content, not an annotation" in {
    y("2001: Odyseja kosmiczna")        shouldBe None // bare leading number
    y("Blade Runner 2049")              shouldBe None // bare trailing number
    y("1917")                           shouldBe None // bare, whole title
    y("SUMMER FALL FESTIVAL 2026")      shouldBe None // festival EDITION year (space-separated)
    y("André Rieu: Letni koncert 2026") shouldBe None // concert edition
    y("Rocky II 1979")                  shouldBe None // bare after a numeral
  }

  it should "abstain outside the plausible film range and on ambiguity" in {
    y("Rok (3000)")                shouldBe None // future, out of range
    y("Coś (1887)")           shouldBe None // before the first film (1888)
    y("Film (2018) (2020)")        shouldBe None // two distinct years in one title
    y("Film (2018)", "Inne (2020)")shouldBe None // two distinct years across titles
    y("No year here")              shouldBe None
  }

  it should "treat the same year twice as unambiguous, and scan across spellings" in {
    y("Film (2018) (2018)")        shouldBe Some(2018)
    // The canonical key strips the annotation, so the year survives only on a raw
    // slot title — collecting across spellings recovers it.
    y("Generał", "Generał (1926)") shouldBe Some(1926)
    y("Lawa [1989]", "Konwicki. Lawa")       shouldBe Some(1989)
  }
}
