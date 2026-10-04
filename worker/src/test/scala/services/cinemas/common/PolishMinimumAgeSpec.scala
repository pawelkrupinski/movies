package services.cinemas.common

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every spelling the Polish venues give a minimum age in — read off their own
 *  pages (Kino Bajka Błonie, Kino Wars, Kino Eva, Kino Stylowy) — badges as "N+". */
class PolishMinimumAgeSpec extends AnyFlatSpec with Matchers {

  "AgeRating.polishMinimumAge" should "read every venue's spelling as N+" in {
    AgeRating.polishMinimumAge("Od lat: 12") shouldBe Some("12+")              // Kino Bajka Błonie
    AgeRating.polishMinimumAge("Akcja, komedia | Od lat 8 | 113 min.") shouldBe Some("8+") // Kino Wars
    AgeRating.polishMinimumAge("od 13 lat") shouldBe Some("13+")              // Kino Eva
    AgeRating.polishMinimumAge("od lat 10") shouldBe Some("10+")              // Kino Stylowy
  }

  it should "state nothing for a label without an age" in {
    AgeRating.polishMinimumAge("od lat ") shouldBe None
    AgeRating.polishMinimumAge("Bez ograniczeń") shouldBe None
    AgeRating.polishMinimumAge("113 min.") shouldBe None
  }
}
