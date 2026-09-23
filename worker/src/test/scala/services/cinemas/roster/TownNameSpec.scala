package services.cinemas.roster

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TownNameSpec extends AnyFlatSpec with Matchers {

  "TownName.same" should "ignore case, diacritics and separators" in {
    TownName.same("Jastrzębie Zdrój", "jastrzebie-zdroj") shouldBe true
    TownName.same("Ostrowiec Świętokrzyski", "Ostrowiec+Świętokrzyski") shouldBe true
  }

  it should "expand an abbreviated qualifier rather than drop it" in {
    TownName.same("Ostrów Wlkp.", "Ostrów Wielkopolski") shouldBe true
    TownName.same("Środa Wlkp.", "Środa Wielkopolska") shouldBe true
    TownName.same("Ostrów Wlkp.", "Ostrów Mazowiecka") shouldBe false
  }

  it should "accept a qualifier one side left off" in {
    TownName.same("Połczyn", "Połczyn-Zdrój") shouldBe true
  }

  it should "tell different towns apart" in {
    TownName.same("Konin", "Koło") shouldBe false
    TownName.same("Braniewo", "Środa Wielkopolska") shouldBe false
    TownName.same("", "Konin") shouldBe false
  }
}
