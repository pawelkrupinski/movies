package services.titles

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PersonKeySpec extends AnyFlatSpec with Matchers {

  "PersonKey.of" should "be one key for a name in either word order" in {
    PersonKey.of("Makoto Shinkai") shouldBe PersonKey.of("Shinkai Makoto")
  }

  // `deburr` leaves letters NFD does not decompose, and the token split then dropped them: "Fatih Akın" keyed as
  // "ak fatih", another person than "Fatih Akin" -- where SamePerson already folds them.
  it should "fold the letters NFD leaves whole, as SamePerson does" in {
    PersonKey.of("Fatih Akın")        shouldBe PersonKey.of("Fatih Akin")
    PersonKey.of("Søren Kragh-Jacobsen") shouldBe PersonKey.of("Soren Kragh-Jacobsen")
    PersonKey.of("Maren Ade")         should not be PersonKey.of("Maren Ide")
  }
}
