package services

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

class MongoAddressSpec extends AnyFlatSpec with Matchers {

  "MongoAddress.databaseFor" should "prefer an explicitly named database over the country's own" in {
    MongoAddress(Some("mongodb://localhost"), Some("kinowo_override_probe")).databaseFor(Country.UnitedKingdom) shouldBe
      "kinowo_override_probe"
  }

  it should "fall back to the country's own database when none is named" in {
    MongoAddress(Some("mongodb://localhost"), None).databaseFor(Country.UnitedKingdom) shouldBe Country.UnitedKingdom.mongoDb
    MongoAddress.Disabled.databaseFor(Country.default) shouldBe Country.default.mongoDb
  }

  "MongoAddress.fromEnv" should "read the cluster and the explicit database from MONGODB_URI / MONGODB_DB" in {
    MongoAddress.fromEnv(Env.of("MONGODB_URI" -> "mongodb://probe:1", "MONGODB_DB" -> "kinowo_probe")) shouldBe
      MongoAddress(Some("mongodb://probe:1"), Some("kinowo_probe"))
  }

  // KINOWO_COUNTRY is not part of an address: which country's database to open is the
  // caller's, handed in, so the environment cannot quietly pick it a second time.
  it should "be the disabled address when neither is set, whatever country the environment names" in {
    MongoAddress.fromEnv(Env.of("KINOWO_COUNTRY" -> "uk")) shouldBe MongoAddress.Disabled
  }
}
