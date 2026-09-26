package services

import settings.{MongoDatabaseName, MongoUri}
import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MongoAddressSpec extends AnyFlatSpec with Matchers {

  "MongoAddress.databaseFor" should "prefer an explicitly named database over the country's own" in {
    MongoAddress(Some(MongoUri("mongodb://localhost")), Some(MongoDatabaseName("kinowo_override_probe"))).databaseFor(Country.UnitedKingdom) shouldBe
      MongoDatabaseName("kinowo_override_probe")
  }

  it should "fall back to the country's own database when none is named" in {
    MongoAddress(Some(MongoUri("mongodb://localhost")), None).databaseFor(Country.UnitedKingdom) shouldBe MongoDatabaseName(Country.UnitedKingdom.mongoDb)
    MongoAddress.Disabled.databaseFor(Country.default) shouldBe MongoDatabaseName(Country.default.mongoDb)
  }
}
