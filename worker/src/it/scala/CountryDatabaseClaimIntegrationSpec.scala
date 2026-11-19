package services

import settings.MongoTuning

import settings.MongoAddress
import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every entrypoint that writes a country's database — the worker and the
 *  `worker/Test/runMain scripts.*` tools alike — opens it through
 *  [[MongoConnection.forCountry]], which claims it for that country. A German run pointed
 *  at Poland's database (`.env.local`'s `MONGODB_DB=kinowo`) is refused before it writes. */
class CountryDatabaseClaimIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  "MongoConnection.forCountry" should "refuse a database another country owns, and claim an unowned one" in
    tools.IntegrationCorpusDatabase.withDatabase(mongoTarget, "country-db-claim") { db =>
      val polish = MongoConnection.forCountry(MongoAddress(Some(mongoTarget.uri), Some(settings.MongoDatabaseName(db.name))).connectionFor(Country.Poland, MongoTuning.Default), services.MongoRequirement.Required, Country.Poland)
      try new DatabaseOwner(db).owner() shouldBe Some(Country.Poland.code)
      finally polish.close()

      val refused = the[IllegalStateException] thrownBy MongoConnection.forCountry(MongoAddress(Some(mongoTarget.uri), Some(settings.MongoDatabaseName(db.name))).connectionFor(Country.Germany, MongoTuning.Default), services.MongoRequirement.Required, Country.Germany)
      refused.getMessage should include (Country.Poland.code)
    }
}
