package services

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Env

/** Every entrypoint that writes a country's database — the worker and the
 *  `worker/Test/runMain scripts.*` tools alike — opens it through
 *  [[MongoConnection.forCountry]], which claims it for that country. A German run pointed
 *  at Poland's database (`.env.local`'s `MONGODB_DB=kinowo`) is refused before it writes. */
class CountryDatabaseClaimIntegrationSpec extends AnyFlatSpec with Matchers {
  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  private val uri = Env.fromProcess().get("MONGODB_URI").get

  private val target = tools.IntegrationMongoTarget.fromEnv(Env.fromProcess()).get
  "MongoConnection.forCountry" should "refuse a database another country owns, and claim an unowned one" in
    tools.IntegrationCorpusDatabase.withDatabase(target, "country-db-claim") { db =>
      val polish = MongoConnection.forCountry(Country.Poland, MongoAddress(Some(uri), Some(db.name)), required = true, env = Env.fromProcess())
      try new DatabaseOwner(db).owner() shouldBe Some(Country.Poland.code)
      finally polish.close()

      val refused = the[IllegalStateException] thrownBy MongoConnection.forCountry(Country.Germany, MongoAddress(Some(uri), Some(db.name)), required = true, env = Env.fromProcess())
      refused.getMessage should include (Country.Poland.code)
    }
}
