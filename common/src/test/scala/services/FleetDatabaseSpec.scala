package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import settings.{FleetDatabaseName, MongoDatabaseName, ProcessConfiguration}
import tools.Env

/** Where every country's web and worker keep the fleet's shared state. The fallback to Poland's
 *  `kinowo` is what lets this ship before the fleet's collections are copied out of it; the
 *  setting is what moves them, and the refusal keeps them from landing beside another corpus. */
class FleetDatabaseSpec extends AnyFlatSpec with Matchers {

  private def fromEnv(pairs: (String, String)*): MongoDatabaseName =
    FleetDatabase.name(new ProcessConfiguration(Env.of(pairs*)).fleetDatabase)

  "FleetDatabase.name" should "read MONGODB_FLEET_DB from the environment" in {
    fromEnv("MONGODB_FLEET_DB" -> "kinowo_fleet") shouldBe MongoDatabaseName("kinowo_fleet")
  }

  it should "stay on Poland's database while MONGODB_FLEET_DB is unset, so a deploy before the copy changes nothing" in {
    fromEnv("KINOWO_COUNTRY" -> "uk") shouldBe MongoDatabaseName("kinowo")
  }

  // Blank is how a ConfigMap switches a setting off: the key stays, the value empties.
  it should "treat a blank value as unset and trim a padded one" in {
    FleetDatabase.name(Some(FleetDatabaseName("   "))) shouldBe FleetDatabase.Legacy
    FleetDatabase.name(Some(FleetDatabaseName(" kinowo_fleet "))) shouldBe FleetDatabase.Dedicated
  }

  it should "accept Poland's database by name, the rollback while the move is pending" in {
    FleetDatabase.name(Some(FleetDatabaseName("kinowo"))) shouldBe FleetDatabase.Legacy
  }

  it should "refuse any other country's corpus database" in {
    models.Country.all.filterNot(_ == models.Country.Poland).foreach { country =>
      withClue(country.code) {
        an[IllegalArgumentException] should be thrownBy FleetDatabase.name(Some(FleetDatabaseName(country.mongoDb)))
      }
    }
  }

  it should "name a dedicated database that is no country's corpus" in {
    models.Country.all.map(_.mongoDb) should not contain FleetDatabase.Dedicated.value
  }
}
