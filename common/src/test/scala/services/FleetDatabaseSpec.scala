package services

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import settings.{FleetDatabaseName, MongoDatabaseName, ProcessConfiguration}
import tools.Env

/** Where every country's web and worker keep the fleet's shared state: `kinowo_fleet` by default
 *  since the move out of Poland's `kinowo` (docs/fleet-database-migration.md), and never a
 *  country's corpus database, Poland's included. */
class FleetDatabaseSpec extends AnyFlatSpec with Matchers {

  private def fromEnv(pairs: (String, String)*): MongoDatabaseName =
    FleetDatabase.name(new ProcessConfiguration(Env.of(pairs*)).fleetDatabase)

  "FleetDatabase.name" should "read MONGODB_FLEET_DB from the environment" in {
    fromEnv("MONGODB_FLEET_DB" -> "kinowo_fleet") shouldBe MongoDatabaseName("kinowo_fleet")
  }

  it should "default to the fleet's own database when MONGODB_FLEET_DB is unset" in {
    fromEnv("KINOWO_COUNTRY" -> "uk") shouldBe MongoDatabaseName("kinowo_fleet")
  }

  // Blank is how a ConfigMap switches a setting off: the key stays, the value empties.
  it should "treat a blank value as unset and trim a padded one" in {
    FleetDatabase.name(Some(FleetDatabaseName("   "))) shouldBe FleetDatabase.Dedicated
    FleetDatabase.name(Some(FleetDatabaseName(" kinowo_fleet "))) shouldBe FleetDatabase.Dedicated
  }

  // `kinowo` was the rollback while the move was pending; after it, naming it would split the
  // fleet's queue and quota between two databases, so it fails the boot like every other corpus.
  it should "refuse every country's corpus database, Poland's included" in {
    models.Country.all.foreach { country =>
      withClue(country.code) {
        an[IllegalArgumentException] should be thrownBy FleetDatabase.name(Some(FleetDatabaseName(country.mongoDb)))
      }
    }
  }

  it should "name a dedicated database that is no country's corpus" in {
    models.Country.all.map(_.mongoDb) should not contain FleetDatabase.Dedicated.value
  }
}
