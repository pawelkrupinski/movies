package services

import settings.{FleetDatabaseName, MongoDatabaseName}

/** The database every country's deployment shares: the fleet's Facebook re-scrape queue and
 *  quota (`facebook_rescrapes`), the film pages Facebook has fetched (`facebook_fetches`) and the
 *  fleet-wide host paces (`fleet_host_pace`) — the [[MongoCollections]] entries in [[CollectionDatabase.Fleet]].
 *
 *  WHICH DATABASE: `MONGODB_FLEET_DB` when set, else [[Dedicated]], `kinowo_fleet`. The fleet
 *  started in Poland's own `kinowo`, because the deployments' Mongo user held rights only on the
 *  country databases. Beside the Polish corpus, a cleanup of a fleet collection was one wrong
 *  database away from deleting Poland's live data (2026-10-24), so the fleet moved: granted,
 *  copied, and switched by setting on 2026-10-25 (`docs/fleet-database-migration.md`), then made
 *  the default here once the soak was over.
 *
 *  A COUNTRY'S CORPUS IS REFUSED, Poland's included: pointing the fleet at one would put the
 *  fleet's state back beside a corpus, the very shape that was undone. `MONGODB_FLEET_DB=kinowo`,
 *  the rollback while the move was pending, now fails the boot rather than splitting the queue. */
object FleetDatabase {

  /** The fleet's own database, and the default. */
  val Dedicated: MongoDatabaseName = MongoDatabaseName("kinowo_fleet")

  /** The fleet database `configured` (`MONGODB_FLEET_DB`) names, or [[Dedicated]] when it names none. */
  def name(configured: Option[FleetDatabaseName]): MongoDatabaseName =
    configured.map(_.value.trim).filter(_.nonEmpty).fold(Dedicated) { value =>
      require(!models.Country.all.exists(_.mongoDb == value),
        s"MONGODB_FLEET_DB=$value is a country's corpus database; the fleet's state does not belong beside one " +
          s"(use ${Dedicated.value})")
      MongoDatabaseName(value)
    }
}
