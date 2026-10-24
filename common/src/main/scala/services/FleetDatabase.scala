package services

import settings.{FleetDatabaseName, MongoDatabaseName}

/** The database every country's deployment shares: the fleet's Facebook re-scrape queue and
 *  quota (`facebook_rescrapes`), the film pages Facebook has fetched (`facebook_fetches`) and the
 *  fleet-wide host paces (`fleet_host_pace`).
 *
 *  WHICH DATABASE: `MONGODB_FLEET_DB` when set, else [[Legacy]], Poland's own `kinowo`. The fleet
 *  started there because the deployments' Mongo user held rights only on the country databases,
 *  so a new database failed every write `Unauthorized` until the server granted it. Beside the
 *  Polish corpus, a cleanup of a fleet collection is one wrong database away from deleting
 *  Poland's live data (2026-10-24), so the fleet moves to [[Dedicated]]: grant, copy, then set
 *  `MONGODB_FLEET_DB`. The ordered steps are in `docs/fleet-database-migration.md`. Unset, every
 *  process keeps reading `kinowo`, which is what makes this code safe to deploy before the copy.
 *
 *  A COUNTRY'S CORPUS IS REFUSED, Poland's excepted while the move is pending: pointing the fleet
 *  at `kinowo_uk` would put the fleet's state beside another corpus, the very shape being undone. */
object FleetDatabase {

  /** Where the fleet lived before it had a database of its own, and still lives while `MONGODB_FLEET_DB` is unset. */
  val Legacy: MongoDatabaseName = MongoDatabaseName(models.Country.Poland.mongoDb)

  /** The fleet's own database, which `MONGODB_FLEET_DB` names once the collections are copied. */
  val Dedicated: MongoDatabaseName = MongoDatabaseName("kinowo_fleet")

  /** The fleet database `configured` (`MONGODB_FLEET_DB`) names, or [[Legacy]] when it names none. */
  def name(configured: Option[FleetDatabaseName]): MongoDatabaseName =
    configured.map(_.value.trim).filter(_.nonEmpty).fold(Legacy) { value =>
      require(value == Legacy.value || !models.Country.all.exists(_.mongoDb == value),
        s"MONGODB_FLEET_DB=$value is a country's corpus database; the fleet's state does not belong beside one " +
          s"(use ${Dedicated.value})")
      MongoDatabaseName(value)
    }
}
