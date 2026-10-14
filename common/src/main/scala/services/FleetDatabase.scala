package services

/** The database every country's deployment shares: the fleet's Facebook re-scrape queue and
 *  quota, and the film pages Facebook has fetched.
 *
 *  POLAND'S DATABASE, NOT A DEDICATED ONE, for the reason the web's shared users database is
 *  (`MONGODB_USERS_DB: kinowo`, see the web manifest): the deployments' Mongo user is scoped to
 *  the country databases, so a new database fails every write `Unauthorized` until the server
 *  grants it. `kinowo` is one every deployment already holds rights on; the fleet's collections
 *  sit beside the Polish corpus and nothing that prunes the corpus reads them. */
object FleetDatabase {
  val Name: settings.MongoDatabaseName = settings.MongoDatabaseName(models.Country.Poland.mongoDb)
}
