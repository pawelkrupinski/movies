package settings

// The two database-name values of `ConfigurationValues`'s kind that `models.Country` names (`usersDbName`), kept in
// a file of their own: the identity model's rules version digests every source the resolver reaches, and it reaches
// `Country`. Declared beside every other setting's type, they put all of those in the digest, and each new setting
// anywhere re-resolved every worker's whole corpus (`IdentityRulesSources`).

/** `MONGODB_DB` — an explicit corpus database, overriding the country's own. Also the
 *  name every connection is opened with once resolved (see `MongoAddress.databaseFor`). */
final case class MongoDatabaseName(value: String) extends AnyVal
/** `MONGODB_USERS_DB` — the database holding the SHARED `users` + `userStates`. */
final case class UsersDatabaseName(value: String) extends AnyVal {
  def database: MongoDatabaseName = MongoDatabaseName(value)
}
