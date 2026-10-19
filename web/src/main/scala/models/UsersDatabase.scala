package models

/** Where a deployment keeps the people who use it, as against its films. */
object UsersDatabase {
  /** The database holding the SHARED `users` + `userStates` collections.
   *
   *  Everything else about a deployment is per country -- its films, its cities,
   *  its own `kinowo_uk` / `kinowo_de` database -- but a PERSON is not. The
   *  web pods are several addresses onto one product, and an account that exists on
   *  `/uk` and not on `/de` is the same person being told they have no account;
   *  worse, a session cookie that DOES reach the neighbouring country (they share
   *  one origin, see [[Country.mountPath]]) would resolve its `userId` against a database
   *  that has never heard of it, and the visitor would be silently signed out
   *  with their hidden films and /plan picks apparently gone. So the two user
   *  collections live in ONE database every country's pod reads: `MONGODB_USERS_DB`.
   *
   *  Unset, this is the deployment's OWN database -- exactly where those two
   *  collections have always been -- so the split is opt-in per environment and a
   *  local dev, a spec, or a country deployed on its own cluster keeps the
   *  single-database shape with nothing to configure.
   *
   *  Identity makes the move safe: [[models.User]]`.id` is the lowercased email,
   *  so the same person already carries the same key in every database and
   *  merging them is a union rather than a re-key. */
  def name(usersDb: Option[settings.UsersDatabaseName], ownDb: settings.MongoDatabaseName): settings.MongoDatabaseName =
    settings.MongoDatabaseName(nameFrom(usersDb.map(_.value), ownDb.value))

  /** Pure core of [[name]] -- the precedence, testable without touching
   *  the environment. A blank or whitespace-only variable counts as UNSET rather
   *  than as a database called `""`: an empty value in a ConfigMap is how the
   *  setting gets switched off, and Mongo would reject the empty name much later,
   *  at the first query rather than at boot.
   *
   *  Public so a spec can resolve a GIVEN deployment's users
   *  database without mutating process state — and so the migration that fills
   *  that database picks its target through this rule rather than restating it. */
  def nameFrom(usersDb: Option[String], ownDb: String): String =
    usersDb.map(_.trim).filter(_.nonEmpty).getOrElse(ownDb)
}
