package modules.webwiring

import modules.Wiring
import play.api.Mode
import services.MongoConnection

/** ── Mongo ─────────────────────────────────────────────────────────────────
 *  The one `MongoClient` this process opens, and the two database views on it:
 *  this country's corpus and the shared users database. */
trait MongoWiring { self: Wiring =>

  // A missing/unreachable Mongo is a hard boot failure everywhere except tests
  // (opt back into silent-degrade with MONGODB_OPTIONAL=true) — see
  // `MongoConnection`.
  protected lazy val mongoRequired: Boolean = {
    val optedOut = env.flag("MONGODB_OPTIONAL")
    MongoConnection.isRequired(environmentMode == Mode.Test, optedOut)
  }

  // ONE MongoClient behind every database view this process opens — this
  // country's corpus, and the shared users database below when that is a
  // different one. Built here, once, so the second view BORROWS this pool: a
  // client per view is a second connection pool, Netty event loop and
  // replica-set monitor thread set, which is the RSS
  // blow-up `MongoConnection` was written to avoid. `None` when the address names
  // no cluster — then there is no pool to share and each connection degrades on its
  // own, exactly as before. Owned by the root: `stop()` closes it after the
  // connections that borrowed it, since their own close() deliberately leaves it
  // alone.
  protected lazy val mongoSharedClient: Option[org.mongodb.scala.MongoClient] =
    MongoConnection.sharedClientAt(mongoAddress, env)

  lazy val mongoConnection: MongoConnection =
    MongoConnection.forDatabase(mongoAddress.uri, mongoAddress.databaseFor(country), mongoRequired, env, sharedClient = mongoSharedClient)

  // ── Users ─────────────────────────────────────────────────────────────────
  // `users` + `userStates` come off `Country.usersDbName` rather than this
  // deployment's own database, so ONE account follows a visitor across every
  // country instead of one unrelated account per country wearing the same email. That
  // matters most where the session cookie now DOES travel: the four Showtimes
  // countries share one origin, so a `userId` minted under /uk arrives at /de,
  // and against a per-country database it would resolve to nobody — a silent
  // sign-out with the visitor's hidden films apparently gone.
  // Unset (`MONGODB_USERS_DB`), this IS this deployment's database and one
  // connection object serves both — no second boot probe of a database we are
  // already talking to.
  lazy val usersConnection: MongoConnection = Wiring.usersConnection(
    ownDbName   = mongoAddress.databaseFor(country),
    usersDbName = models.Country.usersDbName(env, mongoAddress.databaseFor(country)),
    own         = mongoConnection,
    openUsers   = MongoConnection.forDatabase(mongoAddress.uri, _, mongoRequired, env, sharedClient = mongoSharedClient))
}
