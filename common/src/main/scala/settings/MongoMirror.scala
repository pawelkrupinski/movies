package settings

import org.mongodb.scala.MongoClientSettings

import scala.concurrent.duration._
import scala.util.Try

/** The local read-mirror's connection settings (`MONGODB_MOVIES_MIRROR_URI`): a LAN Mongo kept synced from prod, so a
 *  full-corpus read is a LAN hop instead of the prod tunnel. Never relied on: a mirror that does not answer within
 *  [[Timeout]] is down, and its connection disables rather than wedging a read or a boot. */
object MongoMirror {

  /** Boot-probe + server-selection cap for the loopback `/debug` read-mirror.
   *  The mirror is a LAN Mongo that answers in ~ms when healthy, so — unlike the
   *  prod cluster, which keeps the tolerant 30s `MongoTuning.DefaultProbeTimeout` — a mirror
   *  that doesn't respond within a few seconds is simply down. The short cap
   *  turns a down/unreachable mirror into a fast fall-back-to-prod at boot AND a
   *  fast-fail (empty, logged) per request, instead of wedging every `/debug`
   *  load on the driver's 30s default server-selection timeout. */
  val Timeout: FiniteDuration = 3.seconds

  /** The local mirror database holding `prodDb`'s synced copy.
   *
   *  The mirror keeps prod's per-country databases side by side on ONE local
   *  instance, each suffixed rather than reusing prod's name. The suffix is load
   *  bearing: a locally-run worker pointed at the same instance defaults to the
   *  `kinowo` database, and without it that worker would write straight into the
   *  mirrored corpus — the collision the README's `kinowo_local` split exists to
   *  prevent. `scripts/local-mirror/mirror-targets.js` derives the SAME name for
   *  the sync side; change one and you must change the other. */
  def databaseFor(prodDb: String): String = s"${prodDb}_prod_mirror"

  /** The driver settings of the one client every per-country view of the mirror shares, failing fast on server
   *  selection. */
  def clientSettings(uri: MirrorMongoUri, tuning: MongoTuning = MongoTuning.Default): MongoClientSettings =
    MongoConnectionSettings.driverSettings(uri.value, Some(MongoServerSelectionTimeout(Timeout)), tuning)

  /** A `/debug` mirror view of ONE country's database, bound to an already-open
   *  mirror client. Opened `Optional`: the mirror is a dev-only read optimisation,
   *  so an unreachable one disables this connection (→ an empty /debug for that
   *  country) rather than failing boot. Short timeouts for the same reason
   *  [[Timeout]] exists — a loopback Mongo that goes quiet is down, not slow. */
  def forDatabase(uri: MirrorMongoUri, prodDb: String): MongoConnectionSettings =
    MongoConnectionSettings(Some(uri.asMongoUri), MongoDatabaseName(databaseFor(prodDb))).failingFastAfter(Timeout)

  /** A connection of its own to the mirror at `uri` — a second connection alongside the primary one, for the web's
   *  read model. The database is taken from the URI's own path (see `databaseFromUri`, which falls back to
   *  `fallbackDatabase` — the caller's own) so the mirror can live in a different database than the app's working db.
   *  Whether it is required is the opener's to decide (the mirror is a soft optimisation, so the web opens it
   *  `Optional` and degrades to the primary connection when it's absent). */
  def fromUri(uri: MirrorMongoUri, fallbackDatabase: MongoDatabaseName, tuning: MongoTuning): MongoConnectionSettings =
    MongoConnectionSettings(Some(uri.asMongoUri), MongoDatabaseName(databaseFromUri(uri.value, fallbackDatabase.value)),
      tuning).failingFastAfter(Timeout)

  /** Database name for an explicit-URI connection: the URI's own path (e.g.
   *  `…/kinowo_prod_mirror`), so the `/debug` mirror lives in a different
   *  database than the app's working db (the prod connection's). Falls back to
   *  `fallback` (the caller's own database) when the URI names no database. The parse is
   *  guarded so a malformed URI flows through to
   *  `MongoConnection`'s own required/optional handling instead of throwing here. */
  def databaseFromUri(uri: String, fallback: => String): String =
    Try(Option(new com.mongodb.ConnectionString(uri).getDatabase)).toOption.flatten
      .filter(_.nonEmpty)
      .getOrElse(fallback)
}
