package tools

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

import org.mongodb.scala.{MongoClient, MongoClientSettings, SingleObservableFuture}
import play.api.Logging

/** The `MongoClient`s a process SHARES across several database views on one cluster -- one pool, one Netty event loop,
 *  one replica-set monitor thread set for N countries, instead of a full client each (the RSS blow-up
 *  `services.MongoConnection` exists to avoid). The caller OWNS `close()`-ing each, after every connection that
 *  borrowed it is closed. */
object MongoClients extends Logging {

  /** A client dialling with `settings`, authenticated once before anything else uses it. */
  def shared(settings: MongoClientSettings): MongoClient = {
    val client = MongoClient(settings)
    authenticateOnce(client)
    client
  }

  /** One round trip on a fresh client before anything else uses it. Its connection's SCRAM handshake
   *  derives the password's key (PBKDF2, 15,000 iterations) into the client's credential cache, which every
   *  later connection of the pool then reuses. A boot's first reads opened the whole pool at once, and each
   *  connection, finding the cache still empty, derived the key for itself: mongod logged 25 authentications
   *  in a us worker's first minute, ~4 s of CPU in `ScramShaSaslClient.hi` (JFR, 2026-10-03). Bounded and
   *  best-effort — a server out of reach leaves the boot as it was, to the connection's own probe. */
  private[tools] def authenticateOnce(client: MongoClient, timeout: FiniteDuration = AuthenticateOnceTimeout): Unit =
    Try(Await.result(client.getDatabase("admin").runCommand(org.mongodb.scala.Document("ping" -> 1)).toFuture(), timeout))
      .failed.foreach(e => logger.warn(s"MongoConnection: the first round trip did not complete in $timeout " +
        s"(${e.getClass.getSimpleName}); the pool will authenticate as it opens"))

  /** How long a boot waits on [[authenticateOnce]] before carrying on without it. */
  private[tools] val AuthenticateOnceTimeout: FiniteDuration = 5.seconds
}
