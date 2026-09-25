package tools

import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * A throwaway database with a name nobody else can be using, for specs that need
 * a REAL Mongo but must not see — or be seen by — anything running beside them.
 *
 * The `it/` layer's existing convention is one shared database plus per-document
 * sentinel ids and an `afterAll` purge, which works only because those specs
 * touch a handful of known rows. A spec that writes a whole country's corpus and
 * then reads the collection back WHOLE cannot use it: a co-running spec's rows
 * would be indistinguishable from its own, and `IntegrationTest / parallelExecution`
 * is `true`. Isolating by DATABASE rather than by document keeps the legs
 * genuinely independent — including three country legs of the same spec.
 *
 * The name carries the purpose, the discriminator, and enough entropy to survive
 * two runs starting in the same millisecond, so a leaked database is traceable to
 * whatever left it behind.
 */
final class IsolatedMongoDatabase private (client: MongoClient, val database: MongoDatabase) extends AutoCloseable {

  private var dropped = false

  /**
   * Drop this database and close its client. Safe to call twice.
   *
   * The handle owns exactly one database, so a suite tidying up can only ever drop its
   * own: an earlier process-wide registry offered a drop-everything call, and a suite that
   * finished first took another suite's database out from under it — the victim failed
   * only when run alongside the other, the least diagnosable way to fail.
   */
  def drop(): Unit = synchronized {
    if (!dropped) {
      dropped = true
      try Await.result(database.drop().toFuture(), 60.seconds)
      catch { case _: Throwable => () }   // a suite that failed early must still close its client
      finally client.close()
    }
  }

  override def close(): Unit = drop()
}

object IsolatedMongoDatabase {

  /** Prefix every isolated database shares, so a sweep can find strays. */
  val Prefix: String = "kinowo_isolated"

  /** A uniquely-named database that outlives a single block — for a suite whose
   *  tests SHARE one expensive fixture and so cannot each wrap their own scope.
   *  The suite owns the handle and must [[IsolatedMongoDatabase.drop]] it when it
   *  ends; until then the database is left in place deliberately.
   *
   *  Prefer [[withDatabase]] whenever the work fits inside one block: it cannot
   *  leak, because the drop is in a `finally`. */
  def open(uri: String, purpose: String): IsolatedMongoDatabase = {
    IntegrationMongo.requireThrowaway(uri, Env.get(IntegrationMongo.OverrideVar).exists(v => v == "1" || v.equalsIgnoreCase("true")))
    val client = MongoClient(uri)
    new IsolatedMongoDatabase(client, client.getDatabase(nameFor(purpose)))
  }

  /** Open a uniquely-named database on `uri`, run `body` against it, and drop it
   *  afterwards — dropped even when `body` throws, since the alternative is an
   *  orphan database per failed run. */
  def withDatabase[A](uri: String, purpose: String)(body: MongoDatabase => A): A = {
    IntegrationMongo.requireThrowaway(uri, Env.get(IntegrationMongo.OverrideVar).exists(v => v == "1" || v.equalsIgnoreCase("true")))
    val client = MongoClient(uri)
    val name   = nameFor(purpose)
    try {
      val database = client.getDatabase(name)
      try body(database)
      finally Await.result(database.drop().toFuture(), 60.seconds)
    } finally client.close()
  }

  /**
   * `kinowo_isolated_<purpose>_<pid>_<nanos>` — lower-cased and stripped of anything
   * Mongo won't accept in a database name.
   *
   * PRIVATE, because it is not idempotent: the `<nanos>` means every call returns a
   * different name. A caller that generated one here to label a database it had already
   * opened got a SECOND, unrelated database — repositories wrote to one while a
   * connection resolved collections in the other, and the corpus vanished between them
   * with nothing in error. Take the name from the opened `MongoDatabase` instead; it
   * carries its own.
   */
  /** Mongo rejects a database name over 63 characters, and the pid+nanos suffix is
   *  ~30 of them — so a caller's `purpose` is TRUNCATED to what is left rather than
   *  allowed to overflow. A too-long purpose used to surface as `InvalidNamespace`
   *  from deep inside a lazy wiring init, which reads as the storage being broken
   *  rather than the name being long. */
  private val MaxDatabaseNameLength = 63

  private def nameFor(purpose: String): String = {
    val safe = purpose.toLowerCase(java.util.Locale.ROOT).replaceAll("[^a-z0-9]+", "_").stripPrefix("_").stripSuffix("_")
    val suffix = s"_${ProcessHandle.current().pid()}_${System.nanoTime()}"
    val room   = MaxDatabaseNameLength - Prefix.length - 1 - suffix.length
    s"${Prefix}_${safe.take(math.max(1, room))}$suffix"
  }
}
