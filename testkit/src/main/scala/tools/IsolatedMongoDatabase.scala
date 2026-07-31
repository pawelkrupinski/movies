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
object IsolatedMongoDatabase {

  /** Prefix every isolated database shares, so a sweep can find strays. */
  val Prefix: String = "kinowo_isolated"

  // Databases handed out by `open`, so a suite can drop them all at the end.
  // One suite per JVM here, so a process-wide list is the right scope.
  private val opened = scala.collection.mutable.ListBuffer.empty[(MongoClient, MongoDatabase)]

  /** A uniquely-named database that outlives a single block — for a suite whose
   *  tests SHARE one expensive fixture and so cannot each wrap their own scope.
   *  The caller must call [[closeAll]] when the suite ends; until then the
   *  database is left in place deliberately.
   *
   *  Prefer [[withDatabase]] whenever the work fits inside one block: it cannot
   *  leak, because the drop is in a `finally`. */
  def open(uri: String, purpose: String): MongoDatabase = {
    IntegrationMongo.requireThrowaway(uri, Env.get(IntegrationMongo.OverrideVar).exists(v => v == "1" || v.equalsIgnoreCase("true")))
    val client   = MongoClient(uri)
    val database = client.getDatabase(nameFor(purpose))
    opened.synchronized(opened += (client -> database))
    database
  }

  /**
   * Drop ONE database handed out by [[open]] and close its client, leaving any others
   * alone.
   *
   * Prefer this to [[closeAll]] whenever more than one thing in the process might hold an
   * isolated database. `closeAll` drops EVERY one, so a suite that finished tidying up
   * took another suite's database out from under it — two specs in the `it` layer did
   * exactly that to each other, and the victim failed only when run alongside the other,
   * which is the least diagnosable way to fail.
   */
  def drop(database: MongoDatabase): Unit = opened.synchronized {
    opened.indexWhere(_._2.name == database.name) match {
      case -1 => ()
      case at =>
        val (client, db) = opened(at)
        try Await.result(db.drop().toFuture(), 60.seconds)
        catch { case _: Throwable => () }
        finally { client.close(); opened.remove(at) }
    }
  }

  /** Drop every database handed out by [[open]] and close their clients. Safe to
   *  call twice, and safe to call when nothing was opened. Use [[drop]] instead when
   *  anything else in the process may hold one. */
  def closeAll(): Unit = opened.synchronized {
    opened.foreach { case (client, database) =>
      try Await.result(database.drop().toFuture(), 60.seconds)
      catch { case _: Throwable => () }   // a suite that failed early must still close its client
      finally client.close()
    }
    opened.clear()
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
