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

  /** `kinowo_isolated_<purpose>_<pid>_<nanos>` — lower-cased and stripped of
   *  anything Mongo won't accept in a database name. */
  def nameFor(purpose: String): String = {
    val safe = purpose.toLowerCase(java.util.Locale.ROOT).replaceAll("[^a-z0-9]+", "_").stripPrefix("_").stripSuffix("_")
    s"${Prefix}_${safe}_${ProcessHandle.current().pid()}_${System.nanoTime()}"
  }
}
