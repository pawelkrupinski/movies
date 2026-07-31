package integration

import models.{Multikino, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.staging.MongoStagingRepository
import tools.{ConvergenceStorage, Env}

/**
 * That every part of a Mongo-backed convergence run looks at the SAME database.
 *
 * The repositories are handed a `MongoDatabase` directly, while `MongoStagingFolder` is
 * built from a `MongoConnection` and resolves its collections by database NAME. Those are
 * two routes to what must be one place, and nothing checked that they agreed.
 *
 * They didn't. `IsolatedMongoDatabase.nameFor` embeds `System.nanoTime()`, so generating
 * the name a second time for the connection produced a different database: staging wrote
 * 6,975 rows to one, the folder looked for them in the other, found none, and reported
 * nothing to fold. Every component behaved correctly and the corpus still never reached
 * `movies` — the suite said `resolved NOTHING — 0 films` with no error anywhere, and the
 * cause took three wrong diagnoses to find.
 *
 * Asserted through the seam that actually broke — a row written by the storage's own
 * staging repository must be visible through the storage's CONNECTION — rather than by
 * comparing two names, which would pass just as well if a third route appeared.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ConvergenceStorageIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  "a Mongo convergence storage" should "expose one database to its repositories and its connection alike" in {
    val storage = ConvergenceStorage.mongo(Env.get("MONGODB_URI").get, "storage-agreement-spec")
    try {
      storage.staging.upsert(Multikino, "Ghost In The Shell", Some(2017), MovieRecord())

      val throughConnection = new MongoStagingRepository(storage.connection.database).findAll()

      withClue("a row written through the storage's repository must be visible through its " +
               "connection — the folder reaches staging that way: ") {
        throughConnection.map(_.id) should contain (storage.staging.findAll().head.id)
      }
    } finally storage.close()
  }
}
