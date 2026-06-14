package services

import com.mongodb.WriteConcern
import org.mongodb.scala.MongoClient
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MongoMovieRepository
import services.readmodel.MongoReadModelRepository

/**
 * The `movies` collection and the derived read-model collections are written with
 * a relaxed `w:1, j:false` write concern: both are cheaply rebuildable (`movies`
 * by the next scrape, the read model by the projector's reconcile), so skipping
 * the journal sync cuts per-write cost on the shared-CPU Mongo without risking
 * unrecoverable data. Mirrors the trade `MongoTaskQueue` already makes.
 *
 * A collection's configured write concern is pure config — readable without any
 * Mongo I/O — so we point the repos at a client that never connects (unroutable
 * port, fast server-selection timeout; the lazy index creation just times out and
 * is swallowed) and read it back. Fails before the change (default journaled
 * concern), passes after.
 */
class RelaxedWriteConcernSpec extends AnyFlatSpec with Matchers {

  private val relaxed = WriteConcern.W1.withJournal(false)
  private def neverConnects =
    MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200").getDatabase("test")

  "MongoMovieRepository" should "write the `movies` collection with w:1 j:false" in {
    new MongoMovieRepository(sharedDb = Some(neverConnects)).collectionWriteConcern shouldBe Some(relaxed)
  }

  "MongoReadModelRepository" should "write the derived collections with w:1 j:false" in {
    new MongoReadModelRepository(Some(neverConnects)).collectionWriteConcerns.distinct shouldBe Seq(relaxed)
  }
}
