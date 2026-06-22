package integration

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.MongoReadModelRepository
import tools.Env

/**
 * Read-only check that the read model's id-only projections
 * (`findAllMovieIds` / `findAllScreeningRefs`, which the reconcile prune uses to
 * stay off the heap) return the SAME ids as a full `findAllMovies` /
 * `findAllScreenings` decode — i.e. the server-side `{_id}` / `{_id, filmId}`
 * BsonDocument projection is faithful. Requires MONGODB_URI; skips otherwise.
 *
 * Purely read-only against the live `web_movies` / `web_screenings`: it writes
 * nothing, so there are no sentinels to purge.
 */
class ReadModelRepositoryIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
  private val rm     = new MongoReadModelRepository(Some(db))

  override protected def afterAll(): Unit = try { rm.close(); client.close() } finally super.afterAll()

  "findAllMovieIds" should "project the same ids as a full findAllMovies decode" in {
    rm.findAllMovieIds().toSet shouldBe rm.findAllMovies().map(_._id).toSet
  }

  "findAllScreeningRefs" should "project the same (_id, filmId) pairs as a full findAllScreenings decode" in {
    rm.findAllScreeningRefs().map(r => r._id -> r.filmId).toSet shouldBe
      rm.findAllScreenings().map(s => s._id -> s.filmId).toSet
  }
}
