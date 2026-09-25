package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.ObjectGraph

/** [[TestWebWiring]] promises a wiring "over a DISABLED Mongo … without a cluster" — and CI
 *  runs the web unit suites with `MONGODB_URI` set. Every Mongo member the production wiring
 *  opens (the shared pool, the users database, the /debug mirror, the /debug stacks of the
 *  other countries) has to stay shut here whatever the environment holds, or a unit spec
 *  quietly reads the real cluster — and the /debug stacks put every other country into one
 *  country's wiring (`CountryIsolationMatrixSpec` red only in CI).
 *
 *  The wiring is handed its [[services.MongoAddress]]; the environment below names a
 *  cluster anyway (nothing listens on port 1, and a client only dials on its first
 *  operation), so a member that still read `MONGODB_URI` for itself would open a client. */
class TestWebWiringSpec extends AnyFlatSpec with Matchers {

  private val DeadCluster = "mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200"

  "The test web wiring" should "open no Mongo client even when its environment names a cluster" in {
    val wiring = new TestWebWiring() {
      override lazy val env: tools.Env = tools.Env.of(
        "MONGODB_URI" -> DeadCluster, "MONGODB_MOVIES_MIRROR_URI" -> DeadCluster, "MONGODB_USERS_DB" -> "kinowo_users_probe")
    }
    wiring.boot()
    Seq(wiring.movieController, wiring.debugController, wiring.usersConnection, wiring.movieMirrorConnection)
    val clients = ObjectGraph.collect(wiring) {
      case c: org.mongodb.scala.MongoClient                     => c.toString
      case c: com.mongodb.reactivestreams.client.MongoClient    => c.toString
    }
    withClue(clients.map(_._1).mkString("\n"))(clients shouldBe empty)
    wiring.debugCountries.switchable shouldBe false
  }
}
