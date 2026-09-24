package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.ObjectGraph

/** [[TestWebWiring]] promises a wiring "over a DISABLED Mongo … without a cluster" — and CI
 *  runs the web unit suites with `MONGODB_URI` set. Every Mongo member the production wiring
 *  opens from the environment (the shared pool, the users database, the /debug mirror, the
 *  /debug stacks of the other countries) has to stay shut here whatever the shell holds, or
 *  a unit spec quietly reads the real cluster — and the /debug stacks put every other
 *  country into one country's wiring (`CountryIsolationMatrixSpec` red only in CI). */
class TestWebWiringSpec extends AnyFlatSpec with Matchers {

  "The test web wiring" should "open no Mongo client even when MONGODB_URI is set" in {
    val previous = Option(System.getProperty("MONGODB_URI"))
    // Nothing listens on port 1; a client only dials on its first operation.
    System.setProperty("MONGODB_URI", "mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200")
    try {
      val wiring = new TestWebWiring()
      wiring.boot()
      Seq(wiring.movieController, wiring.debugController, wiring.usersConnection, wiring.movieMirrorConnection)
      val clients = ObjectGraph.collect(wiring) {
        case c: org.mongodb.scala.MongoClient                     => c.toString
        case c: com.mongodb.reactivestreams.client.MongoClient    => c.toString
      }
      withClue(clients.map(_._1).mkString("\n"))(clients shouldBe empty)
      wiring.debugCountries.switchable shouldBe false
    } finally previous.fold(System.clearProperty("MONGODB_URI"))(System.setProperty("MONGODB_URI", _))
  }
}
