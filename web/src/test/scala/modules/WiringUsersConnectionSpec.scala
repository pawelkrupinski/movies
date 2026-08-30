package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoConnection

/**
 * Where the `users` + `userStates` collections are read and written.
 *
 * Four country deployments serve four corpora out of four databases, and that
 * split is right for everything the worker projects — but not for a PERSON.
 * The three Showtimes countries share one origin, so a session cookie minted
 * under `/uk` now arrives at `/de`; against a per-country `users` collection its
 * `userId` resolves to nobody and the visitor is silently signed out, hidden
 * films and /plan picks apparently gone. `MONGODB_USERS_DB` is what stops that,
 * and this is the decision that honours it.
 *
 * Asserted here rather than through a booted application because the failure is
 * invisible from the outside: bound to the wrong database every page still
 * renders, every spec still passes, and the only symptom is an account that
 * does not exist one country over.
 */
class WiringUsersConnectionSpec extends AnyFlatSpec with Matchers {

  // A connection with no URI never dials Mongo — `MongoConnection.init` logs and
  // disables itself — so these stand in for real database views with no cluster
  // anywhere near the spec.
  private def disabled(dbName: String) =
    new MongoConnection(uri = None, dbName = dbName, required = false)

  private class RecordingOpen {
    var opened: List[String] = Nil
    val open: String => MongoConnection = name => { opened = opened :+ name; disabled(name) }
  }

  "Wiring.usersConnection" should "reuse the deployment's own connection when no users database is configured" in {
    val own    = disabled("kinowo_uk")
    val opener = new RecordingOpen
    val got    = Wiring.usersConnection("kinowo_uk", "kinowo_uk", own, opener.open)

    (got eq own) shouldBe true
    // The point of reusing it: no second boot probe, and no second close() to
    // get right, against a database we are already connected to.
    opener.opened shouldBe empty
  }

  it should "open a second view, on the users database, when one is configured" in {
    val own    = disabled("kinowo_uk")
    val opener = new RecordingOpen
    val got    = Wiring.usersConnection("kinowo_uk", "kinowo_users", own, opener.open)

    (got eq own) shouldBe false
    opener.opened shouldBe List("kinowo_users")
  }

  // The whole point, stated as one assertion: whatever corpus a pod serves, its
  // accounts come from the same place as every other pod's.
  it should "send every country to ONE users database, whatever its own corpus is" in {
    val landedOn = models.Country.all.map { country =>
      val opener = new RecordingOpen
      Wiring.usersConnection(country.mongoDb, "kinowo_users", disabled(country.mongoDb), opener.open)
      opener.opened
    }
    landedOn.distinct shouldBe List(List("kinowo_users"))
  }

  // Poland's own database is `kinowo`, so an environment that points
  // MONGODB_USERS_DB at it is asking the other three to read Poland's users
  // collection — a real deployment shape (adopt the existing rows rather than
  // migrate them), and Poland itself must NOT then open a duplicate view.
  it should "not duplicate the connection when the users database is this country's own" in {
    val own    = disabled("kinowo")
    val opener = new RecordingOpen
    (Wiring.usersConnection("kinowo", "kinowo", own, opener.open) eq own) shouldBe true
    opener.opened shouldBe empty
  }
}
