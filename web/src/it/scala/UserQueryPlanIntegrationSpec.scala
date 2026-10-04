package integration

import models.User
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.users.{HiddenFilmsChange, MongoUserRepository, MongoUserStateRepository}
import tools.QueryPlans

import java.time.Instant

/** The users stores' every per-request read and write, planned by a real Mongo: each served by an index
 *  (see `QueryPlanIntegrationSpec` for why a result-checking spec cannot tell). */
class UserQueryPlanIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val Now = Instant.parse("2026-05-19T12:00:00Z")

  "the users stores" should "find, write and delete an account and its state by index" in {
    val plans = QueryPlans.of(mongoTarget, "users") { db =>
      val users  = new MongoUserRepository(Some(db))
      val states = new MongoUserStateRepository(Some(db), _root_.tools.SpecClock.Pinned)
      (1 to 10).foreach { i =>
        users.upsert(User(s"user-$i@example.com", "google", s"g-$i", Some(s"user-$i@example.com"), None, None, Now, Now))
        states.changeHiddenFilms(s"user-$i@example.com", "pl", HiddenFilmsChange.Hide(s"film-$i", bucketLimit = 100), Now)
      }
      users.findById("user-1@example.com")
      users.findByProviderSub("facebook", "g-2")
      users.revokeSessions("user-3@example.com")
      users.delete("user-4@example.com")
      states.find("user-1@example.com")
      states.delete("user-4@example.com")
      users.close(); states.close()
    }
    withClue(s"every planned statement:\n${plans.map(_.toString).distinct.mkString("\n")}\n") {
      QueryPlans.violations(plans, Map(
        "users find filter{$and:[{provider},{providerSub}]}" ->
          "Facebook's data-deletion callback, a handful of calls a year over one row per account: an index would be paid on every sign-in"
      )) shouldBe empty
    }
  }
}
