package controllers

import models.User
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.Session
import play.api.test.FakeRequest
import services.users.InMemoryUserRepository

import java.time.Instant

class SignedInUserSpec extends AnyFlatSpec with Matchers {

  private val Now = Instant.parse("2026-05-19T12:00:00Z")

  private def testUser(id: String, sessionVersion: Int = 0) = User(
    id = id, provider = "google", providerSub = s"sub-$id",
    email = Some(s"$id@example.com"), displayName = Some(id), avatarUrl = None,
    createdAt = Now, lastSeenAt = Now, sessionVersion = sessionVersion
  )

  private def requestWith(session: Session) = FakeRequest().withSession(session.data.toSeq*)

  "SignedInUser.apply" should "resolve a freshly-established session to its user" in {
    val users   = new InMemoryUserRepository
    val user    = testUser("u1")
    users.upsert(user)
    val session = SignedInUser.establish(Session.emptyCookie, user)
    SignedInUser(requestWith(session), users).value shouldBe user
  }

  it should "return None for an anonymous request" in {
    SignedInUser(FakeRequest(), new InMemoryUserRepository) shouldBe empty
  }

  it should "return None when the session names a user with no row at all" in {
    val session = SignedInUser.establish(Session.emptyCookie, testUser("ghost"))
    SignedInUser(requestWith(session), new InMemoryUserRepository) shouldBe empty
  }

  // The revocation check: a cookie's own sessionVersion stamp must still
  // match the row's CURRENT one, not just the row's existence.
  it should "return None when the row's sessionVersion has moved on since the cookie was issued" in {
    val users   = new InMemoryUserRepository
    val issued  = testUser("u1", sessionVersion = 0)
    users.upsert(issued)
    val session = SignedInUser.establish(Session.emptyCookie, issued) // stamps sessionVersion=0

    users.upsert(issued.copy(sessionVersion = 1)) // "sign out everywhere" bumped it
    SignedInUser(requestWith(session), users) shouldBe empty
  }

  it should "resolve a session re-established against the bumped version" in {
    val users     = new InMemoryUserRepository
    val revoked   = testUser("u1", sessionVersion = 1)
    users.upsert(revoked)
    val session   = SignedInUser.establish(Session.emptyCookie, revoked)
    SignedInUser(requestWith(session), users).value shouldBe revoked
  }

  // A session issued before `sessionVersion` existed at all has no stamp in
  // the cookie — it must keep working against a row whose own default is
  // also 0, not be treated as already revoked.
  it should "treat a cookie with no sessionVersion stamp as version 0, matching a fresh row's default" in {
    val users   = new InMemoryUserRepository
    val user    = testUser("u1") // sessionVersion defaults to 0
    users.upsert(user)
    val legacySession = Session(Map(SignedInUser.UserIdKey -> "u1")) // no stamp at all
    SignedInUser(requestWith(legacySession), users).value shouldBe user
  }

  it should "still reject a stampless cookie once the row has been revoked at least once" in {
    val users   = new InMemoryUserRepository
    users.upsert(testUser("u1", sessionVersion = 1))
    val legacySession = Session(Map(SignedInUser.UserIdKey -> "u1"))
    SignedInUser(requestWith(legacySession), users) shouldBe empty
  }

  "SignedInUser.establish" should "carry userId, signedInAt, and sessionVersion" in {
    val user    = testUser("u1", sessionVersion = 3).copy(lastSeenAt = Now)
    val session = SignedInUser.establish(Session.emptyCookie, user)
    session.get(SignedInUser.UserIdKey)         shouldBe Some("u1")
    session.get(SignedInUser.SignedInAtKey)     shouldBe Some(Now.toEpochMilli.toString)
    session.get(SignedInUser.SessionVersionKey) shouldBe Some("3")
  }
}
