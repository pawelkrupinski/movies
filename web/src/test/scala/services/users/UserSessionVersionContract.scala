package services.users

import models.User
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

/** What "sign out everywhere" needs from a `UserRepository`, stated once and
 *  run against every store — in memory in `UserRepositorySpec`, Mongo in
 *  `UserRepositoryIntegrationSpec`.
 *
 *  `sessionVersion` is the revocation counter (see `SignedInUser`), and a
 *  sign-in rewrites the whole row from a copy it read a moment earlier. Unless
 *  the store owns the counter, a sign-in that read before a revoke and wrote
 *  after it puts the old version back — and every revoked cookie works again. */
trait UserSessionVersionContract { this: AnyFlatSpec & Matchers =>

  protected def sessionStore: UserRepository
  protected def sessionUserIdPrefix: String

  private val Now = Instant.parse("2026-05-19T12:00:00Z")

  private def freshUser(suffix: String): User = {
    val id = s"$sessionUserIdPrefix$suffix"
    sessionStore.delete(id)
    val user = User(id = id, provider = "google", providerSub = s"$id-sub", email = Some(s"$id@example.com"),
      displayName = None, avatarUrl = None, createdAt = Now, lastSeenAt = Now)
    sessionStore.upsert(user)
    user
  }

  def sessionVersionBehaviour(storeName: String): Unit = {

    s"$storeName.revokeSessions" should "bump the stored sessionVersion and answer the row after it" in {
      val user = freshUser("bump")
      sessionStore.revokeSessions(user.id).value.sessionVersion shouldBe 1
      sessionStore.revokeSessions(user.id).value.sessionVersion shouldBe 2
      sessionStore.findById(user.id).value.sessionVersion shouldBe 2
    }

    it should "answer None, and create nothing, for a user with no row" in {
      val id = s"${sessionUserIdPrefix}absent"
      sessionStore.delete(id)
      sessionStore.revokeSessions(id) shouldBe empty
      sessionStore.findById(id) shouldBe empty
    }

    s"$storeName.upsert" should "not undo a revoke with a copy of the row read before it" in {
      val user = freshUser("stale-sign-in")
      val readBySignIn = sessionStore.findById(user.id).value
      sessionStore.revokeSessions(user.id)
      sessionStore.upsert(readBySignIn.copy(lastSeenAt = Now.plusSeconds(60)))
      val stored = sessionStore.findById(user.id).value
      stored.sessionVersion shouldBe 1
      stored.lastSeenAt shouldBe Now.plusSeconds(60)
    }

    // What the sign-in then puts in its cookie: the version the row holds AFTER
    // the write, in the same step — not the one its stale copy carried, which the
    // revoke already killed.
    it should "answer the row as stored, carrying the version a revoke left" in {
      val user = freshUser("answers-stored")
      val readBySignIn = sessionStore.findById(user.id).value
      sessionStore.revokeSessions(user.id)
      val written = sessionStore.upsert(readBySignIn.copy(lastSeenAt = Now.plusSeconds(60)))
      written.sessionVersion shouldBe 1
      written.lastSeenAt shouldBe Now.plusSeconds(60)
    }

    it should "still clear an optional field the new copy leaves out" in {
      val user = freshUser("clears")
      sessionStore.upsert(user.copy(avatarUrl = Some("https://avatar")))
      sessionStore.upsert(user.copy(avatarUrl = None))
      sessionStore.findById(user.id).value.avatarUrl shouldBe empty
    }
  }
}
