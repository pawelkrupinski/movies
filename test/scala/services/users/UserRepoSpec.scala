package services.users

import models.User
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class UserRepoSpec extends AnyFlatSpec with Matchers {

  // Spec exercises `UserRepo` via the in-memory impl so the trait's
  // contract is what's pinned — `MongoUserRepo` honours the same shape
  // and gets its own integration spec against a real Mongo in `it/`.

  private val Alice = User(
    id          = "uuid-alice",
    provider    = "google",
    providerSub = "g-12345",
    email       = Some("alice@example.com"),
    displayName = Some("Alice"),
    avatarUrl   = Some("https://lh3.googleusercontent.com/a/avatar"),
    createdAt   = Instant.parse("2026-01-01T00:00:00Z"),
    lastSeenAt  = Instant.parse("2026-05-19T12:00:00Z")
  )

  "UserRepo" should "round-trip a freshly upserted user via findById" in {
    val repo = new InMemoryUserRepo
    repo.upsert(Alice)
    repo.findById("uuid-alice") shouldBe Some(Alice)
  }

  it should "look up by (provider, providerSub) for the OAuth callback's 'returning user?' check" in {
    val repo = new InMemoryUserRepo
    repo.upsert(Alice)
    repo.findByProviderSub("google", "g-12345") shouldBe Some(Alice)
  }

  it should "return None for an unknown id" in {
    val repo = new InMemoryUserRepo
    repo.findById("never-seen") shouldBe empty
  }

  it should "return None for a (provider, sub) combo that doesn't exist" in {
    val repo = new InMemoryUserRepo
    repo.upsert(Alice)
    // Right sub but wrong provider — must NOT match
    repo.findByProviderSub("facebook", "g-12345") shouldBe empty
  }

  it should "treat upsert(same id, changed fields) as an update — newest write wins" in {
    val repo = new InMemoryUserRepo
    repo.upsert(Alice)
    val laterSeen = Alice.copy(
      lastSeenAt  = Instant.parse("2026-06-01T00:00:00Z"),
      displayName = Some("Alice (renamed)")
    )
    repo.upsert(laterSeen)
    repo.findById("uuid-alice").map(_.displayName) shouldBe Some(Some("Alice (renamed)"))
    // Sub index must still resolve, not double up.
    repo.findByProviderSub("google", "g-12345") shouldBe Some(laterSeen)
  }

  it should "be enabled — the in-memory impl is always ready, unlike the Mongo one" in {
    new InMemoryUserRepo().enabled shouldBe true
  }
}
