package services.users

import models.User
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class UserRepositorySpec extends AnyFlatSpec with Matchers {

  // Spec exercises `UserRepository` via the in-memory impl so the trait's
  // contract is what's pinned — `MongoUserRepository` honours the same shape
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

  "UserRepository" should "round-trip a freshly upserted user via findById" in {
    val repository = new InMemoryUserRepository
    repository.upsert(Alice)
    repository.findById("uuid-alice") shouldBe Some(Alice)
  }

  it should "look up by (provider, providerSub) for the OAuth callback's 'returning user?' check" in {
    val repository = new InMemoryUserRepository
    repository.upsert(Alice)
    repository.findByProviderSub("google", "g-12345") shouldBe Some(Alice)
  }

  it should "return None for an unknown id" in {
    val repository = new InMemoryUserRepository
    repository.findById("never-seen") shouldBe empty
  }

  it should "return None for a (provider, sub) combo that doesn't exist" in {
    val repository = new InMemoryUserRepository
    repository.upsert(Alice)
    // Right sub but wrong provider — must NOT match
    repository.findByProviderSub("facebook", "g-12345") shouldBe empty
  }

  it should "treat upsert(same id, changed fields) as an update — newest write wins" in {
    val repository = new InMemoryUserRepository
    repository.upsert(Alice)
    val laterSeen = Alice.copy(
      lastSeenAt  = Instant.parse("2026-06-01T00:00:00Z"),
      displayName = Some("Alice (renamed)")
    )
    repository.upsert(laterSeen)
    repository.findById("uuid-alice").map(_.displayName) shouldBe Some(Some("Alice (renamed)"))
    // Sub index must still resolve, not double up.
    repository.findByProviderSub("google", "g-12345") shouldBe Some(laterSeen)
  }

  it should "be enabled — the in-memory impl is always ready, unlike the Mongo one" in {
    new InMemoryUserRepository().enabled shouldBe true
  }

  "UserRepository.findByEmail" should "find the user by case-insensitive email" in {
    val repository = new InMemoryUserRepository
    repository.upsert(Alice)
    repository.findByEmail("alice@example.com").value      shouldBe Alice
    repository.findByEmail("ALICE@EXAMPLE.COM").value      shouldBe Alice    // case-insensitive
    repository.findByEmail("Alice@Example.com").value      shouldBe Alice
  }

  it should "return None when no user has that email" in {
    val repository = new InMemoryUserRepository
    repository.upsert(Alice)
    repository.findByEmail("bob@example.com") shouldBe empty
  }

  it should "return None for users whose email is None — anonymous users don't match arbitrary lookups" in {
    val repository = new InMemoryUserRepository
    repository.upsert(Alice.copy(id = "no-email", email = None))
    repository.findByEmail("any@x") shouldBe empty
    repository.findByEmail("")      shouldBe empty
  }

  "UserRepository.delete" should "remove the user row + its provider/sub index entry" in {
    val repository = new InMemoryUserRepository
    repository.upsert(Alice)
    repository.findById("uuid-alice") should be (defined)

    repository.delete("uuid-alice")
    repository.findById("uuid-alice")                          shouldBe empty
    repository.findByProviderSub("google", "g-12345")          shouldBe empty
    repository.findByEmail("alice@example.com")                shouldBe empty
  }

  it should "no-op on a delete of a non-existent id" in {
    val repository = new InMemoryUserRepository
    noException should be thrownBy repository.delete("never-existed")
  }

}
