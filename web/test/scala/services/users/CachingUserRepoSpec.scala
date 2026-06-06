package services.users

import models.User
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

class CachingUserRepoSpec extends AnyFlatSpec with Matchers {

  private val Alice = User(
    id          = "uuid-alice",
    provider    = "google",
    providerSub = "g-12345",
    email       = Some("alice@example.com"),
    displayName = Some("Alice"),
    avatarUrl   = None,
    createdAt   = Instant.parse("2026-01-01T00:00:00Z"),
    lastSeenAt  = Instant.parse("2026-05-19T12:00:00Z")
  )

  /** Counts how often each method on the inner repo gets hit so the
   *  spec can assert the cache short-circuits before the inner call. */
  private class CountingUserRepo(seed: Seq[User] = Seq.empty) extends UserRepo {
    val findByIdHits           = new AtomicInteger(0)
    val findByProviderSubHits  = new AtomicInteger(0)
    val findByEmailHits        = new AtomicInteger(0)
    val upsertHits             = new AtomicInteger(0)
    val deleteHits             = new AtomicInteger(0)

    private val inner = new InMemoryUserRepo
    seed.foreach(inner.upsert)

    def enabled: Boolean = inner.enabled
    def findById(id: String): Option[User] = { findByIdHits.incrementAndGet(); inner.findById(id) }
    def findByProviderSub(p: String, s: String): Option[User] = { findByProviderSubHits.incrementAndGet(); inner.findByProviderSub(p, s) }
    def findByEmail(e: String): Option[User] = { findByEmailHits.incrementAndGet(); inner.findByEmail(e) }
    def upsert(u: User): Unit = { upsertHits.incrementAndGet(); inner.upsert(u) }
    def delete(id: String): Unit = { deleteHits.incrementAndGet(); inner.delete(id) }
    def close(): Unit = inner.close()
  }

  "CachingUserRepo.findById" should "hit the inner repo once and then serve subsequent calls from cache" in {
    val inner  = new CountingUserRepo(Seq(Alice))
    val cached = new CachingUserRepo(inner)

    cached.findById("uuid-alice") shouldBe Some(Alice)
    cached.findById("uuid-alice") shouldBe Some(Alice)
    cached.findById("uuid-alice") shouldBe Some(Alice)

    inner.findByIdHits.get() shouldBe 1
  }

  it should "not cache a miss — a later findById must still consult the inner repo" in {
    val inner  = new CountingUserRepo()
    val cached = new CachingUserRepo(inner)

    cached.findById("ghost") shouldBe None
    cached.findById("ghost") shouldBe None

    // No phantom-negative cache: each miss must round-trip so a row that
    // shows up between calls (rare but possible — concurrent OAuth
    // callback) is visible without a 1-hour wait.
    inner.findByIdHits.get() shouldBe 2
  }

  it should "warm the cache on upsert so the next findById is a hit" in {
    val inner  = new CountingUserRepo()
    val cached = new CachingUserRepo(inner)

    cached.upsert(Alice)
    cached.findById("uuid-alice") shouldBe Some(Alice)

    inner.upsertHits.get()   shouldBe 1
    inner.findByIdHits.get() shouldBe 0  // served entirely from cache
  }

  it should "refresh the cached value on upsert so a renamed user surfaces immediately" in {
    val inner  = new CountingUserRepo(Seq(Alice))
    val cached = new CachingUserRepo(inner)

    cached.findById("uuid-alice")  // populate cache
    val renamed = Alice.copy(displayName = Some("Alice Renamed"))
    cached.upsert(renamed)

    cached.findById("uuid-alice") shouldBe Some(renamed)
    inner.findByIdHits.get() shouldBe 1  // second findById is cache-served
  }

  it should "invalidate the entry on delete so a re-create lookup hits Mongo" in {
    val inner  = new CountingUserRepo(Seq(Alice))
    val cached = new CachingUserRepo(inner)

    cached.findById("uuid-alice")  // populate
    cached.delete("uuid-alice")
    cached.findById("uuid-alice") shouldBe None
    // The post-delete lookup must re-hit Mongo (just in case a concurrent
    // OAuth callback re-created the row).
    inner.findByIdHits.get() shouldBe 2
  }

  "CachingUserRepo" should "delegate findByProviderSub and findByEmail straight through (cold paths, no cache)" in {
    val inner  = new CountingUserRepo(Seq(Alice))
    val cached = new CachingUserRepo(inner)

    cached.findByProviderSub("google", "g-12345") shouldBe Some(Alice)
    cached.findByProviderSub("google", "g-12345") shouldBe Some(Alice)
    cached.findByEmail("alice@example.com")       shouldBe Some(Alice)
    cached.findByEmail("alice@example.com")       shouldBe Some(Alice)

    inner.findByProviderSubHits.get() shouldBe 2
    inner.findByEmailHits.get()       shouldBe 2
  }
}
