package services.users

import models.UserState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

class CachingUserStateRepoSpec extends AnyFlatSpec with Matchers {

  private val AliceState = UserState(
    userId          = "uuid-alice",
    hiddenFilms     = Set("Bad Movie"),
    disabledCinemas = Set("Multikino Stary Browar"),
    updatedAt       = Instant.parse("2026-05-19T12:00:00Z"),
    selectedMovies  = Set("Diabeł ubiera się u Prady 2"),
    favouriteRooms  = Set("Cinema City Poznań Plaza|Sala IMAX")
  )

  private class CountingUserStateRepo(seed: Seq[UserState] = Seq.empty) extends UserStateRepo {
    val findHits   = new AtomicInteger(0)
    val upsertHits = new AtomicInteger(0)
    val deleteHits = new AtomicInteger(0)

    private val inner = new InMemoryUserStateRepo
    seed.foreach(inner.upsert)

    def enabled: Boolean = inner.enabled
    def find(userId: String): Option[UserState] = { findHits.incrementAndGet(); inner.find(userId) }
    def upsert(s: UserState): Unit = { upsertHits.incrementAndGet(); inner.upsert(s) }
    def delete(userId: String): Unit = { deleteHits.incrementAndGet(); inner.delete(userId) }
    def close(): Unit = inner.close()
  }

  "CachingUserStateRepo.find" should "hit the inner repo once and serve subsequent calls from cache" in {
    val inner  = new CountingUserStateRepo(Seq(AliceState))
    val cached = new CachingUserStateRepo(inner)

    cached.find("uuid-alice") shouldBe Some(AliceState)
    cached.find("uuid-alice") shouldBe Some(AliceState)
    inner.findHits.get() shouldBe 1
  }

  it should "not cache a miss — every find on an unknown user round-trips" in {
    val inner  = new CountingUserStateRepo()
    val cached = new CachingUserStateRepo(inner)

    cached.find("ghost") shouldBe None
    cached.find("ghost") shouldBe None
    inner.findHits.get() shouldBe 2
  }

  it should "warm the cache on upsert so the next find is a hit" in {
    val inner  = new CountingUserStateRepo()
    val cached = new CachingUserStateRepo(inner)

    cached.upsert(AliceState)
    cached.find("uuid-alice") shouldBe Some(AliceState)

    inner.upsertHits.get() shouldBe 1
    inner.findHits.get()   shouldBe 0
  }

  it should "refresh the cached value on a follow-up upsert" in {
    val inner  = new CountingUserStateRepo(Seq(AliceState))
    val cached = new CachingUserStateRepo(inner)

    cached.find("uuid-alice")  // populate cache
    val updated = AliceState.copy(hiddenFilms = Set("Bad Movie", "Other Bad Movie"))
    cached.upsert(updated)

    cached.find("uuid-alice") shouldBe Some(updated)
    inner.findHits.get() shouldBe 1
  }

  it should "invalidate the entry on delete" in {
    val inner  = new CountingUserStateRepo(Seq(AliceState))
    val cached = new CachingUserStateRepo(inner)

    cached.find("uuid-alice")  // populate
    cached.delete("uuid-alice")
    cached.find("uuid-alice") shouldBe None
    inner.findHits.get() shouldBe 2
  }
}
