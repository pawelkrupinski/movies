package services.users

import models.UserState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger

class CachingUserStateRepositorySpec extends AnyFlatSpec with Matchers {

  private val AliceState = UserState(
    userId          = "uuid-alice",
    hiddenFilms     = Set("Bad Movie"),
    disabledCinemas = Set("Multikino Stary Browar"),
    updatedAt       = Instant.parse("2026-05-19T12:00:00Z")
  )

  private class CountingUserStateRepository(seed: Seq[UserState] = Seq.empty) extends UserStateRepository {
    val findHits   = new AtomicInteger(0)
    val upsertHits = new AtomicInteger(0)
    val deleteHits = new AtomicInteger(0)

    private val inner = new InMemoryUserStateRepository
    seed.foreach(inner.upsert)

    def enabled: Boolean = inner.enabled
    def find(userId: String): Option[UserState] = { findHits.incrementAndGet(); inner.find(userId) }
    def upsert(s: UserState): Unit = { upsertHits.incrementAndGet(); inner.upsert(s) }
    def delete(userId: String): Unit = { deleteHits.incrementAndGet(); inner.delete(userId) }
    def close(): Unit = inner.close()
    override def watchChanges(onUpsert: UserState => Unit, onDelete: String => Unit, onDisconnect: () => Unit): Option[AutoCloseable] =
      inner.watchChanges(onUpsert, onDelete, onDisconnect)
  }

  "CachingUserStateRepository.find" should "hit the inner repository once and serve subsequent calls from cache" in {
    val inner  = new CountingUserStateRepository(Seq(AliceState))
    val cached = new CachingUserStateRepository(inner)

    cached.find("uuid-alice") shouldBe Some(AliceState)
    cached.find("uuid-alice") shouldBe Some(AliceState)
    inner.findHits.get() shouldBe 1
  }

  it should "not cache a miss — every find on an unknown user round-trips" in {
    val inner  = new CountingUserStateRepository()
    val cached = new CachingUserStateRepository(inner)

    cached.find("ghost") shouldBe None
    cached.find("ghost") shouldBe None
    inner.findHits.get() shouldBe 2
  }

  it should "warm the cache on upsert so the next find is a hit" in {
    val inner  = new CountingUserStateRepository()
    val cached = new CachingUserStateRepository(inner)

    cached.upsert(AliceState)
    cached.find("uuid-alice") shouldBe Some(AliceState)

    inner.upsertHits.get() shouldBe 1
    inner.findHits.get()   shouldBe 0
  }

  it should "refresh the cached value on a follow-up upsert" in {
    val inner  = new CountingUserStateRepository(Seq(AliceState))
    val cached = new CachingUserStateRepository(inner)

    cached.find("uuid-alice")  // populate cache
    val updated = AliceState.copy(hiddenFilms = Set("Bad Movie", "Other Bad Movie"))
    cached.upsert(updated)

    cached.find("uuid-alice") shouldBe Some(updated)
    inner.findHits.get() shouldBe 1
  }

  it should "invalidate the entry on delete" in {
    val inner  = new CountingUserStateRepository(Seq(AliceState))
    val cached = new CachingUserStateRepository(inner)

    cached.find("uuid-alice")  // populate
    cached.delete("uuid-alice")
    cached.find("uuid-alice") shouldBe None
    inner.findHits.get() shouldBe 2
  }

  // The users database is SHARED across pods — every web replica and every country's host —
  // and the controller's writes are read-modify-write full-document replaces. A pod serving
  // `find` from its own 10-minute cache after ANOTHER pod wrote would hand the controller a
  // stale base, and its upsert would silently overwrite the other pod's change (e.g. a film
  // hidden on showtimes.cc lost by the next hide on kinowo.net). The change stream sees every
  // pod's writes, so the decorator drops a cached row the stream says moved on.
  it should "drop a cached row when the change stream reports another writer's newer state" in {
    val inner  = new InMemoryUserStateRepository
    val cached = new CachingUserStateRepository(inner)
    cached.watchChanges(onUpsert = _ => (), onDelete = _ => (), onDisconnect = () => ())
    inner.upsert(AliceState)
    cached.find("uuid-alice") shouldBe Some(AliceState) // cached on this pod

    val elsewhere = AliceState.copy(hiddenFilms = Set("Bad Movie", "Hidden Elsewhere"), updatedAt = AliceState.updatedAt.plusSeconds(5))
    inner.upsert(elsewhere) // another pod's write — reaches this one only through the stream

    cached.find("uuid-alice") shouldBe Some(elsewhere)
  }

  it should "drop a cached row on a delete from elsewhere, and everything when the stream loses track" in {
    val inner  = new InMemoryUserStateRepository
    val cached = new CachingUserStateRepository(inner)
    cached.watchChanges(onUpsert = _ => (), onDelete = _ => (), onDisconnect = () => ())
    val bob = AliceState.copy(userId = "uuid-bob")
    inner.upsert(AliceState); inner.upsert(bob)
    cached.find("uuid-alice"); cached.find("uuid-bob")

    inner.delete("uuid-alice")
    cached.find("uuid-alice") shouldBe None

    inner.upsert(bob.copy(hiddenFilms = Set.empty)) // lands while the cursor is down...
    inner.simulateDisconnect()                        // ...which the stream reports as losing track
    cached.find("uuid-bob").map(_.hiddenFilms) shouldBe Some(Set.empty)
  }

  it should "keep the cached row across the stream's echo of this pod's own write" in {
    val inner  = new CountingUserStateRepository()
    val cached = new CachingUserStateRepository(inner)
    cached.watchChanges(_ => (), _ => (), () => ())

    cached.upsert(AliceState) // the in-memory store rings the echo synchronously: same state, nothing moved
    cached.find("uuid-alice") shouldBe Some(AliceState)
    inner.findHits.get() shouldBe 0
  }
}
