package services.users

import models.UserState
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration._

class UserChangeTimeCacheSpec extends AnyFlatSpec with Matchers {

  private val Now = Instant.parse("2026-05-19T12:00:00Z")

  "CaffeineUserChangeTimeCache" should "have no answer for a user it's never seen" in {
    val cache = new CaffeineUserChangeTimeCache(new InMemoryUserStateRepository)
    cache.start()
    cache.lastChangeAt("nobody") shouldBe empty
  }

  it should "learn a user's change time from the repository's change stream" in {
    val repository = new InMemoryUserStateRepository
    val cache = new CaffeineUserChangeTimeCache(repository)
    cache.start()

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    cache.lastChangeAt("u1") shouldBe Some(Now)
  }

  it should "update on a second write to the same user" in {
    val repository = new InMemoryUserStateRepository
    val cache = new CaffeineUserChangeTimeCache(repository)
    cache.start()

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repository.upsert(UserState("u1", Set("A", "B"), Set.empty, Now.plusSeconds(60)))
    cache.lastChangeAt("u1") shouldBe Some(Now.plusSeconds(60))
  }

  it should "forget a user on a delete" in {
    val repository = new InMemoryUserStateRepository
    val cache = new CaffeineUserChangeTimeCache(repository)
    cache.start()

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repository.delete("u1")
    cache.lastChangeAt("u1") shouldBe empty
  }

  it should "have no answer for anyone when the repository has no change stream to offer" in {
    val cache = new CaffeineUserChangeTimeCache(new NoWatchUserStateRepository)
    cache.start()
    // Nothing to assert beyond "doesn't throw" — the repository's watchChanges
    // returns None, exactly like a repository with no Mongo change streams.
    cache.lastChangeAt("anyone") shouldBe empty
  }

  // THE POLICY THIS CACHE EXISTS TO GET RIGHT: unlike `MovieCache`, a stream
  // failure must clear everything, not just stop updating — a stale positive
  // here silently tells a real requester "nothing changed" when it did.
  it should "invalidate EVERY entry — not just the affected user's — when the stream disconnects" in {
    val repository = new InMemoryUserStateRepository
    val cache = new CaffeineUserChangeTimeCache(repository)
    cache.start()

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repository.upsert(UserState("u2", Set("B"), Set.empty, Now))
    cache.lastChangeAt("u1") shouldBe defined
    cache.lastChangeAt("u2") shouldBe defined

    repository.simulateDisconnect()

    cache.lastChangeAt("u1") shouldBe empty
    cache.lastChangeAt("u2") shouldBe empty
  }

  it should "resume learning changes after a stream disconnect — the registration itself survives" in {
    val repository = new InMemoryUserStateRepository
    val cache = new CaffeineUserChangeTimeCache(repository)
    cache.start()

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repository.simulateDisconnect()
    cache.lastChangeAt("u1") shouldBe empty

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now.plusSeconds(60)))
    cache.lastChangeAt("u1") shouldBe Some(Now.plusSeconds(60))
  }

  it should "stop dispatching to this cache once stopped" in {
    val repository = new InMemoryUserStateRepository
    val cache = new CaffeineUserChangeTimeCache(repository)
    cache.start()
    cache.stop()

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    cache.lastChangeAt("u1") shouldBe empty
  }

  it should "evict the least-recently-used entry once the size cap is hit" in {
    val repository = new InMemoryUserStateRepository
    val cache = new CaffeineUserChangeTimeCache(repository, maxEntries = 2)
    cache.start()

    repository.upsert(UserState("u1", Set.empty, Set.empty, Now))
    repository.upsert(UserState("u2", Set.empty, Set.empty, Now))
    cache.lastChangeAt("u1") // touch u1 so u2 is the least-recently-used one
    repository.upsert(UserState("u3", Set.empty, Set.empty, Now))

    cache.lastChangeAt("u1") shouldBe defined
    cache.lastChangeAt("u3") shouldBe defined
    cache.lastChangeAt("u2") shouldBe empty
  }

  it should "not trust an entry older than entryTtl even with no disconnect at all" in {
    val repository = new InMemoryUserStateRepository
    val cache = new CaffeineUserChangeTimeCache(repository, entryTtl = 1.millisecond)
    cache.start()

    repository.upsert(UserState("u1", Set.empty, Set.empty, Now))
    Thread.sleep(20)
    cache.lastChangeAt("u1") shouldBe empty
  }

  /** A repository that behaves like one with no change-stream support at all
   *  (a store that can't stream, per `UserStateRepository.watchChanges`'s doc). */
  private class NoWatchUserStateRepository extends InMemoryUserStateRepository {
    override def watchChanges(
      onUpsert:     UserState => Unit,
      onDelete:     String => Unit,
      onDisconnect: () => Unit
    ): Option[AutoCloseable] = None
  }
}
