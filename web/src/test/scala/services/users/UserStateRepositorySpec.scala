package services.users

import models.UserState
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class UserStateRepositorySpec extends AnyFlatSpec with Matchers with UserStateWritesContract {

  protected val writesStore  = new InMemoryUserStateRepository
  protected val userIdPrefix = "contract-"

  private val Now = Instant.parse("2026-05-19T12:00:00Z")

  "UserStateRepository" should "return None for a user with no stored state — callers fall back to UserState.empty" in {
    new InMemoryUserStateRepository().find("nobody") shouldBe empty
  }

  it should "round-trip an upserted state via find" in {
    val repository = new InMemoryUserStateRepository
    val s = UserState(
      userId          = "u1",
      hiddenFilms     = Set("Madagaskar"),
      disabledCinemas = Set("Kino Apollo"),
      updatedAt       = Now
    )
    repository.upsert(s)
    repository.find("u1") shouldBe Some(s)
  }

  it should "let upsert overwrite the previous state — second write wins" in {
    val repository = new InMemoryUserStateRepository
    repository.upsert(UserState("u1", Set.empty,    Set.empty,         Now))
    repository.upsert(UserState("u1", Set("Hidden"), Set("Kino Foo"), Now.plusSeconds(60)))
    val got = repository.find("u1").value
    got.hiddenFilms      shouldBe Set("Hidden")
    got.disabledCinemas  shouldBe Set("Kino Foo")
  }

  it should "keep states for different users isolated" in {
    val repository = new InMemoryUserStateRepository
    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repository.upsert(UserState("u2", Set("B"), Set.empty, Now))
    repository.find("u1").value.hiddenFilms shouldBe Set("A")
    repository.find("u2").value.hiddenFilms shouldBe Set("B")
  }

  "UserStateRepository.delete" should "remove the row, leaving subsequent finds empty" in {
    val repository = new InMemoryUserStateRepository
    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repository.find("u1") should be (defined)
    repository.delete("u1")
    repository.find("u1") shouldBe empty
  }

  it should "no-op on a delete of a non-existent userId" in {
    val repository = new InMemoryUserStateRepository
    noException should be thrownBy repository.delete("never-existed")
  }

  "UserState.empty" should "produce a state with everything blank" in {
    val s = UserState.empty("u1", Now)
    s.userId              shouldBe "u1"
    s.hiddenFilms         shouldBe empty
    s.disabledCinemas     shouldBe empty
    s.hiddenFilmsByCountry shouldBe empty
    s.updatedAt           shouldBe Now
  }

  // ── watchChanges (the seam UserChangeTimeCache consumes) ─────────────────

  "InMemoryUserStateRepository.watchChanges" should "dispatch every upsert to the registered listener" in {
    val repository = new InMemoryUserStateRepository
    val seen = scala.collection.mutable.Buffer.empty[UserState]
    repository.watchChanges(onUpsert = seen += _, onDelete = _ => (), onDisconnect = () => ())

    val s = UserState("u1", Set("A"), Set.empty, Now)
    repository.upsert(s)
    seen shouldBe Seq(s)
  }

  it should "dispatch every delete, by userId, to the registered listener" in {
    val repository = new InMemoryUserStateRepository
    val deleted = scala.collection.mutable.Buffer.empty[String]
    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repository.watchChanges(onUpsert = _ => (), onDelete = deleted += _, onDisconnect = () => ())

    repository.delete("u1")
    deleted shouldBe Seq("u1")
  }

  it should "replace the previous registration rather than add a second listener" in {
    val repository = new InMemoryUserStateRepository
    val first  = scala.collection.mutable.Buffer.empty[UserState]
    val second = scala.collection.mutable.Buffer.empty[UserState]
    repository.watchChanges(onUpsert = first += _,  onDelete = _ => (), onDisconnect = () => ())
    repository.watchChanges(onUpsert = second += _, onDelete = _ => (), onDisconnect = () => ())

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    first  shouldBe empty     // detached by the second registration
    second should not be empty
  }

  it should "detach on the returned handle's close — no further dispatch" in {
    val repository = new InMemoryUserStateRepository
    val seen = scala.collection.mutable.Buffer.empty[UserState]
    val handle = repository.watchChanges(onUpsert = seen += _, onDelete = _ => (), onDisconnect = () => ())
    handle.value.close()

    repository.upsert(UserState("u1", Set("A"), Set.empty, Now))
    seen shouldBe empty
  }

  it should "fire onDisconnect on simulateDisconnect — the fake's stand-in for a dead cursor" in {
    val repository = new InMemoryUserStateRepository
    var disconnected = false
    repository.watchChanges(onUpsert = _ => (), onDelete = _ => (), onDisconnect = () => { disconnected = true })

    repository.simulateDisconnect()
    disconnected shouldBe true
  }


  it should "publish an applied hidden-films change to the watcher, and stay silent on a declined one" in {
    val repository = new InMemoryUserStateRepository
    val seen       = scala.collection.mutable.ListBuffer.empty[UserState]
    repository.watchChanges(seen += _, _ => (), () => ())
    repository.upsert(UserState("u1", Set.empty, Set.empty, Now, Map("pl" -> Set("A"))))
    seen.clear()

    val applied = repository.changeHiddenFilms("u1", "pl", HiddenFilmsChange.Hide("A", 1), Now.plusSeconds(1)).value
    repository.changeHiddenFilms("u1", "pl", HiddenFilmsChange.Hide("B", 1), Now.plusSeconds(2))
    seen.toList shouldBe List(applied)
  }

  atomicWritesBehaviour("InMemoryUserStateRepository")
}
