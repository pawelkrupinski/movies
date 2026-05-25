package services.users

import models.UserState
import org.scalatest.OptionValues._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class UserStateRepoSpec extends AnyFlatSpec with Matchers {

  private val Now = Instant.parse("2026-05-19T12:00:00Z")

  "UserStateRepo" should "return None for a user with no stored state — callers fall back to UserState.empty" in {
    new InMemoryUserStateRepo().find("nobody") shouldBe empty
  }

  it should "round-trip an upserted state via find" in {
    val repo = new InMemoryUserStateRepo
    val s = UserState(
      userId          = "u1",
      hiddenFilms     = Set("Madagaskar"),
      disabledCinemas = Set("Kino Apollo"),
      updatedAt       = Now
    )
    repo.upsert(s)
    repo.find("u1") shouldBe Some(s)
  }

  it should "let upsert overwrite the previous state — second write wins" in {
    val repo = new InMemoryUserStateRepo
    repo.upsert(UserState("u1", Set.empty,    Set.empty,         Now))
    repo.upsert(UserState("u1", Set("Hidden"), Set("Kino Foo"), Now.plusSeconds(60)))
    val got = repo.find("u1").value
    got.hiddenFilms      shouldBe Set("Hidden")
    got.disabledCinemas  shouldBe Set("Kino Foo")
  }

  it should "keep states for different users isolated" in {
    val repo = new InMemoryUserStateRepo
    repo.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repo.upsert(UserState("u2", Set("B"), Set.empty, Now))
    repo.find("u1").value.hiddenFilms shouldBe Set("A")
    repo.find("u2").value.hiddenFilms shouldBe Set("B")
  }

  "UserStateRepo.delete" should "remove the row, leaving subsequent finds empty" in {
    val repo = new InMemoryUserStateRepo
    repo.upsert(UserState("u1", Set("A"), Set.empty, Now))
    repo.find("u1") should be (defined)
    repo.delete("u1")
    repo.find("u1") shouldBe empty
  }

  it should "no-op on a delete of a non-existent userId" in {
    val repo = new InMemoryUserStateRepo
    noException should be thrownBy repo.delete("never-existed")
  }

  "UserState.empty" should "produce a state with everything blank" in {
    val s = UserState.empty("u1", Now)
    s.userId          shouldBe "u1"
    s.hiddenFilms     shouldBe empty
    s.disabledCinemas shouldBe empty
    s.updatedAt       shouldBe Now
  }

}
