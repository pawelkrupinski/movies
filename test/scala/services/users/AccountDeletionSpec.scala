package services.users

import models.{User, UserState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class AccountDeletionSpec extends AnyFlatSpec with Matchers {

  private def fixture = {
    val userRepo  = new InMemoryUserRepo
    val stateRepo = new InMemoryUserStateRepo
    (new AccountDeletion(userRepo, stateRepo), userRepo, stateRepo)
  }

  "AccountDeletion.delete" should "remove both the user row and the state row" in {
    val (deletion, userRepo, stateRepo) = fixture
    userRepo.upsert(User(
      id = "u1", provider = "google", providerSub = "G-1",
      email = Some("u1@x"), displayName = None, avatarUrl = None,
      createdAt = Instant.now(), lastSeenAt = Instant.now()
    ))
    stateRepo.upsert(UserState("u1", Set("Conclave"), Set.empty, Instant.now()))

    deletion.delete("u1")

    userRepo.findById("u1") shouldBe empty
    stateRepo.find("u1")    shouldBe empty
  }

  it should "be a harmless no-op for an id neither repo knows" in {
    val (deletion, userRepo, stateRepo) = fixture
    noException should be thrownBy deletion.delete("ghost")
    userRepo.findById("ghost") shouldBe empty
    stateRepo.find("ghost")    shouldBe empty
  }
}
