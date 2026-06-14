package services.users

import models.{User, UserState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class AccountDeletionSpec extends AnyFlatSpec with Matchers {

  private def fixture = {
    val userRepository  = new InMemoryUserRepository
    val stateRepository = new InMemoryUserStateRepository
    (new AccountDeletion(userRepository, stateRepository), userRepository, stateRepository)
  }

  "AccountDeletion.delete" should "remove both the user row and the state row" in {
    val (deletion, userRepository, stateRepository) = fixture
    userRepository.upsert(User(
      id = "u1", provider = "google", providerSub = "G-1",
      email = Some("u1@x"), displayName = None, avatarUrl = None,
      createdAt = Instant.now(), lastSeenAt = Instant.now()
    ))
    stateRepository.upsert(UserState("u1", Set("Conclave"), Set.empty, Instant.now()))

    deletion.delete("u1")

    userRepository.findById("u1") shouldBe empty
    stateRepository.find("u1")    shouldBe empty
  }

  it should "be a harmless no-op for an id neither repository knows" in {
    val (deletion, userRepository, stateRepository) = fixture
    noException should be thrownBy deletion.delete("ghost")
    userRepository.findById("ghost") shouldBe empty
    stateRepository.find("ghost")    shouldBe empty
  }
}
