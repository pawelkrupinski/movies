package services.users

import models.{User, UserState}
import tools.contracts.FailsOnPurpose

/** A [[UserRepository]] whose lookups THROW while writes still land on the in-memory
 *  store — the shape `MongoUserRepository` gives a caller when Mongo cannot be read. A
 *  caller must answer "unavailable", never act on the lookup as "no such user". */
class FailingReadUserRepository(failure: => Throwable = new RuntimeException("users unreadable"))
  extends InMemoryUserRepository with FailsOnPurpose {
  override def findById(id: String): Option[User]                                   = throw failure
  override def findByProviderSub(provider: String, providerSub: String): Option[User] = throw failure
  override def findByEmail(email: String): Option[User]                              = throw failure
}

/** A [[UserStateRepository]] whose `find` THROWS while writes still land — see
 *  [[FailingReadUserRepository]]. A failed read must never be served as an empty state. */
class FailingReadUserStateRepository(failure: => Throwable = new RuntimeException("user state unreadable"))
  extends InMemoryUserStateRepository with FailsOnPurpose {
  override def find(userId: String): Option[UserState] = throw failure
}
