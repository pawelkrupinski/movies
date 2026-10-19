package controllers

import services.users.InMemoryUserRepository

import java.time.Instant

/** The person the auth specs sign in: a Google account named by its email. */
object SignedInVisitors {

  /** `email`'s account in `repository`, created and last seen `at`: its user id. */
  def signedIn(repository: InMemoryUserRepository, email: String, at: Instant): String = {
    repository.upsert(models.User(
      id          = email,
      provider    = "google",
      providerSub = s"sub-$email",
      email       = Some(email),
      displayName = Some("Alice"),
      avatarUrl   = None,
      createdAt   = at,
      lastSeenAt  = at))
    email
  }
}
