package services.auth

import models.User
import services.users.UserRepository

import java.time.Clock
import java.util.Locale

/** The account a provider's profile signs in as: keyed by the lower-cased email,
 *  created on first sign-in, refreshed (provider, its `sub`, name, avatar, last
 *  seen) on every later one. */
final class OauthAccounts(userRepository: UserRepository, clock: Clock) {

  /** The stored row for `profile` — its `sessionVersion` may be a revoke's that
   *  landed after the read here, and it is what the new cookie must carry. Throws
   *  when the profile carries no email: there is nothing to identify the user by. */
  def signIn(provider: String, profile: OauthProfile): User = {
    val now   = clock.instant()
    val email = profile.email.getOrElse(
      throw new RuntimeException(s"OAuth $provider profile has no email — cannot identify user")
    ).toLowerCase(Locale.ROOT)
    val user = userRepository.findById(email) match {
      case Some(existing) =>
        existing.copy(
          provider    = provider,
          providerSub = profile.sub,
          displayName = profile.displayName.orElse(existing.displayName),
          avatarUrl   = profile.avatarUrl.orElse(existing.avatarUrl),
          lastSeenAt  = now
        )
      case None =>
        User(
          id          = email,
          provider    = provider,
          providerSub = profile.sub,
          email       = Some(email),
          displayName = profile.displayName,
          avatarUrl   = profile.avatarUrl,
          createdAt   = now,
          lastSeenAt  = now
        )
    }
    userRepository.upsert(user)
  }
}
