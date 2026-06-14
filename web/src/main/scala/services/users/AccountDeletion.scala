package services.users

import play.api.Logging

/**
 * The single account-deletion operation, shared by the in-app
 * `DELETE /api/me` path and Facebook's data-deletion callback. Drops
 * the user's personalization-state row and then the user row — after
 * this we hold nothing keyed by that id.
 *
 * Idempotent: deleting an id neither repository knows is a harmless no-op,
 * so the Facebook callback can fire for a user who never logged in
 * here (or who already deleted) without special-casing.
 */
class AccountDeletion(userRepository: UserRepository, userStateRepository: UserStateRepository) extends Logging {
  def delete(userId: String): Unit = {
    logger.info(s"Deleting account $userId (state + user row)")
    userStateRepository.delete(userId)
    userRepository.delete(userId)
  }
}
