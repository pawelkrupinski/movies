package models

import java.time.Instant

case class UserState(
  userId:          String,
  hiddenFilms:     Set[String],
  disabledCinemas: Set[String],
  updatedAt:       Instant
)

object UserState {
  def empty(userId: String, now: Instant = Instant.now()): UserState =
    UserState(userId, Set.empty, Set.empty, now)
}
