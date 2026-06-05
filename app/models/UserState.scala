package models

import java.time.Instant

// Multi-city note: `disabledCinemas` and `favouriteRooms` key on globally-
// unique cinema display names ("Helios Posnania", "Cinema City Kinepolis"|room),
// so they never collide across cities — a future "Helios Wrocław" is a distinct
// Cinema with a distinct displayName. Cross-city keys are inert: a page only
// surfaces cinemas in its own city, so out-of-city entries are simply ignored.
// Hence user state needs no city dimension.
case class UserState(
  userId:          String,
  hiddenFilms:     Set[String],
  disabledCinemas: Set[String],
  updatedAt:       Instant,
  // /plan picks. `selectedMovies` is the inverse of `hiddenFilms` — titles
  // the user wants to schedule. `favouriteRooms` is a set of composite
  // `"<Cinema displayName>|<Room>"` keys; per-cinema semantics on /plan are
  // "no entries for this cinema → all rooms OK, any entries → only those
  // rooms OK". The same key shape is used elsewhere (badge `data-room` on
  // the grid pages, the Filtry → Sale list).
  selectedMovies:  Set[String]       = Set.empty,
  favouriteRooms:  Set[String]       = Set.empty
)

object UserState {
  def empty(userId: String, now: Instant = Instant.now()): UserState =
    UserState(userId, Set.empty, Set.empty, now)
}
