package services.users

/**
 * Where `MongoUserStateRepository` reports whether `userStates` has the UNIQUE
 * `userId` index its atomic writes depend on — the seam
 * `services.metrics.UserStateIndexMetrics` exports through, so the store knows
 * nothing about Prometheus.
 *
 * Reported once per boot, when the store first reaches its collection. Without
 * the index an upsert keyed on `userId` cannot tell "this user's row" from "the
 * first of several", and duplicate rows (which is what makes the build fail)
 * leave reads and writes disagreeing about which row is the user's.
 */
trait UserStateIndexHealth {
  def uniqueUserIdIndex(present: Boolean): Unit
}

object UserStateIndexHealth {
  val none: UserStateIndexHealth = (_: Boolean) => ()
}
