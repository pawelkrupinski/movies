package tools

import models.{Country, User, UserState}
import org.mongodb.scala.{MongoDatabase, ObservableFuture}
import services.MongoConnection
import services.users.{MongoUserRepository, MongoUserStateRepository, UserCodecs}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * One-shot: fold every country's `users` and `userStates` into the ONE database
 * `MONGODB_USERS_DB` names, so that a visitor who signed in on one country is
 * the same account — with the same hidden films and /plan picks — on the next.
 *
 * WHY IT IS NEEDED. `Country.usersDbName` decides where a web pod reads those
 * two collections, and `AppLoader` now leaves the session cookie at the host
 * root so the three Showtimes countries share a sign-in. Both are inert until
 * the rows themselves are in one place: flip the variable without running this
 * and a visitor's session arrives at a database that has never heard of them.
 *
 * ADDITIVE, and that is the safety story. It never deletes or rewrites a SOURCE
 * row — every per-country collection is left exactly as it is — so the cutover
 * is reversible by unsetting `MONGODB_USERS_DB`, and a re-run after more sign-ins
 * simply folds in what is new. Idempotent: merging a row with itself is that row.
 *
 * DRY RUN BY DEFAULT. It prints what it would write and touches nothing. Pass
 * `--write` once the counts look right:
 *
 * ```
 * set -a; . ./.env.local; set +a
 * MONGODB_USERS_DB=kinowo_users sbt 'web/Test/runMain tools.SharedUsersMigration'
 * MONGODB_USERS_DB=kinowo_users sbt 'web/Test/runMain tools.SharedUsersMigration --write'
 * ```
 *
 * Sources are the per-country databases by NAME (`Country.mongoDb`), not through
 * `dbNameFor` — an ambient `MONGODB_DB` would otherwise collapse all four to one
 * and the migration would silently read the same rows four times.
 */
object SharedUsersMigration {

  /** Fold every copy of one person into one row.
   *
   *  `User.id` is the lowercased email, so the same person already carries the
   *  same key in all four databases — this is a union, never a re-key. The
   *  newest row wins the mutable profile fields (they came from the most recent
   *  OAuth callback), but `createdAt` is the EARLIEST seen: the account is as old
   *  as the first country it was made on, and letting a later country's row
   *  overwrite that would quietly re-date every account it touched. */
  def mergeUsers(rows: Seq[User]): Seq[User] =
    rows.groupBy(_.id).values.map(mergeUser).toSeq.sortBy(_.id)

  private[tools] def mergeUser(rows: Seq[User]): User = {
    val newest = rows.maxBy(_.lastSeenAt.toEpochMilli)
    newest.copy(
      createdAt   = rows.map(_.createdAt).minBy(_.toEpochMilli),
      lastSeenAt  = rows.map(_.lastSeenAt).maxBy(_.toEpochMilli),
      // A country where the visitor declined a field must not blank one another
      // country has: the newest row leads, the rest fill the gaps.
      email       = newest.email.orElse(rows.flatMap(_.email).headOption),
      displayName = newest.displayName.orElse(rows.flatMap(_.displayName).headOption),
      avatarUrl   = newest.avatarUrl.orElse(rows.flatMap(_.avatarUrl).headOption))
  }

  /** Fold every copy of one person's state into one row, by UNION.
   *
   *  Safe because the keys are already global and cross-country entries are
   *  inert — `UserState`'s own note: a page only surfaces cinemas in its own
   *  city, so entries from elsewhere are simply ignored. So a union keeps what
   *  the visitor hid in Poland AND what they hid in the UK, and neither shows up
   *  where it does not belong. Last-write-wins would instead throw one country's
   *  choices away on the strength of a timestamp. */
  def mergeStates(rows: Seq[UserState]): Seq[UserState] =
    rows.groupBy(_.userId).values.map(mergeState).toSeq.sortBy(_.userId)

  private[tools] def mergeState(rows: Seq[UserState]): UserState =
    rows.reduce((a, b) => UserState(
      userId          = a.userId,
      hiddenFilms     = a.hiddenFilms     ++ b.hiddenFilms,
      disabledCinemas = a.disabledCinemas ++ b.disabledCinemas,
      updatedAt       = if (a.updatedAt.isAfter(b.updatedAt)) a.updatedAt else b.updatedAt,
      selectedMovies  = a.selectedMovies  ++ b.selectedMovies,
      favouriteRooms  = a.favouriteRooms  ++ b.favouriteRooms))

  // ── The one-shot itself ──────────────────────────────────────────────────

  private val ReadTimeout = 60.seconds

  private def readUsers(db: MongoDatabase): Seq[User] =
    Await.result(db.withCodecRegistry(UserCodecs.registry).getCollection[User]("users").find().toFuture(), ReadTimeout).toSeq

  private def readStates(db: MongoDatabase): Seq[UserState] =
    Await.result(db.withCodecRegistry(UserCodecs.registry).getCollection[UserState]("userStates").find().toFuture(), ReadTimeout).toSeq

  def main(args: Array[String]): Unit = {
    val write  = args.contains("--write")
    val target = Env.get("MONGODB_USERS_DB").map(_.trim).filter(_.nonEmpty).getOrElse {
      println("MONGODB_USERS_DB is not set — nothing to migrate INTO. Refusing to guess.")
      sys.exit(1)
    }

    // One client for every database view, the way the app itself does it.
    val client = MongoConnection.sharedClientFromEnv().getOrElse {
      println("MONGODB_URI is not set.")
      sys.exit(1)
    }

    try {
      val sourced = Country.all.map { country =>
        val db     = client.getDatabase(country.mongoDb)
        val users  = readUsers(db)
        val states = readStates(db)
        println(f"${country.code}%-3s ${country.mongoDb}%-12s ${users.size}%5d users  ${states.size}%5d states")
        (users, states)
      }

      val users  = mergeUsers(sourced.flatMap(_._1))
      val states = mergeStates(sourced.flatMap(_._2))
      println(s"\n→ $target: ${users.size} users, ${states.size} states after merge " +
        s"(${sourced.map(_._1.size).sum - users.size} user rows were the same person on more than one country)")

      if (!write) println("\nDry run — nothing written. Re-run with --write.")
      else {
        val targetDb    = client.getDatabase(target)
        // The production write path, so the rows land under the same codecs and
        // the same indexes the app expects to find.
        val userStore   = new MongoUserRepository(Some(targetDb), fallbackToOwnInit = false)
        val stateStore  = new MongoUserStateRepository(Some(targetDb), fallbackToOwnInit = false)
        users.foreach(userStore.upsert)
        states.foreach(stateStore.upsert)
        userStore.close()
        stateStore.close()
        println(s"\nWrote ${users.size} users and ${states.size} states into $target. " +
          "Source collections untouched — unset MONGODB_USERS_DB to roll back.")
      }
    } finally client.close()
  }
}
