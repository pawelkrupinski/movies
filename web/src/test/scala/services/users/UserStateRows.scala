package services.users

import com.mongodb.client.model.ReplaceOptions
import models.UserState
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoDatabase, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * A whole-row write to `userStates` — for seeding a Mongo store in a spec, and for the
 * `tools.SharedUsersMigration` fold. Test/tooling support ONLY.
 *
 * This used to be `UserStateRepository.upsert`, on the production trait, where nothing in
 * production called it: every request-path write is one of the atomic, field-scoped
 * pipelines (`changeHiddenFilms`, `patchLegacyState`), because a whole-row replace racing
 * a hide on another pod erases one of them. Keeping the replace on the trait invited
 * exactly that write back into the request path, so it lives here instead; the in-memory
 * store keeps its own `upsert` for seeding.
 *
 * Unlike the repository's best-effort writes this THROWS on failure — a seed that did not
 * land must fail the spec, not log a warning and carry on.
 */
object UserStateRows {

  /** Replace `state.userId`'s row with `state`, creating it if absent. */
  def replace(db: MongoDatabase, state: UserState): Unit = {
    Await.result(
      db.withCodecRegistry(UserCodecs.registry).getCollection[UserState](UserStateRepository.Collection)
        .replaceOne(Filters.eq("userId", state.userId), state, new ReplaceOptions().upsert(true)).toFuture(),
      10.seconds)
    ()
  }
}
