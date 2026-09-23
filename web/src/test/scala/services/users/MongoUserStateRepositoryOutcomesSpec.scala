package services.users

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.users.UserStateWriteOutcomes.{Endpoint, Outcome}

import java.time.Instant
import scala.collection.mutable

/** The write outcomes `MongoUserStateRepository` reports for the paths that need
 *  no database: a pod whose users store never came up answers every write with
 *  a 503, and that has to be countable as `unavailable` rather than lost among
 *  Mongo throwing (`store_failure`). The `ok` / `conflict` paths are driven
 *  against real Mongo in `HiddenFilmsConcurrentWritesIntegrationSpec`. */
class MongoUserStateRepositoryOutcomesSpec extends AnyFlatSpec with Matchers {

  private def recording() = {
    val seen = mutable.ListBuffer.empty[(String, String)]
    val outcomes: UserStateWriteOutcomes = (endpoint: String, outcome: String) => seen += (endpoint -> outcome)
    (seen, outcomes)
  }

  "a users store that never came up" should "report each refused write as unavailable, on its own endpoint" in {
    val (seen, outcomes) = recording()
    val store = new MongoUserStateRepository(sharedDb = None, fallbackToOwnInit = false, writeOutcomes = outcomes)
    val now   = Instant.parse("2026-09-23T12:00:00Z")

    store.changeHiddenFilms("u", "pl", HiddenFilmsChange.Hide("Film", 10), now) shouldBe None
    store.changeHiddenFilms("u", "pl", HiddenFilmsChange.Unhide("Film"), now) shouldBe None
    store.changeHiddenFilms("u", "pl", HiddenFilmsChange.Clear, now) shouldBe None
    store.patchLegacyState("u", LegacyStatePatch(None, None, Some(Some("en"))), now) shouldBe None

    seen.toList shouldBe List(
      Endpoint.Hide      -> Outcome.Unavailable,
      Endpoint.Unhide    -> Outcome.Unavailable,
      Endpoint.Clear     -> Outcome.Unavailable,
      Endpoint.LegacyPut -> Outcome.Unavailable)
  }
}
