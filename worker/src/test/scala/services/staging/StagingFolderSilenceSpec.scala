package services.staging

import models.{Multikino, MovieRecord}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoConnection
import services.movies.InMemoryMovieRepository

/**
 * That a fold which COULD NOT ACT says so, instead of returning the same empty answer as
 * a fold that had nothing to do.
 *
 * This is the shape that cost a day. `MongoStagingFolder.foldGroup` returned `Seq.empty`
 * when it could not start a session or reach its collections — identical to the answer
 * for "this group is already folded" — so a convergence leg on a real database graduated
 * nothing, logged nothing, and looked exactly like a corpus with no work outstanding.
 * Three separate diagnoses went past it, because an empty answer is not evidence of
 * anything.
 *
 * The distinction is the whole point of these two tests, and neither is about fold LOGIC:
 * that has some twenty specs and every one of them was green throughout. What had no test
 * was whether the code can be TRUSTED WHEN IT SAYS NOTHING HAPPENED.
 */
class StagingFolderSilenceSpec extends AnyFlatSpec with Matchers {

  "a staging folder that cannot reach Mongo" should "fail loudly rather than report an empty fold" in {
    val disabled = new MongoConnection(uri = None, dbName = "kinowo", required = false)

    val failure = the [IllegalStateException] thrownBy new MongoStagingFolder(disabled).foldGroup("Ghost In The Shell")

    withClue("the message must name the cause, since the caller sees only an exception: ") {
      failure.getMessage.toLowerCase should include ("fold")
    }
  }

  // The other half, so the first isn't satisfied by a folder that simply always throws:
  // an empty answer must stay available for the case it legitimately describes.
  it should "still return empty when there is genuinely nothing to fold" in {
    val staging = new InMemoryStagingRepository
    staging.upsert(Multikino, "Ghost In The Shell", Some(2017), MovieRecord())

    new InMemoryStagingFolder(staging, new InMemoryMovieRepository).foldGroup("nothing-matches-this") shouldBe empty
  }
}
