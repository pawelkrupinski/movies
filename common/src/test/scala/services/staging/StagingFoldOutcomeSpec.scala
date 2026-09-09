package services.staging

import com.mongodb.{MongoException, MongoWriteException, ServerAddress, WriteError}
import com.mongodb.MongoSocketReadException
import org.bson.BsonDocument
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

/**
 * What a transaction attempt's outcome MEANS — pinned separately from the I/O because the
 * distinction it draws is the whole bug.
 *
 * A failed fold used to be reported as `Seq.empty`, which is byte-identical to a clean
 * fold that promoted nothing. The task was marked Done with its staging rows still in
 * place, `StagingReaper` re-enqueued the same fold every tick, and `pending_movies` grew
 * without bound — 1100+ rows / 273 films on prod PL before anyone looked. The
 * `Missing field: sourceData` decode bug rode that silence for hours.
 */
class StagingFoldOutcomeSpec extends AnyFlatSpec with Matchers {

  private val maxRetries = 3
  private def transient: MongoException = {
    val e = new MongoSocketReadException("boom", new ServerAddress("localhost", 27017), new RuntimeException)
    e.addLabel(MongoException.TRANSIENT_TRANSACTION_ERROR_LABEL)
    e
  }

  // The prod PL 2026-09-08 incident, verbatim shape: two decorated 'Lalka' spellings
  // (tmdbId 1321666) folded as separate title-groups, and whichever committed second
  // hit the partial unique index its sibling had just satisfied.
  private def tmdbIdRace: MongoWriteException = new MongoWriteException(
    new WriteError(11000,
      "E11000 duplicate key error collection: kinowo.movies index: tmdbId_1 dup key: { tmdbId: 1321666 }",
      new BsonDocument()),
    new ServerAddress("localhost", 27017), java.util.Collections.emptyList[String]())

  private def keyCollision: MongoWriteException = new MongoWriteException(
    new WriteError(11000,
      "E11000 duplicate key error collection: kinowo.movies index: key_1 dup key: { key: \"lalka|2026\" }",
      new BsonDocument()),
    new ServerAddress("localhost", 27017), java.util.Collections.emptyList[String]())

  "a successful attempt" should "commit, promotions and all" in {
    StagingFold.nextAfterAttempt(Success(Seq.empty), attempt = 1, maxRetries) shouldBe
      StagingFold.Next.Commit(Seq.empty)
  }

  "a transient txn error with retries left" should "go round again" in {
    StagingFold.nextAfterAttempt(Failure(transient), attempt = 1, maxRetries) shouldBe a[StagingFold.Next.Retry]
  }

  "a transient txn error out of retries" should "be abandoned, not silently committed" in {
    StagingFold.nextAfterAttempt(Failure(transient), attempt = maxRetries, maxRetries) shouldBe
      a[StagingFold.Next.Abandon]
  }

  // THE regression. A decode failure inside the transaction body is not transient and
  // never will be: retrying re-reads the same undecodable document.
  "a non-transient failure" should "be abandoned rather than reported as an empty fold" in {
    val cause = new org.bson.BsonInvalidOperationException("Missing field: sourceData")
    val next  = StagingFold.nextAfterAttempt(Failure(cause), attempt = 1, maxRetries)
    next                                        shouldBe StagingFold.Next.Abandon(cause)
    // …and specifically NOT the value a fold that promoted nothing returns.
    next                                 should not be StagingFold.Next.Commit(Seq.empty)
  }

  // THE 2026-09-08 regression. A losing race against a DIFFERENT title-group's fold for
  // the same tmdbId used to abandon on the very first attempt (a duplicate-key write
  // error carries no transient label), rethrowing and rescheduling the whole task under
  // backoff instead of just re-reading — noisy, and slow to converge. A retry is safe
  // here specifically because the winner's write is already majority-committed by the
  // time the loser's fails, so the very next attempt sees it as a sibling and merges.
  "a losing race against a sibling fold for the same tmdbId, with retries left" should
    "go round again rather than abandon on the first attempt" in {
    StagingFold.nextAfterAttempt(Failure(tmdbIdRace), attempt = 1, maxRetries) shouldBe a[StagingFold.Next.Retry]
  }

  "a losing race against a sibling fold for the same tmdbId, out of retries" should
    "be abandoned, not silently committed" in {
    StagingFold.nextAfterAttempt(Failure(tmdbIdRace), attempt = maxRetries, maxRetries) shouldBe
      a[StagingFold.Next.Abandon]
  }

  // A `key_1` collision is a DIFFERENT situation: two clusters `clusterByFilm` correctly
  // kept apart (disagreeing tmdbId/imdbId) both concluding the identical (sanitize, year)
  // key — `planGroup` plans both, undeferred (see `StagingFoldSpec`'s "plan two DIFFERENT
  // films at the identical key"). The same two clusters produce the same collision every
  // retry, so — unlike the tmdbId race above — retrying cannot help; it must abandon
  // immediately like any other non-transient failure.
  "a key_1 collision between two genuinely different films" should
    "be abandoned rather than retried" in {
    StagingFold.nextAfterAttempt(Failure(keyCollision), attempt = 1, maxRetries) shouldBe a[StagingFold.Next.Abandon]
  }
}
