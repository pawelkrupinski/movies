package services.staging

import com.mongodb.{MongoException, ServerAddress}
import com.mongodb.MongoSocketReadException
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
}
