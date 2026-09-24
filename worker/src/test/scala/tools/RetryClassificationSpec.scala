package tools

import com.mongodb.MongoWriteException
import modules.wiring.EgressWiring
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.{MongoErrors, UptimeMonitor}
import services.cinemas.common.DetailFetchOutcome
import services.scrapes.GoneUpstream
import services.tasks.TaskWorker
import tools.contracts.RetryClassificationTable
import tools.contracts.RetryClassificationTable.{Row, Verdict}

import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Try}

/**
 * Holds the worker's production classifiers to `test/resources/retry-classification.json`,
 * row by row. The verdict is DERIVED from what the production code does with the failure,
 * never restated here:
 *
 *  - permanent — the origin said the page is gone (`EnrichmentRead.isAbsent`, which the
 *    detail caches, the egress chain and the slug ladders all read), a task failure the
 *    pool drops without retry (`TaskWorker.isDeterministic`), or a duplicate key.
 *  - transient — anything the origin's own signals count: an origin status, a failure that
 *    trips the host's circuit breaker, or one /uptime books as the host failing.
 *  - provider — none of those: an egress route's own failure, which the chain falls
 *    through and which never reaches the origin's breaker, uptime row or gone-verdict.
 *
 * The history this guards: Zyte's 429 opening the origin's breaker and its 404 stamping a
 * page gone (0ba08b827), a require failure retried to exhaustion (0e471cfb3), an origin
 * 404 through the proxy retried as a composite failure (57db70023).
 */
class RetryClassificationSpec extends AnyFlatSpec with Matchers {
  import RetryClassificationFailures.Url

  private val table = RetryClassificationTable.load

  private val WorkerSources = table.sourcesConsumedBy("worker")

  "the retry-classification table" should "give every worker source rows this spec derives a verdict for" in {
    WorkerSources shouldBe Set("origin", "origin-via-zyte", "zyte", "decodo", "task", "mongo-write")
    WorkerSources.foreach(source => table.rowsFor(source) should not be empty)
  }

  for (row <- table.rows if WorkerSources.contains(row.source)) {
    it should s"agree with the worker on $row" in {
      verdictOf(row) shouldBe row.verdict
    }
  }

  // Every place that reads "is this origin failure final" must read it the same way; they
  // drifted before (a detail cache remembering what the uptime quarantine did not).
  for (row <- table.rows if Set("origin", "origin-via-zyte", "zyte").contains(row.source)) {
    it should s"have every gone-reader agree on $row" in {
      val failure = RetryClassificationFailures.of(row)
      val absent  = EnrichmentRead.isAbsent(failure)
      withClue("FallbackHttpFetch.OriginAnswered: ")(FallbackHttpFetch.OriginAnswered(failure) shouldBe absent)
      withClue("DetailFetchOutcome.of: ")(DetailFetchOutcome.of(Failure(failure)).isInstanceOf[DetailFetchOutcome.Gone] shouldBe absent)
      withClue("GoneUpstream.saysPageIsGone: ")(GoneUpstream.saysPageIsGone(failure.getMessage) shouldBe absent)
    }
  }

  private def verdictOf(row: Row): Verdict = {
    val failure = RetryClassificationFailures.of(row)
    row.source match {
      case "task"        => if (TaskWorker.isDeterministic(failure)) Verdict.Permanent else Verdict.Transient
      case "mongo-write" => failure match {
        case write: MongoWriteException if MongoErrors.isDuplicateKey(write) => Verdict.Permanent
        case _                                                              => Verdict.Transient
      }
      // A proxy tunnel failure is an IOException like any connection reset; what makes it
      // the provider's is WHERE it happens — so it is judged by the route it happens on.
      case "decodo" => if (fallsThroughProxyLeg(failure)) Verdict.Provider else httpVerdict(failure)
      case _        => httpVerdict(failure)
    }
  }

  private def httpVerdict(failure: Throwable): Verdict =
    if (EnrichmentRead.isAbsent(failure)) Verdict.Permanent
    else if (countsAgainstOrigin(failure)) Verdict.Transient
    else Verdict.Provider

  private def countsAgainstOrigin(failure: Throwable): Boolean =
    failure.isInstanceOf[HttpStatusException] || tripsOriginBreaker(failure) ||
      new MonitoringHttpFetch(throwing(failure), new UptimeMonitor()).isFailure(failure)

  /** Whether one such failure opens the host's breaker (threshold 1). */
  private def tripsOriginBreaker(failure: Throwable): Boolean = {
    val breaker = new HostCircuitBreakerHttpFetch(throwing(failure), failureThreshold = 1)
    Try(breaker.get(Url))
    breaker.openRemainingMillis("www.odeon.co.uk") > 0
  }

  /** The production proxy chain (`EgressWiring.proxyPrimary`) with its proxy leg failing
   *  this way every time: a provider failure must never end the chain nor stop the next
   *  route answering, however often it repeats. */
  private def fallsThroughProxyLeg(failure: Throwable): Boolean = {
    val fallbackCalls = new AtomicInteger
    val fallback = new HttpFetch {
      def get(url: String): String = { fallbackCalls.incrementAndGet(); "ok" }
      def post(url: String, body: String, contentType: String): String = get(url)
    }
    val chain   = EgressWiring.proxyPrimary(IndexedSeq(throwing(failure)), fallback)
    val answers = (1 to 10).map(_ => Try(chain.get(Url)).toOption)
    answers.forall(_.contains("ok")) && fallbackCalls.get == 10 &&
      !failure.isInstanceOf[HttpStatusException] && !EnrichmentRead.isAbsent(failure)
  }

  private def throwing(failure: Throwable): HttpFetch = new HttpFetch {
    def get(url: String): String                                      = throw failure
    def post(url: String, body: String, contentType: String): String = throw failure
  }
}
