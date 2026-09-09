package services.staging

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * `MongoStagingFolder.retryBackoffMs` — the jittered delay `foldWithRetry`
 * sleeps before looping back to `startTransaction()` on a `StagingFold.Next.Retry`.
 *
 * Exists because a zero-backoff retry made a genuine (if rare) three-way race
 * possible: two losing folds that retry IMMEDIATELY tend to re-collide with
 * EACH OTHER rather than with the by-then-committed winner —
 * `StagingFoldConcurrentTmdbRaceIntegrationSpec`'s three-way race (its
 * `CyclicBarrier` deliberately starts every racer at the same instant) hit
 * exactly this on CI 2026-09-09: a second collision on the last of
 * `maxRetries` attempts abandoned instead of converging. This pins the pure
 * delay CALCULATION only — the real-Mongo convergence claim is the it/ spec's
 * job, and it can't inject a controlled collision to prove backoff fixed it
 * (the race it guards against is exactly the kind neither a fast dev Mac nor
 * an isolated local run reproduces on demand; see the commit message).
 */
class MongoStagingFolderBackoffSpec extends AnyFlatSpec with Matchers {

  "retryBackoffMs" should "grow with the attempt number" in {
    // Base component only (10 * attempt); jitter is additive and non-negative,
    // so comparing the MINIMUM each attempt can produce over many samples is
    // deterministic even though a single call is randomised.
    def minOverManySamples(attempt: Int): Long =
      (1 to 500).map(_ => MongoStagingFolder.retryBackoffMs(attempt)).min

    minOverManySamples(1) should be < minOverManySamples(2)
    minOverManySamples(2) should be < minOverManySamples(3)
  }

  it should "stay within [10*attempt, 10*attempt + 20) for every attempt" in {
    for (attempt <- 1 to 5; _ <- 1 to 200) {
      val delay = MongoStagingFolder.retryBackoffMs(attempt)
      delay should be >= (10L * attempt)
      delay should be < (10L * attempt + 20L)
    }
  }

  it should "not be constant — the whole point is to de-synchronize simultaneous retriers" in {
    val samples = (1 to 50).map(_ => MongoStagingFolder.retryBackoffMs(1)).toSet
    // Extremely unlikely to collapse to one value by chance over 50 draws from
    // a 20-wide range unless jitter is broken (e.g. `nextInt(0)` or a fixed seed).
    samples.size should be > 1
  }
}
