package services.movies

import java.util.concurrent.Executors
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * Runs the determinism specs' independent whole-corpus replays CONCURRENTLY
 * rather than serially. Each replay was ~serial CI minutes and the specs ran 3
 * back-to-back, making the `scrape` / `staging` e2e shards the build's long pole;
 * the replays are independent computations, so fanning them out collapses that
 * to ~one replay's wall-clock.
 *
 * Safe to run in one JVM because every replay builds a fully isolated
 * `FixtureTestWiring` (its own cache, repository, staging folder, read model and
 * thread pools). The one process-global on the path — `TitleNormalizer.active` —
 * is loaded at class-init from `TitleRuleDefaults ++ ExtraTitleRules` and
 * never reassigned in tests. Concurrent replays therefore only READ an
 * immutable rule set. The caller's cross-replay byte-equality assertion is
 * the backstop: accidental shared mutable state would surface as a
 * divergence, not a silent pass.
 */
object ParallelReplays {

  /** Compute `replay(seed)` for every seed concurrently, returning the results
   *  in the SAME order as `seeds` (so `head` stays the reference replay). */
  def apply[A](seeds: Seq[Long])(replay: Long => A): Seq[A] = {
    val pool = Executors.newFixedThreadPool(seeds.size.max(1))
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try Await.result(Future.sequence(seeds.map(s => Future(replay(s)))), 30.minutes)
    finally pool.shutdown()
  }
}
