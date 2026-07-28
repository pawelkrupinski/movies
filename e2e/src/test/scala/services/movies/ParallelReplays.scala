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
 * Safe to run in one JVM because every replay builds a fully isolated wiring
 * (its own cache, repository, staging folder, read model and thread pools).
 *
 * The one process-global on the path is `TitleNormalizer.active`. The fixture
 * replays never touch it. The per-country convergence specs DO — each installs
 * its country's `TitleRuleSet` — but they install it ONCE, before any replay
 * starts, and every replay in a run belongs to the same country, so concurrent
 * replays still only READ a rule set that is stable for their whole life. What
 * would be unsafe is installing rules while replays are in flight, or fanning
 * out replays of two different countries; neither happens, and the per-country
 * spec-and-CI-job split exists precisely to keep it that way.
 *
 * The caller's cross-replay byte-equality assertion is the backstop: accidental
 * shared mutable state would surface as a divergence, not a silent pass.
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
