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

  /** The bound a caller gets when it names none.
   *
   *  30 minutes was sized when a replay ran entirely in memory. Every collection is on
   *  a real database now, so a pass does the persistence work too — and the UK corpus is
   *  788 venues / 24k listings, roughly 3.5x Poland's, with all `seeds.size` passes
   *  contending on one mongod. That leg's replays went from ~10 minutes to over 30 and
   *  died on this bound, taking a leg that had otherwise gone FASTER (the warm rating
   *  cache cut its boot by 18 minutes) down with them. */
  val DefaultWithin: FiniteDuration = 75.minutes

  /** Compute `replay(seed)` for every seed concurrently, returning the results
   *  in the SAME order as `seeds` (so `head` stays the reference replay).
   *
   *  `within` is a RUNAWAY GUARD, not a budget: it exists so a wedged replay fails
   *  instead of hanging until the job is cancelled, which would discard the recorded
   *  fixtures the next run needs. It is the CALLER's to size because the right value is
   *  a fact about the caller's corpus and its CI step ceiling, not about this helper —
   *  a single constant here was raised twice (30 → 62 → 75) by whichever country had
   *  just outgrown it, and each raise silently re-armed the same trap for the next one.
   *  The United States sprang it: three concurrent passes over 4,304 venues / 121,544
   *  listings need something on the order of four hours, and were killed at 75 minutes
   *  having diverged on nothing. Keep it just under the step ceiling that wraps the run,
   *  so an overrun FAILS the step (which reports) rather than cancelling the job (which
   *  does not). */
  def apply[A](seeds: Seq[Long], within: FiniteDuration = DefaultWithin)(replay: Long => A): Seq[A] = {
    val pool = Executors.newFixedThreadPool(seeds.size.max(1))
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)
    try Await.result(Future.sequence(seeds.map(s => Future(replay(s)))), within)
    finally pool.shutdown()
  }
}
