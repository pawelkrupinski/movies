package tools

import scala.concurrent.ExecutionContextExecutorService

/** An [[ExecutionBudget]] whose every execution context runs tasks INLINE on the
 *  calling thread (see [[SameThreadExecutorService]]). Wired by the determinism
 *  specs so the whole background cascade — TMDB resolution AND IMDb-id recovery,
 *  the two pools `backgroundBudget` hands out — is single-threaded and driven by
 *  ORDERING (the seeded arrival/event shuffle) alone, with no `Thread.sleep` jitter
 *  and no scheduler race. The sub-limit overload ignores its cap: with one thread
 *  there is never more than one task running anyway. */
class SameThreadExecutionBudget extends ExecutionBudget {
  override def executionContext(name: String): ExecutionContextExecutorService =
    SameThreadExecutorService.newEC()
  override def executionContext(name: String, subLimit: Int): ExecutionContextExecutorService =
    SameThreadExecutorService.newEC()
}
