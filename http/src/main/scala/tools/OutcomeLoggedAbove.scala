package tools

/**
 * Whether a layer above logs the failure of the request this thread is making, as the outcome of the whole call --
 * [[RetryingHttpFetch]], which asks a recoverable failure again and warns once if the call still fails
 * ([[RetryWithBackoff]]). The wire leaf ([[RealHttpFetch]]) then keeps its line for that failure at DEBUG: warned at
 * every attempt as well, one broken page asked three times wrote six warnings, and a call that recovered on its second
 * try still wrote two. With no such layer above (a POST, a residential-proxy leg, a tool's bare fetch) the leaf warns.
 *
 * On the calling thread only, like [[RequestedHost]]: every member of the chain below the retry is synchronous.
 */
object OutcomeLoggedAbove {
  private val depth = ThreadLocal.withInitial[Integer](() => 0)

  /** `body` run with its requests' failures logged above. */
  def within[A](body: => A): A = {
    depth.set(depth.get + 1)
    try body
    finally depth.set(depth.get - 1)
  }

  /** A layer above this thread's request logs its failure. */
  def apply(): Boolean = depth.get > 0
}
