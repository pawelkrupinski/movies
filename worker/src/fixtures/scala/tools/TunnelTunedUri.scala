package tools

/**
 * Connection options for a Mongo URI that crosses a `flyctl proxy`.
 *
 * The tunnel is not a reliable pipe: measured over one convergence run, the proxy
 * process EXITED and was restarted five times in half an hour. The supervisor
 * brings it back within a couple of seconds, but the driver does not find out
 * nearly that fast — it holds pooled connections that are already dead and, on
 * the defaults, spends 30 seconds of server selection per attempt discovering so.
 * A run then sits at 0% CPU for minutes at a time, blocked on a socket nobody is
 * listening to, which is what turned a 12.9 MB archive read into a stall.
 *
 * So the timeouts are tuned for a link that breaks and comes back, rather than one
 * that is either up or down:
 *
 *   - `serverSelectionTimeoutMS` short, so a dead pool fails fast into the caller's
 *     own retry (which backs off 2s→32s and outlasts a restart) instead of blocking.
 *   - `heartbeatFrequencyMS` short, so the driver notices the replacement proxy in
 *     seconds rather than at its default 10s cadence.
 *   - `socketTimeoutMS` bounded, so a half-open socket left by a killed proxy
 *     cannot hang a page read indefinitely — the failure mode that produced
 *     `MongoSocketReadException: Prematurely reached end of stream` only after a
 *     long wait.
 *
 * Applied only to URIs this suite opens across the tunnel; production connections
 * are untouched, because a real network does not need any of this.
 */
object TunnelTunedUri {

  /** Options appended unless the caller already set them — an explicit value in the
   *  secret always wins, so this can never override a deliberate choice. */
  private val Defaults = Seq(
    "serverSelectionTimeoutMS" -> "5000",
    "heartbeatFrequencyMS"     -> "2000",
    "socketTimeoutMS"          -> "45000",
    "connectTimeoutMS"         -> "10000"
  )

  def apply(uri: String): String = {
    val (base, query) = uri.indexOf('?') match {
      case -1 => (uri, "")
      case at => (uri.substring(0, at), uri.substring(at + 1))
    }
    val present = query.split("&").filter(_.nonEmpty)
      .map(_.takeWhile(_ != '=').toLowerCase(java.util.Locale.ROOT)).toSet
    val additions = Defaults
      .filterNot { case (name, _) => present.contains(name.toLowerCase(java.util.Locale.ROOT)) }
      .map { case (name, value) => s"$name=$value" }

    if (additions.isEmpty) uri
    else {
      val parts = (query.split("&").filter(_.nonEmpty) ++ additions).mkString("&")
      s"$base?$parts"
    }
  }
}
