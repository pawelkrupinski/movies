package tools

/**
 * A failure that says the host's name did not resolve. Usually that describes the site — its
 * domain lapsed — but the JDK raises the same for a resolver that timed out or failed, so a
 * reader must not take one sighting as proof: the closure sweep only REQUESTS a retirement,
 * which the retire workflow's own live re-check confirms or drops. Two shapes reach a caller: the JDK
 * `HttpClient` (what [[RealHttpFetch]] sends with) throws a `ConnectException` whose
 * cause is `java.nio.channels.UnresolvedAddressException`, while `InetAddress` and the
 * older stacks throw `java.net.UnknownHostException`. Either may sit anywhere in the
 * cause chain, so the whole chain is walked.
 */
object UnresolvedHost {
  def unapply(failure: Throwable): Boolean =
    Iterator.iterate(failure)(_.getCause).takeWhile(_ != null).take(16).exists {
      case _: java.nio.channels.UnresolvedAddressException | _: java.net.UnknownHostException => true
      case _                                                                                  => false
    }
}
