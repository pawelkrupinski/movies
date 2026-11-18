package tools

/** What a failed request says about its HOST, as [[HostCircuitBreakerHttpFetch]] reads it: whether it counts toward
 *  opening the host's breaker, whether it says nothing at all, which page it is about, and how it reads in one line. */
object HostFailure {

  /** Trip-worthy = the host is failing to serve us: a timeout (request OR connect —
   *  HttpConnectTimeoutException is a subtype), a 5xx, a 429, or a lower-level IO
   *  error (connection refused/reset).
   *
   *  429 is the one 4xx that counts. Every other 4xx is the host ANSWERING (404 is
   *  a real "not found" for the MC/RT slug probes) — but a 429 is it declining to,
   *  and `failureThreshold` CONSECUTIVE ones (any success resets the count) mean it
   *  is refusing us outright rather than shaping a burst. Without this the breaker
   *  could not open on the case that most needs it: Filmstarts 429'd every request
   *  for hours on 2026-07-18 while the worker kept firing ~14k of them an hour,
   *  since ThrottledHttpFetch's gate only paces retries and never gives up. */
  def isTripWorthy(e: Throwable): Boolean = e match {
    case s: HttpStatusException                 => s.code >= 500 || s.code == 429
    case _: java.net.http.HttpTimeoutException  => true
    case _: java.io.IOException                 => true
    case _                                      => false
  }

  /** A call that ended before the host could answer -- never sent ([[NeverSent]]) or abandoned by its caller
   *  ([[CallerCancelled]]): neither a failure nor the host answering, so the breaker records nothing. An interrupted
   *  half-open probe used to count as an answer and close the breaker on a host that had said nothing. */
  def saysNothing(e: Throwable): Boolean = e.isInstanceOf[NeverSent] || CallerCancelled(e)

  /** The page a failure is about, when it is about one: a server error is the host answering for that URL, so it
   *  counts once per page. Any other trip-worthy failure (a timeout, a lost connection, a 429) is the host's, and
   *  counts every time. */
  def failingPage(url: String, cause: Throwable): Option[String] = cause match {
    case s: HttpStatusException if s.code >= 500 => Some(url)
    case _                                       => None
  }

  /** The causing failure as one short line for the breaker to carry — type plus
   *  message, since `SSLHandshakeException` and `HttpTimeoutException` are already
   *  half the diagnosis. Bounded, because it rides in an exception message that
   *  uptime truncates. */
  def describe(cause: Throwable): String = {
    val message = Option(cause.getMessage).getOrElse("")
    val line    = if (message.isEmpty) cause.getClass.getSimpleName else s"${cause.getClass.getSimpleName}: $message"
    if (line.length <= 120) line else line.take(117) + "..."
  }
}
