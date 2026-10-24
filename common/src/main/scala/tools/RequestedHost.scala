package tools

/**
 * The host a piece of work last sent a request to, for a caller whose failure does not name it: an
 * `HttpTimeoutException`, a refused connection or another `IOException` carries no URL, and the identity questions'
 * memory releases a failed ask by the host it failed against (`HostRecovery`). The caller opens a [[Recording]] around
 * its work ([[within]]); the host breaker, in every fetch chain, notes each request's host on the thread that makes it
 * ([[note]]) — a cheap no-op where no recording is open.
 *
 * On the calling thread only: a request handed to another thread is not seen, and of several requests the last one
 * made is what the recording holds — for a synchronous ask, the one that failed.
 */
object RequestedHost {
  /** The host most recently requested within it, if any. */
  final class Recording {
    @volatile private var noted: Option[String] = None
    def last: Option[String] = noted
    private[RequestedHost] def note(host: String): Unit = noted = Some(host)
  }

  private val open = new ThreadLocal[Recording]

  /** `body` run with `recording` noting each host this thread requests, until it returns or throws. */
  def within[A](recording: Recording)(body: => A): A = {
    val outer = open.get
    open.set(recording)
    try body
    finally open.set(outer)
  }

  /** A request to `host` is going out on this thread. */
  def note(host: String): Unit = { val recording = open.get; if (recording != null) recording.note(host) }
}
