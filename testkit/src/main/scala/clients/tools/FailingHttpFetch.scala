package clients.tools

import tools.{HttpFetch, HttpStatusException}

import java.net.ConnectException
import java.net.http.HttpTimeoutException

/** An [[tools.HttpFetch]] whose every request fails with `fault(method, url)`. Used to
 *  assert that a caller PROPAGATES a fetch failure (so it surfaces red on the uptime
 *  page) instead of swallowing it into an empty list (which reads as a successful
 *  "0 showtimes" scrape — white, indistinguishable from a genuinely film-dormant venue).
 *
 *  The single-status ctor keeps the original shape: an [[HttpStatusException]], the same
 *  typed exception `RealHttpFetch` throws on a non-2xx response, defaulting to 503 (the
 *  shared-hosting overload code that motivated the guard). The companion's
 *  [[FailingHttpFetch.Faults]] adds the two transport failures `RealHttpFetch` lets
 *  through unchanged — a request timeout and a refused connection.
 *
 *  POST fails the same way GET does: a client whose listing call is a POST (AMC's
 *  GraphQL day query) needs the identical guard, and a fake that answered POST
 *  with a DIFFERENT exception type would let such a client's failure path pass
 *  its test while behaving differently in production. */
class FailingHttpFetch(fault: (String, String) => Throwable) extends HttpFetch {

  def this(status: Int) = this((method, url) => new HttpStatusException(status, method, url, retryAfter = None))
  def this() = this(503)

  override def get(url: String): String = throw fault("GET", url)

  override def post(url: String, body: String, contentType: String): String = throw fault("POST", url)
}

object FailingHttpFetch {
  /** A named way for an upstream to be down, for a spec parametrised over them. */
  final case class Fault(name: String, fetch: HttpFetch)

  /** The three shapes a wholly-down upstream takes on the wire: it answers 500, it
   *  never answers (the JDK client's request timeout), or nothing listens (connection
   *  refused). Each is an exception `RealHttpFetch` propagates to the caller. */
  val Faults: Seq[Fault] = Seq(
    Fault("HTTP 500", new FailingHttpFetch(500)),
    Fault("a request timeout", new FailingHttpFetch((method, url) => new HttpTimeoutException(s"$method $url: request timed out"))),
    Fault("a refused connection", new FailingHttpFetch((method, url) => new ConnectException(s"$method $url: Connection refused")))
  )
}
