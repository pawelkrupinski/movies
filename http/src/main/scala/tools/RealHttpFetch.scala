package tools

import java.net.URI
import java.net.http.HttpRequest
import java.time.Duration
import java.util.concurrent.CompletableFuture

import scala.annotation.unused

import play.api.Logging

class RealHttpFetch(
  proxy: Option[ProxyConfig] = None,
  // The TLS context every client below handshakes with (see TlsTrust). A wiring builds
  // one and hands it to each fetch it composes, so they share its session cache; a
  // fetch built without one gets its own.
  tls: javax.net.ssl.SSLContext = TlsTrust.newContext(),
  // The host's request budget — HostPolicies' table; a spec passes a short one.
  requestTimeoutFor: String => Duration = HostPolicies.requestTimeoutFor,
  // The host's connect budget (TCP and TLS handshake) — HostPolicies' table. A spec whose handshake is the thing under
  // test, in a cold JVM on a loaded machine, passes its own (IssuerCertificateFetchingSpec): the 5 s a live host needs
  // is no bound on a starved runner's class loading and path building.
  connectTimeoutFor: String => Duration = HostPolicies.connectTimeoutFor,
  // Which of a host's addresses the JVM connects to (AddressFailover): the JVM's resolver consults this one instance,
  // so a connect failure reported here moves the next connect onto the host's next address.
  addresses: AddressFailover = FailoverAddressResolverProvider.failover
) extends HttpFetch with Logging {
  /** The clients this fetch sends on and how one exchange goes out: redirects, address failover, its deadline
   *  ([[HttpTransport]]), logged under this class's logger. */
  private val transport = new HttpTransport(proxy, tls, requestTimeoutFor, connectTimeoutFor, addresses, logger)

  /** The client `url`'s host policy picks ([[HttpTransport.clientFor]]). Package-private so the spec can assert the
   *  routing. */
  private[tools] def clientFor(url: String): java.net.http.HttpClient = transport.clientFor(url)

  /** What an exchange's reply or failure becomes -- a body, a typed status failure -- and the line it is logged as,
   *  under this class's logger. */
  private[tools] val replies = new HttpReplies(logger, requestTimeoutFor, connectTimeoutFor)

  override def get(url: String): String = get(url, Map.empty)

  /** Raw wire bytes (gunzipped, NOT charset-decoded) so a caller can pick the
   *  right charset itself. See [[HttpFetch.getBytes]] — only the charset-quirky
   *  scrapers (Kino Charlie) use this; everyone else gets the UTF-8 `get`. */
  override def getBytes(url: String): Array[Byte] = {
    replies.bytes(url, transport.send(buildRequest(url, Map.empty)))
  }

  override def get(url: String, headers: Map[String, String]): String =
    replies.body("GET", url, replies.sentLogged("GET", url, transport.send(buildRequest(url, headers))))

  /** GET, also reporting the URL the redirects ended on — for a caller that
   *  must know whether the address it holds is the one the upstream serves (the
   *  roster audit's stale-slug check). Same status handling as `get`. */
  def getPage(url: String): FetchedPage = {
    val response = replies.sentLogged("GET", url, transport.send(buildRequest(url)))
    FetchedPage(response.uri().toString, replies.body("GET", url, response))
  }

  override def getAsync(url: String): CompletableFuture[String] =
    transport.sendAsync(buildRequest(url))
      .handle[String] { (response, throwable) =>
        if (throwable != null) {
          replies.failed("GET", url, HttpTransport.unwrap(throwable))
          throw throwable
        }
        replies.body("GET", url, response)
      }

  override def post(url: String, body: String, contentType: String = "application/json"): String =
    replies.body("POST", url, replies.sentLogged("POST", url, transport.send(postRequest(url, body, contentType))))

  /** Package-private so the spec can assert what actually goes on the wire — the
   *  header a host policy demands is only observable on the built request. */
  private[tools] def postRequest(url: String, body: String, contentType: String): HttpRequest = {
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(requestTimeoutFor(url))
      .header("Content-Type", contentType)
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      // Decoded transparently ([[HttpReplies]]). See the matching
      // header on `buildRequest` for the GET path.
      .header("Accept-Encoding", "gzip")
      .POST(HttpRequest.BodyPublishers.ofString(body))
    // `setHeader` (overwrite), not `header` (append) — same reason as `buildRequest`.
    HostPolicies.headersFor(url).foreach { case (k, v) => builder.setHeader(k, v) }
    withProxyCredentials(builder).build()
  }

  private[tools] def buildRequest(url: String, extraHeaders: Map[String, String] = Map.empty): HttpRequest = {
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(requestTimeoutFor(url))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      // Advertise gzip — [[HttpReplies]] decompresses transparently. Without
      // this header some CDNs still gzip based on the Chrome-shaped UA we
      // send, so the decode path runs regardless; advertising it makes the
      // negotiation explicit and shrinks the wire payload.
      .header("Accept-Encoding", "gzip")
      .GET()
    // `setHeader`, NOT `header`: the latter APPENDS, so a caller overriding one of
    // the defaults above (most importantly `User-Agent`) got BOTH values sent —
    // our identity plus a Chrome string, on the very requests where the override
    // exists because the upstream cares what we claim to be — Wikimedia's UA
    // policy ("use your app and version") is exactly that case.
    // Host-policy headers first, caller's second: an explicit per-call override
    // is more specific than a blanket rule about the host, so it wins.
    HostPolicies.headersFor(url).foreach { case (k, v) => builder.setHeader(k, v) }
    extraHeaders.foreach { case (k, v) => builder.setHeader(k, v) }

    withProxyCredentials(decorateBuilder(builder, url)).build()
  }

  /** The proxy's Basic credentials, sent up front. java.net.http copies a request's
   *  proxy-* headers onto the HTTPS `CONNECT` (subject to the process's
   *  [[ProxyTunnelAuthentication]]) and strips them from what goes to the origin. */
  private def withProxyCredentials(builder: HttpRequest.Builder): HttpRequest.Builder =
    proxy.fold(builder)(p => builder.setHeader("Proxy-Authorization", p.basicCredentials))

  protected def decorateBuilder(builder: HttpRequest.Builder, @unused url: String): HttpRequest.Builder = builder
}

/** A page body and the URL it was finally served from, after redirects. */
final case class FetchedPage(finalUrl: String, body: String)
