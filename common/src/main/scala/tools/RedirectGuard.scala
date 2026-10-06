package tools

import java.io.IOException
import java.net.{InetAddress, URI}
import java.net.http.{HttpRequest, HttpResponse}
import scala.util.Try

/**
 * How [[RealHttpFetch]] follows a redirect: the JDK's `Redirect.NORMAL` rules, plus one
 * the JDK has no hook for — a redirect may not move a request onto a local address.
 *
 * Why it follows redirects itself: the JDK client follows a `Location` wherever it
 * points, and an upstream that answers `301 Location: http://127.0.0.1/…` (sfr.pl,
 * reported for Kino Kreska's listing POST on 2026-10-06 — a misconfigured origin or
 * CDN) would have the worker send its request to its own loopback, or to a private
 * fleet address. Such a hop is refused with a [[RefusedRedirectException]] naming the
 * status and the target, so the scrape fails red with the reason on /uptime instead
 * of reading whatever answers locally.
 *
 * The NORMAL rules kept: 301/302/303/307/308 with a `Location` (resolved against the
 * request) are followed; a 303, or a 301/302 answering a POST, continues as a GET
 * without the body; an https → http downgrade is not followed (the 3xx is returned,
 * and the caller's status check fails it); at most [[MaxHops]] hops.
 */
object RedirectGuard {

  val MaxHops = 5

  private val RedirectStatuses = Set(301, 302, 303, 307, 308)

  /** The request to send next, or `None` when `response` is the answer. Throws
   *  [[RefusedRedirectException]] for a hop onto a local address. */
  def next(request: HttpRequest, response: HttpResponse[?], hops: Int): Option[HttpRequest] = {
    val code     = response.statusCode()
    val location = Option(response.headers().firstValue("Location").orElse(null)).map(_.trim).filter(_.nonEmpty)
    location.filter(_ => RedirectStatuses(code)).flatMap { raw =>
      val from = request.uri()
      val to   = from.resolve(raw)
      refusal(from, to).foreach(why => throw new RefusedRedirectException(request.method(), from, code, to, why))
      if (from.getScheme.equalsIgnoreCase("https") && to.getScheme.equalsIgnoreCase("http")) None
      else if (hops >= MaxHops) throw new RefusedRedirectException(request.method(), from, code, to, s"more than $MaxHops redirects")
      else Some(redirected(request, to, asGet = code == 303 || ((code == 301 || code == 302) && request.method() == "POST")))
    }
  }

  /** Why the hop `from` → `to` is refused: `to` is a local address (loopback, wildcard,
   *  link-local, private, or a `localhost` name) that `from` was not already on. A host
   *  name is never resolved — only a literal address or a localhost name is judged. */
  def refusal(from: URI, to: URI): Option[String] =
    Option(to.getHost).map(_.toLowerCase(java.util.Locale.ROOT).stripPrefix("[").stripSuffix("]"))
      .filter(host => isLocal(host) && !Option(from.getHost).exists(_.equalsIgnoreCase(to.getHost)))
      .map(host => s"$host is a local address")

  private def isLocal(host: String): Boolean =
    host == "localhost" || host.endsWith(".localhost") || literalAddress(host).exists(address =>
      address.isLoopbackAddress || address.isAnyLocalAddress || address.isLinkLocalAddress || address.isSiteLocalAddress)

  /** The address an IP literal names; `None` for a host name, which is left unresolved. */
  private def literalAddress(host: String): Option[InetAddress] =
    Option.when(host.contains(':') || host.matches("""\d{1,3}(\.\d{1,3}){3}"""))(host)
      .flatMap(literal => Try(InetAddress.getByName(literal)).toOption)

  private def redirected(request: HttpRequest, to: URI, asGet: Boolean): HttpRequest = {
    val builder = HttpRequest.newBuilder(request, (name, _) => !(asGet && name.equalsIgnoreCase("Content-Type"))).uri(to)
    (if (asGet) builder.GET() else builder).build()
  }
}

/** A redirect [[RedirectGuard]] would not follow — a transport failure, never an answer. */
class RefusedRedirectException(method: String, from: URI, code: Int, to: URI, why: String)
  extends IOException(s"HTTP $code for $method ${RedactedUrl(from.toString)} redirected to ${RedactedUrl(to.toString)}, refused: $why")
