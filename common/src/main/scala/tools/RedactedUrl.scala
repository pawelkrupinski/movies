package tools

/**
 * Renders a URL safe to LOG by masking the value of any query parameter that
 * carries a credential.
 *
 * Three of our upstreams authenticate in the query string rather than a header —
 * TMDB (`api_key=`), OMDb (`apikey=`) and the Firestore endpoint Kino Aurum
 * reads (`key=`) — so every place that writes a URL into a log line or an
 * exception message publishes the key with it. That is not theoretical: a dead
 * TMDB id (`/movie/1715017/external_ids`) reschedules forever, and each attempt
 * wrote the full v3 key into `/data/logs/worker.log`, which is kept on the Fly
 * volume for 14 days and read out with `kubectl logs` during any incident.
 *
 * Only the VALUE is replaced, never the parameter name or the URL's shape:
 * callers match on the message (`MonitoringHttpFetch`'s `HTTP 5\d\d .*`
 * classifier) and a redaction that moved things around would break them. The
 * raw URL survives untouched on [[HttpStatusException.url]] for code that needs
 * to re-issue or inspect it — this is a rendering concern, not a storage one.
 */
object RedactedUrl {

  /** Query parameters whose value is a credential. Matched case-insensitively.
   *  `key` is deliberately included even though it is a generic name: the one
   *  parameter we actually spell that way IS a Google API key, and over-masking
   *  a benign field in a log line costs nothing next to leaking a real one. */
  private val SecretParameters: Set[String] =
    Set("api_key", "apikey", "key", "token", "access_token", "auth", "password", "secret", "signature", "sig")

  val Mask = "***"

  /** The URL with every secret parameter's value replaced by [[Mask]]. A URL with
   *  no query string is returned unchanged. */
  def apply(url: String): String = url.indexOf('?') match {
    case -1 => url
    case at =>
      val (base, query) = (url.substring(0, at), url.substring(at + 1))
      // Split on the fragment first so a `#…` tail isn't swept into the last
      // parameter's value (and re-attached after, unredacted — it carries no
      // credentials and is never sent to the server).
      val (parameters, fragment) = query.indexOf('#') match {
        case -1 => (query, "")
        case hash => (query.substring(0, hash), query.substring(hash))
      }
      val masked = parameters.split("&", -1).map(redactParameter).mkString("&")
      s"$base?$masked$fragment"
  }

  /** `name=secret` → `name=***` for a credential-bearing name; anything else
   *  (including a bare valueless flag) is left exactly as it was. */
  private def redactParameter(parameter: String): String = parameter.indexOf('=') match {
    case -1 => parameter
    case at =>
      val name = parameter.substring(0, at)
      if (SecretParameters.contains(name.toLowerCase)) s"$name=$Mask" else parameter
  }
}
