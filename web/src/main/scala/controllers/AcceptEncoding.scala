package controllers

/**
 * Whether a client's `Accept-Encoding` lets us answer gzip — the one compressed
 * form the origin puts on the wire (see `MovieController.conditionalCompressed`
 * for why it is the only one).
 *
 * ⚠️ ONLY THE CACHED RESPONSES ASK THIS. Everything else leaves the controller
 * uncompressed and Play's `GzipFilter` compresses it on the way out — the filter
 * skips any response that already carries a `Content-Encoding`, which is exactly
 * how the two arrangements stay out of each other's way.
 */
object AcceptEncoding {

  /** `token` / `token;q=0.5`, tolerant of the spacing real clients send. */
  private val Coding = """\s*([A-Za-z0-9*_.+-]+)\s*(?:;\s*[Qq]\s*=\s*([0-9.]+))?\s*""".r

  /** Does this client take gzip? `None` (no header) is "no".
   *
   *  Q-VALUES ARE HONOURED RATHER THAN ASSUMED. The check this replaced was
   *  `acceptEncoding.contains("gzip")`, which reads `gzip;q=0` — an explicit
   *  REFUSAL — as acceptance. `q=0` is the only way a client can say "not this
   *  one", and a proxy that can only inflate identity says so exactly that way.
   *
   *  A `*` covers anything not named, so `Accept-Encoding: *` means "whatever you
   *  have" — but a named `gzip;q=0` still wins over it. */
  def acceptsGzip(acceptEncoding: Option[String]): Boolean = {
    val offered: Map[String, Double] =
      acceptEncoding.toList
        .flatMap(_.split(','))
        .collect { case Coding(token, q) =>
          token.toLowerCase -> Option(q).flatMap(_.toDoubleOption).getOrElse(1.0)
        }
        .toMap
    offered.getOrElse("gzip", offered.getOrElse("*", 0.0)) > 0
  }
}
