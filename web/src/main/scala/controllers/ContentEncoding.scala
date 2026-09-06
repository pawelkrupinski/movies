package controllers

/**
 * A compressed representation this server can build for a response body, and the
 * `Accept-Encoding` negotiation that picks between them.
 *
 * WHY THIS EXISTS AT ALL: Cloudflare used to do the choosing. It recompressed our
 * gzip to brotli at the edge and handed clients `content-encoding: br` — which is
 * also why it deleted our `ETag`, because a body it rewrote is not a body our
 * validator describes. `Cache-Control: no-transform` stopped the rewriting and got
 * the validator through, and the cost of that was the brotli: full fetches went
 * from 228,940 bytes to 287,531 on `/uk/manchester/`, +26%. Compressing here gets
 * it back in principle — the bytes we stamp would be the bytes we send, and at
 * 197,131 they beat the 228,940 the edge produced.
 *
 * ⚠️ IN PRACTICE THE ORIGIN OFFERS GZIP ONLY, and this type exists to make that a
 * decision rather than an assumption. Cloudflare caches ONE variant per URL and
 * cannot key it on the encoding, so offering two meant it stored br and served
 * gzip-only clients the decompressed body. `MovieController.ServableEncodings` is
 * where that is decided and where the numbers are.
 *
 * ⚠️ ONLY THE CACHED RESPONSES REACH THIS. Everything else leaves the controller
 * uncompressed and Play's `GzipFilter` compresses it on the way out — the filter
 * skips any response that already carries a `Content-Encoding`, which is exactly
 * how the two arrangements stay out of each other's way.
 */
enum ContentEncoding(val token: String) {

  /** 34% smaller than gzip on this HTML, and cheaper to produce — see
   *  `EncodedResponseCache.BrotliQuality` for the measurements. Every current
   *  browser asks for it, and we do not currently answer: see
   *  `MovieController.ServableEncodings`. Built and tested, waiting on an edge that
   *  can cache per encoding. */
  case Brotli extends ContentEncoding("br")

  /** The floor. Anything that talks HTTP/1.1 to us takes it. */
  case Gzip extends ContentEncoding("gzip")
}

object ContentEncoding {

  /** `token` / `token;q=0.5`, tolerant of the spacing real clients send. */
  private val Coding = """\s*([A-Za-z0-9*_.+-]+)\s*(?:;\s*[Qq]\s*=\s*([0-9.]+))?\s*""".r

  /** The best encoding this client will accept, or `None` to send it uncompressed.
   *
   *  Q-VALUES ARE HONOURED RATHER THAN ASSUMED. The check this replaces was
   *  `acceptEncoding.contains("gzip")`, which reads `gzip;q=0` — an explicit
   *  REFUSAL — as acceptance, and cannot express a client that would rather have
   *  gzip than brotli. Both are real: `q=0` is the only way to say "not this one",
   *  and a proxy that can only inflate gzip says so by weighting.
   *
   *  Where the client expresses no preference between them we take brotli, because
   *  it is smaller and it is what the edge was already handing these clients before
   *  `no-transform` stopped it.
   *
   *  A `*` covers anything not named, so `Accept-Encoding: *` means "whatever you
   *  have" — but a named `q=0` still wins over it, which is what lets a client take
   *  everything EXCEPT brotli. */
  def negotiate(acceptEncoding: Option[String],
                willing: Set[ContentEncoding] = values.toSet): Option[ContentEncoding] = {
    val offered: Map[String, Double] =
      acceptEncoding.toList
        .flatMap(_.split(','))
        .collect { case Coding(token, q) =>
          token.toLowerCase -> Option(q).flatMap(_.toDoubleOption).getOrElse(1.0)
        }
        .toMap

    def weightOf(encoding: ContentEncoding): Double =
      offered.getOrElse(encoding.token, offered.getOrElse("*", 0.0))

    // Heaviest weight wins; a tie falls to declaration order, which is brotli
    // first. Spelled out rather than leaning on how `maxBy` breaks ties.
    values.filter(willing).map(e => e -> weightOf(e)).filter(_._2 > 0) match {
      case Array()   => None
      case supported => Some(supported.minBy { case (e, q) => (-q, e.ordinal) }._1)
    }
  }
}
