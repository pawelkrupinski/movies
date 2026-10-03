package tools

import play.api.libs.json.{JsValue, Json}

/**
 * JSON response bodies, each parsed once where two readers on one thread parse the same body: the
 * identity store's normalizing fetch parses a TMDB response to file it (`NormalizingHttpFetch`), then
 * hands the very same `String` to the client that asked, which parsed it again. A film record's two
 * responses carry its whole cast and crew, and parsing each twice was a quarter of a US take-up's CPU
 * (JFR). The last body a thread parsed is handed back to the next parse of that same instance — by
 * identity, never by content — once; the readers share it by sharing this instance, which the
 * composition root wires into both.
 *
 * Open for one subclass, the archive replay's `SharedJsonBodies`, which parses each body once for
 * every pass of an order-independence replay; production wires this class as it is.
 */
open class JsonBodies {
  private val last = new ThreadLocal[(String, JsValue)]

  def parse(body: String): JsValue = {
    val held = last.get
    if (held != null && (held._1 eq body)) { last.remove(); held._2 }
    else {
      val parsed = Json.parse(body)
      last.set(body -> parsed)
      parsed
    }
  }
}
