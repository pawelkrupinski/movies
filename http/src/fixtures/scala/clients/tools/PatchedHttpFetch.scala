package clients.tools

import tools.HttpFetch

/** A `delegate` (usually a [[FakeHttpFetch]] replay) with some GETs answered otherwise: a URL
 *  `patch` is defined at gets `patch(url)` — a doctored page, or a throw modelling that one
 *  request failing — and every other request, POSTs included, goes to the delegate unchanged.
 *  For "the recorded venue, but this one day is down". */
class PatchedHttpFetch(delegate: HttpFetch)(patch: PartialFunction[String, String]) extends HttpFetch {
  override def get(url: String): String = patch.applyOrElse(url, delegate.get(_))
  override def get(url: String, headers: Map[String, String]): String = patch.applyOrElse(url, delegate.get(_, headers))
  override def getBytes(url: String): Array[Byte] =
    if (patch.isDefinedAt(url)) patch(url).getBytes(java.nio.charset.StandardCharsets.UTF_8) else delegate.getBytes(url)
  override def post(url: String, body: String, contentType: String): String = delegate.post(url, body, contentType)
}
