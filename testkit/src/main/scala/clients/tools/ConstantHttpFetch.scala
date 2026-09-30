package clients.tools

import tools.HttpFetch

/** An `HttpFetch` that answers every GET and POST with the same `body`, whatever the URL.
 *  For a test whose subject is the code AROUND the fetch — a fallback chain, a cache, a
 *  retry wrapper — where the upstream only has to answer. Wrap it in
 *  [[RequestLogHttpFetch]] to also see what was asked of it. */
class ConstantHttpFetch(body: String) extends HttpFetch {
  override def get(url: String): String = body
  override def post(url: String, requestBody: String, contentType: String): String = body
}
