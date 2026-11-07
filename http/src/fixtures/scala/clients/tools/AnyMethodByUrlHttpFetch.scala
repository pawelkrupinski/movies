package clients.tools

import tools.HttpFetch

/** An `HttpFetch` that answers every request — GET, header-bearing GET, raw bytes, POST — with
 *  `respond(url)`: return a body, or throw to model the upstream failing for that URL. The
 *  GET-only [[ScriptedByUrlHttpFetch]]'s twin for a subject that also POSTs (IMDb's GraphQL, a
 *  wiring whose leaf every client shares), where a refused POST would fail the spec for the
 *  wrong reason. Count or record inside `respond` to observe the traffic. */
class AnyMethodByUrlHttpFetch(respond: String => String) extends HttpFetch {
  override def get(url: String): String = respond(url)
  override def post(url: String, body: String, contentType: String): String = respond(url)
}
