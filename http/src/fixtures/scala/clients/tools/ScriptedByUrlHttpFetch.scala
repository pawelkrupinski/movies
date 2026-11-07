package clients.tools

import tools.GetOnlyHttpFetch

/** A GET-only `HttpFetch` that answers each request with `respond(url)` — return a body,
 *  or throw to model the upstream failing for that URL. For hand-built pages where the
 *  answer depends on the URL in a way a fixed fragment table (`UrlFragmentHttpFetch`)
 *  can't express: a date read out of the query, "every day but this one fails". */
class ScriptedByUrlHttpFetch(respond: String => String) extends GetOnlyHttpFetch {
  override def get(url: String): String = respond(url)
}
