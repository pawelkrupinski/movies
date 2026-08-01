package tools

/**
 * What a real site does for a URL that has no page: **404**.
 *
 * Client fakes used to signal "I don't serve that URL" with a bare
 * `RuntimeException("unexpected URL: …")`. While the clients swallowed every
 * failure into `None` that was indistinguishable from a 404, so the slug-probe
 * ladders behaved the same either way and the imprecision cost nothing.
 *
 * It costs something now. The clients draw a line between an upstream that
 * ANSWERED "no such page" (data — keep probing the next candidate) and a read
 * that FAILED (not data — propagate, see [[EnrichmentRead]]). A fake that
 * reports a nonexistent slug as a generic error is claiming the site is broken,
 * which aborts the ladder. Fakes have to speak the same failure vocabulary as
 * the real thing or they test a different system.
 *
 * Use this for "this URL genuinely doesn't exist upstream". Keep a plain
 * exception for "the test wired something wrong and this call should never have
 * happened" — those are different claims and should stay distinguishable.
 */
object UpstreamNotFound {

  /** The 404 a real site returns for `url`. */
  def apply(url: String): Nothing =
    throw new HttpStatusException(404, "GET", url, None)
}
