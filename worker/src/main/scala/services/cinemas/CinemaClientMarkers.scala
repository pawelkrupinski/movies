package services.cinemas

import services.UptimeMonitor

/**
 * Derives, for every cinema, a marker describing the scraper client behind it:
 * whether that client is REUSED across several cinemas (a shared platform client
 * like `FilmwebShowtimesClient` / `MultikinoClient`) or is BESPOKE to the one
 * cinema. The catalog is the single source of truth — reusability is just "does
 * more than one cinema use this client class", counted over the live scraper set,
 * so adding/removing a cinema reclassifies automatically with no hand-kept list.
 *
 * The marker is published as a generic `UptimeMonitor` tag (per cinema row) so
 * the /uptime page — which lives in the `web` module and can't see this
 * worker-only catalog — can render it. Tag form is `"<kind>:<ClientClass>"`,
 * e.g. `"shared:FilmwebShowtimesClient"` or `"custom:RialtoClient"`; the page
 * splits on the first `:` into a styled kind + label.
 */
object CinemaClientMarkers {
  val SharedKind   = "shared"
  val CustomKind   = "custom"
  val FallbackKind = "fallback"
  /** Tag flagging a cinema currently served via the Filmweb fallback (its own
   *  scraper is down). Rendered as an "FtFW" (fell-to-Filmweb) chip on /uptime.
   *  The worker writes it on each fallback ENTER and clears it on RECOVER (and
   *  reconciles already-active ones at boot); it has no client label, so it stays
   *  a bare `"fallback:FtFW"`. */
  val FilmwebFallbackTag = s"$FallbackKind:FtFW"

  /** The full uptime-tag set for one cinema: its scraper-client marker (if any),
   *  its public source-page link (if any), plus the FtFW tag while it's actively
   *  in Filmweb fallback. Kept here, beside the marker format they share, so both
   *  the boot reconcile and the per-event retag in `WorkerWiring` compute the
   *  same set. */
  def tagsFor(clientMarker: Option[String], sourceUrl: Option[String], inFallback: Boolean): Set[String] =
    clientMarker.toSet ++
      // The venue's public source page (`url:<https…>`); the /uptime + /debug
      // pages turn the cinema name into a link to it. Not a chip — the page
      // reads it for the `href` and skips it in the chip row. Prefix shared
      // with the reader via `UptimeMonitor.UrlTagPrefix`.
      sourceUrl.map(u => UptimeMonitor.UrlTagPrefix + u) ++
      (if (inFallback) Set(FilmwebFallbackTag) else Set.empty[String])

  /** The raw scraper's client class as the marker label. For the multi-venue
   *  chains this is the per-venue adapter (`CinemaCityScraper`); for everything
   *  else it's the client itself (`FilmwebShowtimesClient`, `RialtoClient`, …). */
  private def clientOf(scraper: CinemaScraper): String = scraper.getClass.getSimpleName

  /** `cinema displayName -> "<kind>:<ClientClass>"`, derived from the raw
   *  (unwrapped) scrapers. Reusability is counted over `scrapers`, so pass the
   *  full catalog set. A cinema with more than one scraper keeps the marker of
   *  its first (catalog order). */
  def markers(scrapers: Seq[CinemaScraper]): Map[String, String] = {
    val counts = scrapers.groupBy(clientOf).view.mapValues(_.size).toMap
    scrapers.foldLeft(Map.empty[String, String]) { (acc, s) =>
      val name = s.cinema.displayName
      if (acc.contains(name)) acc
      else {
        val client = clientOf(s)
        val kind   = if (counts.getOrElse(client, 0) > 1) SharedKind else CustomKind
        acc + (name -> s"$kind:$client")
      }
    }
  }

  /** `cinema displayName -> public source-page URL`, for the cinemas whose
   *  scraper reports a `sourceUrl`. A cinema with more than one scraper keeps
   *  the first's URL (catalog order), mirroring `markers`. Cinemas whose scraper
   *  has no public page are simply absent — the page renders their name plain. */
  def sourceUrls(scrapers: Seq[CinemaScraper]): Map[String, String] =
    scrapers.foldLeft(Map.empty[String, String]) { (acc, s) =>
      val name = s.cinema.displayName
      if (acc.contains(name)) acc
      else s.sourceUrl.fold(acc)(url => acc + (name -> url))
    }
}
