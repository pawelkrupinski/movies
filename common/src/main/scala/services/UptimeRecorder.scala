package services

/** The write side of /uptime: what a process that only RECORDS outcomes needs — the scrapers,
 *  the monitored HTTP chain, the venue-page readers, the image-load beacon. Each call mutates the
 *  current 15-minute bucket of `service`; persisting it is the implementation's business
 *  ([[UptimeMonitor]] batches it into one Mongo write per flush). */
trait UptimeRecorder {

  /** A successful call that was not timed (a browser image-load event). */
  def recordSuccess(service: String): Unit

  /** Record a successful call along with how long it took, so the uptime page
   *  can show per-service average latency (1h + total). For a cinema scrape,
   *  `thin` says the listing held screenings but none in the near term (see
   *  `NearTermProgramme` in the worker): still a success, so the bar stays green,
   *  but /uptime marks it — a feed that stopped carrying a venue's programme can
   *  keep a stray far-off slot and never go white. */
  def recordSuccess(service: String, durationMs: Long, thin: Boolean = false): Unit

  /** Record a success that was served via the Filmweb fallback — the primary
   *  scrape failed or came back empty, so the showtimes came from Filmweb. Counts
   *  as a success (the user got data) but also marks the bucket `fallback`, so the
   *  /uptime bar can show "served via Filmweb" rather than a plain green. `thin`
   *  as for [[recordSuccess]], judged on what the fallback served. */
  def recordFallbackSuccess(service: String, durationMs: Long, thin: Boolean = false): Unit

  /** Record a call that completed without error but came back empty-handed —
   *  for a cinema scrape, the page loaded and parsed yet yielded zero
   *  screenings. That's neither a success (we got no data) nor a failure (the
   *  upstream didn't error), so it gets its own dimension and surfaces as a
   *  white "no screenings" bar. The round-trip was real, so its latency still
   *  counts toward the average. */
  def recordEmpty(service: String, durationMs: Long): Unit

  def recordFailure(service: String, error: String): Unit

  /** Attach `tags` to `service`, replacing any existing set; returns whether a
   *  Mongo write was made. The rules — and why a no-op set must NOT write — are
   *  on [[ServiceTags.tagService]]. */
  def tagService(service: String, tags: Set[String]): Boolean
}
