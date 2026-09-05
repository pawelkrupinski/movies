package tools

/**
 * Orchestrates the film Open Graph card: fetch + decode the poster, hand it to
 * the pure [[OgCardRenderer]], and memoise the resulting PNG so the repeated
 * shares + Facebook's aggressive re-scrapes of the same film don't re-fetch
 * the poster and re-rasterise the card every time.
 *
 * The cache key is the full set of card inputs (title + subtitle + ratings +
 * candidate poster URLs). When the worker refreshes a rating or a cinema swaps
 * the poster, the key diverges and the stale card simply falls out of use; the
 * TTL + max-size only bound growth for inputs that never change.
 *
 * `posters` is injected at the composition root, so tests drive a fake
 * returning fixture bytes and never touch the network.
 */
class OgCardService(posters: PosterFetch) {
  private val loader = new PosterImageLoader(posters)
  private val cache  = new OgCardCache(OgCard.MaxCacheBytes)

  /** This service's card cache, for `kinowo_web_cache_*` — the film cards' own
   *  budget, held separately from the other card service's. */
  def cacheOccupancy: services.metrics.CacheOccupancy = cache.occupancy

  /** `posterUrls` is the ordered candidate list — the primary poster followed by
   *  the cinema fallbacks — mirroring the browser's `<img onerror>` fallback
   *  chain. The card walks it until one URL decodes. This matters because a
   *  film's *primary* poster is often a Multikino origin whose Cloudflare 403s
   *  our datacentre egress IP — Fly's before the 2026-08-29 move, Hetzner's
   *  since; the block is about datacentre ranges, not one provider (and weserv
   *  SkipHosts Multikino, so the proxy can't rescue it) — without the fallbacks
   *  ~a third of films rendered text-only. */
  def card(title: String, subtitle: String, badges: Seq[OgCardRenderer.Badge], posterUrls: Seq[String],
           host: String, director: Option[String] = None, synopsis: Option[String] = None): Array[Byte] = {
    val candidates = posterUrls.filter(_.nonEmpty).take(OgCard.MaxPosterCandidates)
    // Plain concatenation, not an s-interpolator: a nested double-quote (from
    // mkString) inside an interpolation block would close the string early.
    val ratingKey = badges.flatMap(_.segs.map(_.text)).mkString(",")
    // `host` is in the key even though one deployment only ever passes one value: it is drawn
    // into the image, so two hosts must not share a cached card.
    val key = Seq(title, subtitle, ratingKey, candidates.mkString("|"), host,
                  director.getOrElse(""), synopsis.getOrElse("")).mkString(" ")
    cache.getOrRender(key) {
      val poster = loader.loadFirst(candidates)
      val bytes  = OgCardRenderer.render(title, subtitle, badges, poster, host, director, synopsis)
      // Only cache a *complete* card: one with no poster to show, or whose
      // poster actually loaded. A transient poster-fetch failure must NOT be
      // frozen as a posterless card -- leave it uncached so the next share
      // retries. This was the bug behind text-only cards on first share.
      (bytes, candidates.isEmpty || poster.isDefined)
    }
  }
}
