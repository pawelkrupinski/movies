package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.util.concurrent.TimeUnit

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
  private val cache: Cache[String, Array[Byte]] =
    Caffeine.newBuilder().maximumSize(1000).expireAfterWrite(12, TimeUnit.HOURS).build()

  /** `posterUrls` is the ordered candidate list — the primary poster followed by
   *  the cinema fallbacks — mirroring the browser's `<img onerror>` fallback
   *  chain. The card walks it until one URL decodes. This matters because a
   *  film's *primary* poster is often a Multikino origin whose Cloudflare 403s
   *  our Fly datacenter IP (and weserv SkipHosts Multikino, so the proxy can't
   *  rescue it) — without the fallbacks ~a third of films rendered text-only. */
  def card(title: String, subtitle: String, badges: Seq[OgCardRenderer.Badge], posterUrls: Seq[String],
           director: Option[String] = None, synopsis: Option[String] = None): Array[Byte] = {
    val candidates = posterUrls.filter(_.nonEmpty).take(OgCardService.MaxPosterCandidates)
    // Plain concatenation, not an s-interpolator: a nested double-quote (from
    // mkString) inside an interpolation block would close the string early.
    val ratingKey = badges.flatMap(_.segs.map(_.text)).mkString(",")
    val key = Seq(title, subtitle, ratingKey, candidates.mkString("|"),
                  director.getOrElse(""), synopsis.getOrElse("")).mkString(" ")
    Option(cache.getIfPresent(key)).getOrElse {
      val poster = loader.loadFirst(candidates)
      val bytes  = OgCardRenderer.render(title, subtitle, badges, poster, director, synopsis)
      // Only cache a *complete* card: one with no poster to show, or whose
      // poster actually loaded. A transient poster-fetch failure must NOT be
      // frozen for the cache's lifetime as a posterless card -- leave it
      // uncached so the next share retries (origin/weserv will likely be warm
      // by then). This was the bug behind text-only cards on first share.
      if (candidates.isEmpty || poster.isDefined) cache.put(key, bytes)
      bytes
    }
  }
}

object OgCardService {
  /** Bound on how many candidate poster URLs the card fetches before giving up
   *  on a text-only render. The fallback list can hold 25+ URLs; the first
   *  reachable origin almost always wins, but the cap keeps a pathological
   *  all-dead chain from stacking dozens of slow connect timeouts on one
   *  request. */
  private val MaxPosterCandidates = 6
}
