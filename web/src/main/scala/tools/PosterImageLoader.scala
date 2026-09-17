package tools

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import javax.imageio.ImageIO

/**
 * Fetch + decode a poster URL to a `BufferedImage`, with the origin-first,
 * weserv-proxy-fallback chain both OG-card services need.
 *
 * Each URL is fetched at its origin directly first ([[PosterFetch]] gives it the
 * generous connect budget slow cinema origins need); the weserv JPEG is tried
 * only for the rare origin ImageIO can't read, and only when it differs from the
 * origin (so a SkipHosts origin like Multikino isn't re-fetched pointlessly).
 * A source that never decodes degrades to `None` — a clean text/gradient card
 * rather than a 500.
 */
class PosterImageLoader(posters: PosterFetch) {

  /** The first candidate that decodes, or None when every source fails.
   *
   *  The PRIMARY candidate (index 0) is tried ALONE first: most films' primary
   *  poster works, so the common case stays exactly as cheap as one fetch —
   *  see OgCardServiceSpec's "stop at the first candidate that loads, leaving
   *  later fallbacks unfetched". Only once the primary has failed (often a
   *  Multikino origin Cloudflare 403s our datacentre IP — see
   *  [[OgCardService.card]]'s doc) are the REMAINING fallback candidates
   *  raced CONCURRENTLY rather than walked one at a time: those are real
   *  cinema origins with a legitimately slow (~6-7s) cold connect (see
   *  [[PosterFetch]]), and trying several of them in sequence is what drove
   *  the og-image endpoint's p95 into its histogram cap (2026-09-12). */
  def loadFirst(candidates: Seq[String]): Option[BufferedImage] =
    candidates.headOption.flatMap(load).orElse {
      ConcurrentCandidateProbe.firstMatch("poster-fallbacks", candidates.drop(1), maxConcurrent = MaxConcurrentFallbacks)(load)
    }

  /** Bounds how many fallback candidates race at once — each holds a full image
   *  download + decode in memory for its duration, so an unbounded fan-out
   *  multiplies PEAK memory by the candidate count (up to 5 fallbacks). That is
   *  what OOM-killed `web-pl` twice in ~4h on 2026-09-17: PL's frequent
   *  Multikino-primary-poster miss forces this path disproportionately, and the
   *  cgroup limit has no margin for 5 concurrent image buffers on top of the
   *  JVM's own budget. 3 keeps most of `ba49e065c`'s latency win (worst case
   *  2 rounds instead of 5 sequential fetches) while capping the multiplier. */
  private val MaxConcurrentFallbacks = 3

  def load(url: String): Option[BufferedImage] =
    decode(url).orElse {
      val proxied = PosterProxy.posterForCard(url)
      if (proxied != url) decode(proxied) else None
    }

  private def decode(url: String): Option[BufferedImage] =
    posters.bytes(url).flatMap { b =>
      try Option(ImageIO.read(new ByteArrayInputStream(b)))
      catch { case _: Throwable => None }
    }
}
