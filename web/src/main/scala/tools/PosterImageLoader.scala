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

  /** The first candidate that decodes, or None when every source fails. */
  def loadFirst(candidates: Seq[String]): Option[BufferedImage] =
    candidates.iterator.flatMap(load(_).iterator).nextOption()

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
