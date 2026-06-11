package tools

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.util.concurrent.TimeUnit
import javax.imageio.ImageIO

/**
 * Orchestrates the film Open Graph card: fetch + decode the poster, hand it to
 * the pure [[OgCardRenderer]], and memoise the resulting PNG so the repeated
 * shares + Facebook's aggressive re-scrapes of the same film don't re-fetch
 * the poster and re-rasterise the card every time.
 *
 * The cache key is the full set of card inputs (title + subtitle + ratings +
 * poster URL). When the worker refreshes a rating or a cinema swaps the
 * poster, the key diverges and the stale card simply falls out of use; the TTL
 * + max-size only bound growth for inputs that never change.
 *
 * `posters` is injected at the composition root, so tests drive a fake
 * returning fixture bytes and never touch the network.
 */
class OgCardService(posters: PosterFetch) {
  private val cache: Cache[String, Array[Byte]] =
    Caffeine.newBuilder().maximumSize(1000).expireAfterWrite(12, TimeUnit.HOURS).build()

  def card(title: String, subtitle: String, badges: Seq[OgCardRenderer.Badge], posterUrl: Option[String]): Array[Byte] = {
    // Plain concatenation, not an s-interpolator: a nested double-quote (from
    // mkString / getOrElse) inside an interpolation block would close the
    // string early.
    val ratingKey = badges.flatMap(_.segs.map(_.text)).mkString(",")
    val key = Seq(title, subtitle, ratingKey, posterUrl.getOrElse("")).mkString(" ")
    Option(cache.getIfPresent(key)).getOrElse {
      val poster = loadPoster(posterUrl)
      val bytes  = OgCardRenderer.render(title, subtitle, badges, poster)
      // Only cache a *complete* card: one with no poster to show, or whose
      // poster actually loaded. A transient poster-fetch failure must NOT be
      // frozen for the cache's lifetime as a posterless card -- leave it
      // uncached so the next share retries (origin/weserv will likely be warm
      // by then). This was the bug behind text-only cards on first share.
      if (posterUrl.forall(_.isEmpty) || poster.isDefined) cache.put(key, bytes)
      bytes
    }
  }

  /** Fetch + decode the poster, or None when there's no poster URL or every
   *  source fails / isn't an ImageIO-readable format -- which degrades to a
   *  clean text-only card rather than a 500.
   *
   *  Fetches the origin URL directly ([[PosterFetch]] gives it the generous
   *  connect budget slow cinema origins need, which the scraper's tight 5s
   *  budget denied). Falls back to the weserv JPEG for the rare webp-only
   *  origin ImageIO can't decode natively. */
  private def loadPoster(posterUrl: Option[String]): Option[BufferedImage] =
    posterUrl.filter(_.nonEmpty).flatMap { url =>
      decode(url).orElse(decode(PosterProxy.posterForCard(url)))
    }

  private def decode(url: String): Option[BufferedImage] =
    posters.bytes(url).flatMap { b =>
      try Option(ImageIO.read(new ByteArrayInputStream(b)))
      catch { case _: Throwable => None }
    }
}
