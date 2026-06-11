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
 * `http` is the same [[HttpFetch]] every client uses, injected at the
 * composition root, so tests drive a fake returning fixture bytes and never
 * touch the network.
 */
class OgCardService(http: HttpFetch) {
  private val cache: Cache[String, Array[Byte]] =
    Caffeine.newBuilder().maximumSize(1000).expireAfterWrite(12, TimeUnit.HOURS).build()

  def card(title: String, subtitle: String, ratings: Seq[String], posterUrl: Option[String]): Array[Byte] = {
    // Plain concatenation, not an s-interpolator: a nested double-quote (from
    // mkString / getOrElse) inside an interpolation block would close the
    // string early.
    val key = Seq(title, subtitle, ratings.mkString(""), posterUrl.getOrElse("")).mkString(" ")
    cache.get(key, _ => OgCardRenderer.render(title, subtitle, ratings, loadPoster(posterUrl)))
  }

  /** Fetch + decode the poster, or None when there's no poster URL, the fetch
   *  fails, or the bytes aren't a format ImageIO can read -- every one of which
   *  degrades to a clean text-only card rather than a 500. */
  private def loadPoster(posterUrl: Option[String]): Option[BufferedImage] =
    posterUrl.filter(_.nonEmpty).flatMap { url =>
      try Option(ImageIO.read(new ByteArrayInputStream(http.getBytes(PosterProxy.posterForCard(url)))))
      catch { case _: Throwable => None }
    }
}
