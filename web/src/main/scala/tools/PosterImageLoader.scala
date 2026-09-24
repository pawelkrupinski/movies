package tools

import java.awt.image.BufferedImage
import java.io.ByteArrayInputStream
import java.util.concurrent.Semaphore
import javax.imageio.ImageIO
import javax.imageio.stream.MemoryCacheImageInputStream
import scala.util.Using

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
 *
 * Every decode goes through `gate`, the PROCESS-WIDE bound on how many posters
 * decode at once (see [[PosterDecodeGate]]), and through [[PosterDecode]], which
 * reads the image no larger than the card can use.
 */
class PosterImageLoader(posters: PosterFetch, gate: PosterDecodeGate = PosterDecodeGate.Shared) {

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
      try gate.withPermit(PosterDecode(b))
      catch { case _: Throwable => None }
    }
}

/**
 * How many posters may be DECODING at once in this process, across every card and every request.
 *
 * ⚠️ THIS IS A NATIVE-MEMORY BOUND, NOT A HEAP ONE, and that is why it has to exist. A progressive
 * JPEG's decoder (libjpeg, inside the JDK) holds a coefficient buffer the size of the WHOLE image in
 * malloc'd memory for the length of the decode -- outside `-Xmx`, metaspace, code cache and direct
 * memory alike, so nothing in the JVM bounds it and no JMX pool shows it. Measured in a 1 GiB
 * container with a pre-touched 384 MB heap: one 2000×3000 progressive poster decoding adds ~30 MB
 * of RSS, four at once ~100 MB, twelve at once ~340 MB -- and glibc keeps it after the decode ends.
 *
 * web-pl was OOM-killed seven times on 2026-09-21 17:01-18:25Z while AhrefsBot swept the film share
 * cards (612 og-image requests against ~10 on a normal evening): RSS reached 911 MB of the 1 GiB
 * limit with 445 MB of it accounted for by no JVM pool. The per-card bound in
 * [[PosterImageLoader.loadFirst]] could not help -- it limits the fallbacks ONE card races, and a
 * crawler opens many cards at once. Only a bound across requests caps the sum.
 *
 * Only the decode holds a permit, not the download: a slow origin (up to the 20s read budget in
 * [[HttpPosterFetch]]) must not stall every other card's decode behind it.
 */
class PosterDecodeGate(permits: Int) {
  private val semaphore = new Semaphore(permits, true)

  def withPermit[A](body: => A): A = {
    semaphore.acquire()
    try body finally semaphore.release()
  }
}

object PosterDecodeGate {
  /** The one gate the process's card services share. A process singleton on purpose: the resource
   *  it rations -- the container's native memory -- is one per process too. Two permits cost at most
   *  ~2×[[PosterDecode.MaxPixels]] of native buffer while keeping a card's primary poster from
   *  queueing behind a single slow decode. */
  val Shared = new PosterDecodeGate(permits = 2)
}

/**
 * Decode poster bytes to the SMALLEST image that still covers the largest card slot
 * ([[OgCardRenderer.PosterSlotWidth]] × [[OgCardRenderer.PosterSlotHeight]]), or None.
 *
 * Two bounds, both read from the image HEADER before a single pixel is decoded:
 *
 *  - SUBSAMPLED READ. A 2000×3000 poster decoded whole is an 18-24 MB raster for a 420×630 slot;
 *    the reader skips rows and columns instead, so the heap holds ~1/16th of that and the renderer
 *    never scales a giant image down. The step is the largest integer that keeps BOTH sides at or
 *    above the slot, so the card is never upscaled from a poster that had the pixels.
 *  - A PIXEL CAP. Subsampling does not shrink the decoder's native coefficient buffer (see
 *    [[PosterDecodeGate]]) -- that one is sized by the SOURCE -- so a poster larger than any card
 *    could use is refused outright. The loader then tries the next source (the weserv-resized copy
 *    of the same URL, then the next candidate), exactly as for a poster that failed to fetch.
 */
private[tools] object PosterDecode {
  /** 12 MP -- a 2828×4243 poster -- is ~6× what the largest slot can show at 2× density, and bounds
   *  one progressive decode's native buffer at ~70 MB. */
  val MaxPixels: Long = 12L * 1000 * 1000

  def apply(bytes: Array[Byte]): Option[BufferedImage] =
    Using.resource(new MemoryCacheImageInputStream(new ByteArrayInputStream(bytes))) { in =>
      val readers = ImageIO.getImageReaders(in)
      Option.when(readers.hasNext)(readers.next()).flatMap { reader =>
        try {
          reader.setInput(in, true, true)
          val (width, height) = (reader.getWidth(0), reader.getHeight(0))
          Option.when(width.toLong * height <= MaxPixels) {
            val step  = math.max(1, math.min(width / OgCardRenderer.PosterSlotWidth, height / OgCardRenderer.PosterSlotHeight))
            val param = reader.getDefaultReadParam
            param.setSourceSubsampling(step, step, 0, 0)
            Option(reader.read(0, param))
          }.flatten
        } finally reader.dispose()
      }
    }
}
