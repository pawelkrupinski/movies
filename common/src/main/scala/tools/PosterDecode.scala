package tools

import java.awt.image.BufferedImage
import java.util.concurrent.Semaphore
import javax.imageio.ImageIO
import scala.util.Using

/**
 * How many posters may be DECODING at once in this process, across every render.
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
 * limit with 445 MB of it accounted for by no JVM pool; a per-card bound could not help, since a
 * crawler opens many cards at once. That is why the cards moved to the worker -- where every shrink
 * (libvips in a capped subprocess, or the JDK decode below) still holds one of these permits,
 * because the worker's render tasks run four at once on its pool.
 *
 * Only the decode holds a permit, not the download: a slow origin must not stall every other
 * card's decode behind it.
 */
class PosterDecodeGate(permits: Int) {
  private val semaphore = new Semaphore(permits, true)

  def withPermit[A](body: => A): A = {
    semaphore.acquire()
    try body finally semaphore.release()
  }
}

object PosterDecodeGate {
  /** The one gate the process's poster decodes share. A process singleton on purpose: the resource
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
object PosterDecode {
  /** 12 MP -- a 2828×4243 poster -- is ~6× what the largest slot can show at 2× density, and bounds
   *  one progressive decode's native buffer at ~70 MB. */
  val MaxPixels: Long = 12L * 1000 * 1000

  /** The bounded read of a poster on disk — the worker's download lands in a file, so a large
   *  poster is never held in the heap as bytes too. */
  def fromFile(file: java.io.File): Option[BufferedImage] =
    Using.resource(new javax.imageio.stream.FileImageInputStream(file))(read)

  private def read(in: javax.imageio.stream.ImageInputStream): Option[BufferedImage] = {
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
