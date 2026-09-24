package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.util.Locale
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import javax.imageio.spi.{IIORegistry, ImageReaderSpi}
import javax.imageio.stream.ImageInputStream
import javax.imageio.{ImageIO, ImageReadParam, ImageReader, ImageTypeSpecifier}
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

class PosterImageLoaderSpec extends AnyFlatSpec with Matchers {

  /** Tracks how many `bytes` calls are ever in flight at once, and always fails
   *  to decode (so `loadFirst` walks every fallback candidate rather than
   *  stopping early). All URLs are `multikino.pl` — a `PosterProxy` `SkipHosts`
   *  entry, so `PosterImageLoader.load`'s proxy fallback never fires a SECOND
   *  `bytes` call per candidate, keeping the concurrency count meaningful. */
  private class TrackingFetch(delayMillis: Long) extends PosterFetch {
    private val inFlight = new AtomicInteger(0)
    val peakConcurrent = new AtomicInteger(0)
    def bytes(url: String): Option[Array[Byte]] = {
      val now = inFlight.incrementAndGet()
      peakConcurrent.updateAndGet(prev => math.max(prev, now))
      Thread.sleep(delayMillis)
      inFlight.decrementAndGet()
      None
    }
  }

  private class FixedFetch(payload: Array[Byte]) extends PosterFetch {
    val calls = new AtomicInteger(0)
    def bytes(url: String): Option[Array[Byte]] = { calls.incrementAndGet(); Some(payload) }
  }

  private def jpeg(width: Int, height: Int): Array[Byte] = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    val out   = new ByteArrayOutputStream()
    ImageIO.write(image, "jpg", out)
    out.toByteArray
  }

  // web-pl OOM kills, 2026-09-17: racing every fallback candidate at once
  // multiplies peak memory (a full image download + decode per candidate) by
  // the candidate count. This caps it, trading some of the latency win
  // `ba49e065c` was for back for a bounded worst case.
  "loadFirst" should "never race more than a small bounded number of fallback candidates at once" in {
    val fetch = new TrackingFetch(delayMillis = 80)
    val loader = new PosterImageLoader(fetch)
    val candidates = (1 to 6).map(i => s"https://www.multikino.pl/poster/$i.jpg")

    val result = loader.loadFirst(candidates)

    result shouldBe None
    fetch.peakConcurrent.get() should be <= 3
  }

  // web-pl OOM KILLS, 2026-09-21 and 09-23, with the per-request bound above already in place. That
  // bound is per CARD; a crawler sweeping many films opens many cards at once, and a progressive
  // JPEG's decoder holds a native coefficient buffer the size of the WHOLE image (~30 MB of RSS for
  // a 2000×3000 poster, measured in a 1 GiB container) outside every JVM cap. Twelve at once added
  // ~340 MB of RSS over a pre-touched 384 MB heap. The bound that matters is across requests.
  "decoding" should "be bounded across every concurrent card, not only within one" in {
    val gate    = new PosterDecodeGate(permits = 2)
    val loaders = Seq(new PosterImageLoader(new FixedFetch(CountingReader.Magic), gate),
                      new PosterImageLoader(new FixedFetch(CountingReader.Magic), gate))
    CountingReader.reset()
    val registry = IIORegistry.getDefaultInstance
    registry.registerServiceProvider(CountingReader.Spi)
    val pool = Executors.newFixedThreadPool(8)
    try {
      implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
      val requests = (1 to 8).map(i => Future(loaders(i % 2).loadFirst(Seq(s"https://www.multikino.pl/$i.jpg"))))
      Await.result(Future.sequence(requests), 30.seconds).flatten should have size 8
    } finally {
      pool.shutdown(); pool.awaitTermination(10, TimeUnit.SECONDS)
      registry.deregisterServiceProvider(CountingReader.Spi)
    }
    CountingReader.reads.get() shouldBe 8
    CountingReader.peak.get() should be <= 2
  }

  // A 2000×3000 poster decoded whole is a 6 MP raster (18-24 MB of heap) for a slot 420×630 pixels
  // wide; eight of those at once is half the 384 MB heap. The decoder subsamples at read time to the
  // smallest size that still COVERS the largest slot, so no card is ever upscaled.
  "load" should "decode a large poster subsampled to just cover the card's poster slot" in {
    val poster = new PosterImageLoader(new FixedFetch(jpeg(2000, 3000)), new PosterDecodeGate(1))
      .load("https://www.multikino.pl/big.jpg").get

    poster.getWidth should (be >= OgCardRenderer.PosterSlotWidth and be < 2 * OgCardRenderer.PosterSlotWidth)
    poster.getHeight should (be >= OgCardRenderer.PosterSlotHeight and be < 2 * OgCardRenderer.PosterSlotHeight)
  }

  it should "decode a poster already smaller than the slot at its own size" in {
    val poster = new PosterImageLoader(new FixedFetch(jpeg(300, 450)), new PosterDecodeGate(1))
      .load("https://www.multikino.pl/small.jpg").get

    (poster.getWidth, poster.getHeight) shouldBe (300, 450)
  }

  // The native buffer grows with the pixel count and subsampling does not shrink it, so an origin
  // serving a poster larger than any card could use is refused BEFORE its pixels are decoded -- the
  // loader then falls through to the next candidate, as it would for a poster that failed to fetch.
  it should "refuse a poster whose header declares more pixels than the cap, without decoding it" in {
    val fetch = new FixedFetch(jpeg(4000, 4000))

    new PosterImageLoader(fetch, new PosterDecodeGate(1)).load("https://www.multikino.pl/huge.jpg") shouldBe None
  }
}

/** A test-only ImageIO reader for a made-up format (payload `CNTR…`) that records how many reads
 *  run at once. Registering it with ImageIO's own registry puts the counter at the exact point the
 *  loader hands bytes to a decoder, whichever ImageIO entry point the loader uses. */
private object CountingReader {
  val Magic: Array[Byte] = "CNTR-poster".getBytes("US-ASCII")
  val reads    = new AtomicInteger(0)
  val peak     = new AtomicInteger(0)
  private val inFlight = new AtomicInteger(0)
  def reset(): Unit = { reads.set(0); peak.set(0); inFlight.set(0) }

  object Spi extends ImageReaderSpi("kinowo-test", "1", Array("cntr"), Array("cntr"), Array("image/x-cntr"),
                                    classOf[Reader].getName, Array(classOf[ImageInputStream]), null,
                                    false, null, null, null, null, false, null, null, null, null) {
    def canDecodeInput(source: AnyRef): Boolean = source match {
      case in: ImageInputStream =>
        val head = new Array[Byte](4)
        in.mark()
        try { in.readFully(head); head.sameElements(Magic.take(4)) }
        catch { case _: java.io.IOException => false }
        finally in.reset()
      case _ => false
    }
    def createReaderInstance(extension: AnyRef): ImageReader = new Reader(this)
    def getDescription(locale: Locale): String = "concurrency-counting test reader"
  }

  class Reader(spi: ImageReaderSpi) extends ImageReader(spi) {
    def getNumImages(allowSearch: Boolean): Int = 1
    def getWidth(imageIndex: Int): Int = 40
    def getHeight(imageIndex: Int): Int = 60
    def getImageTypes(imageIndex: Int): java.util.Iterator[ImageTypeSpecifier] =
      java.util.List.of(ImageTypeSpecifier.createFromBufferedImageType(BufferedImage.TYPE_INT_RGB)).iterator()
    def getStreamMetadata: javax.imageio.metadata.IIOMetadata = null
    def getImageMetadata(imageIndex: Int): javax.imageio.metadata.IIOMetadata = null
    def read(imageIndex: Int, param: ImageReadParam): BufferedImage = {
      reads.incrementAndGet()
      val now = inFlight.incrementAndGet()
      peak.updateAndGet(prev => math.max(prev, now))
      try { Thread.sleep(100); new BufferedImage(40, 60, BufferedImage.TYPE_INT_RGB) }
      finally inFlight.decrementAndGet()
    }
  }
}
