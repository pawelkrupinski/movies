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

class PosterDecodeSpec extends AnyFlatSpec with Matchers {

  private def file(bytes: Array[Byte]): java.io.File = {
    val f = java.nio.file.Files.createTempFile("poster-", ".img")
    java.nio.file.Files.write(f, bytes)
    f.toFile.deleteOnExit()
    f.toFile
  }

  private def jpeg(width: Int, height: Int): java.io.File = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    val out   = new ByteArrayOutputStream()
    ImageIO.write(image, "jpg", out)
    file(out.toByteArray)
  }

  // web-pl OOM KILLS, 2026-09-21 and 09-23: a progressive JPEG's decoder holds a native coefficient
  // buffer the size of the WHOLE image (~30 MB of RSS for a 2000×3000 poster, measured in a 1 GiB
  // container) outside every JVM cap, and a per-card bound could not cap a crawler opening many
  // cards at once. The worker's four render tasks share this gate the same way.
  "decoding" should "be bounded across every concurrent caller" in {
    val gate = new PosterDecodeGate(permits = 2)
    val payload = file(CountingReader.Magic)
    val counted = CountingReader.registered { reader =>
      val pool = Executors.newFixedThreadPool(8)
      try {
        implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
        val decodes = (1 to 8).map(_ => Future(gate.withPermit(PosterDecode.fromFile(payload))))
        Await.result(Future.sequence(decodes), 30.seconds).flatten should have size 8
      } finally { pool.shutdown(); pool.awaitTermination(10, TimeUnit.SECONDS) }
      reader
    }
    counted.reads.get() shouldBe 8
    counted.peak.get() should be <= 2
  }

  // The counter was once an object shared by every test and zeroed by hand, so a test that forgot
  // the reset read another's decodes. Each is now its test's own, registered only while it runs.
  "a counting reader" should "count only its own test's decodes, and leave ImageIO when the test ends" in {
    val payload = file(CountingReader.Magic)
    val first = CountingReader.registered { reader => PosterDecode.fromFile(payload) shouldBe defined; reader }
    PosterDecode.fromFile(payload) shouldBe None
    CountingReader.registered { second =>
      PosterDecode.fromFile(payload) shouldBe defined
      second.reads.get() shouldBe 1
    }
    first.reads.get() shouldBe 1
  }

  // A 2000×3000 poster decoded whole is a 6 MP raster (18-24 MB of heap) for a slot 420×630 pixels
  // wide. The decoder subsamples at read time to the smallest size that still COVERS the slot, so no
  // card is ever upscaled.
  "PosterDecode" should "decode a large poster subsampled to just cover the card's poster slot" in {
    val poster = PosterDecode.fromFile(jpeg(2000, 3000)).get
    poster.getWidth should (be >= OgCardRenderer.PosterSlotWidth and be < 2 * OgCardRenderer.PosterSlotWidth)
    poster.getHeight should (be >= OgCardRenderer.PosterSlotHeight and be < 2 * OgCardRenderer.PosterSlotHeight)
  }

  it should "decode a poster already smaller than the slot at its own size" in {
    val poster = PosterDecode.fromFile(jpeg(300, 450)).get
    (poster.getWidth, poster.getHeight) shouldBe (300, 450)
  }

  // The native buffer grows with the pixel count and subsampling does not shrink it, so a poster
  // larger than any card could use is refused BEFORE its pixels are decoded. (The worker hands such
  // posters to libvips first; this is the fallback's bound.)
  it should "refuse a poster whose header declares more pixels than the cap, without decoding it" in {
    PosterDecode.fromFile(jpeg(4000, 4000)) shouldBe None
  }
}

/** A test-only ImageIO reader for a made-up format (payload `CNTR…`) that records how many reads
 *  run at once. Registering it with ImageIO's own registry puts the counter at the exact point a
 *  decode hands the stream to a decoder. One per test, via [[CountingReader.registered]]: the
 *  registry is JVM-wide, so the SPI is registered only for the test's body. */
private final class CountingReader {
  val reads    = new AtomicInteger(0)
  val peak     = new AtomicInteger(0)
  private val inFlight = new AtomicInteger(0)
  val spi: ImageReaderSpi = new CountingReader.Spi(this)

  private[tools] def counting[A](body: => A): A = {
    reads.incrementAndGet()
    val now = inFlight.incrementAndGet()
    peak.updateAndGet(prev => math.max(prev, now))
    try body finally inFlight.decrementAndGet()
  }
}

private object CountingReader {
  val Magic: Array[Byte] = "CNTR-poster".getBytes("US-ASCII")

  /** Runs `body` with a fresh reader registered in ImageIO, deregistering it afterwards. */
  def registered[A](body: CountingReader => A): A = {
    val reader   = new CountingReader
    val registry = IIORegistry.getDefaultInstance
    registry.registerServiceProvider(reader.spi)
    try body(reader) finally registry.deregisterServiceProvider(reader.spi)
  }

  final class Spi(counter: CountingReader)
      extends ImageReaderSpi("kinowo-test", "1", Array("cntr"), Array("cntr"), Array("image/x-cntr"),
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
    def createReaderInstance(extension: AnyRef): ImageReader = new Reader(this, counter)
    def getDescription(locale: Locale): String = "concurrency-counting test reader"
  }

  final class Reader(spi: ImageReaderSpi, counter: CountingReader) extends ImageReader(spi) {
    def getNumImages(allowSearch: Boolean): Int = 1
    def getWidth(imageIndex: Int): Int = 40
    def getHeight(imageIndex: Int): Int = 60
    def getImageTypes(imageIndex: Int): java.util.Iterator[ImageTypeSpecifier] =
      java.util.List.of(ImageTypeSpecifier.createFromBufferedImageType(BufferedImage.TYPE_INT_RGB)).iterator()
    def getStreamMetadata: javax.imageio.metadata.IIOMetadata = null
    def getImageMetadata(imageIndex: Int): javax.imageio.metadata.IIOMetadata = null
    def read(imageIndex: Int, param: ImageReadParam): BufferedImage =
      counter.counting { Thread.sleep(100); new BufferedImage(40, 60, BufferedImage.TYPE_INT_RGB) }
  }
}
