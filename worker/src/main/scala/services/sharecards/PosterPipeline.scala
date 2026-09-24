package services.sharecards

import play.api.Logging
import tools.{OgCardRenderer, PosterDecode, PosterDecodeGate, TlsTrust}

import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, InputStream}
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.{Files, Path}
import java.time.Duration
import java.util.concurrent.TimeUnit
import javax.imageio.{IIOImage, ImageIO, ImageWriteParam}
import scala.util.{Try, Using}

/** Smaller renditions of a poster URL to try before the original, where the source offers one —
 *  most posters then never reach a giant decode. Of the poster hosts in the corpus (fixture survey,
 *  2026-09-24) two serve arbitrary sizes on request: TMDB (`/t/p/<size>/`) and IMDb's Amazon CDN
 *  (`…@._V1_SX<width>.jpg`), whose ORIGINALS run to 6116×8974. The cinema hosts serve one file
 *  (several already a thumbnail: bilety24 `sf_api_thumb_400`, Drupal/WordPress derivatives). A slot
 *  is 420×630, so a 780-wide copy covers it at better than 1.5×; 500 wide is the next-smaller try. */
object PosterRendition {
  private val TmdbSized = """(https?://image\.tmdb\.org/t/p/)(original|w\d+)(/.+)""".r
  private val Amazon    = """(https?://m\.media-amazon\.com/images/M/[^@]+@)\._V1_[^/]*\.jpg""".r

  /** The URLs to fetch for `url`, cheapest first, ending with `url` itself. */
  def candidates(url: String): Seq[String] = url match {
    case TmdbSized(prefix, size, path) if size == "original" || size.drop(1).toIntOption.exists(_ > 780) =>
      Seq(s"${prefix}w780$path", s"${prefix}w500$path", url)
    case Amazon(prefix) => (Seq(s"$prefix._V1_SX780.jpg", s"$prefix._V1_SX500.jpg") :+ url).distinct
    case _              => Seq(url)
  }
}

/** What a JPEG's header says about the decode it would cost, read before any pixel is. */
final case class JpegHeader(progressive: Boolean, width: Int, height: Int, sampling: Seq[(Int, Int)]) {
  /** The DCT coefficients a PROGRESSIVE decode holds in memory until its last scan: every block of
   *  every component (chroma planes scaled by their subsampling), 64 coefficients of 2 bytes each. */
  def coefficientBytes: Long = {
    val (hMax, vMax) = (sampling.map(_._1).max, sampling.map(_._2).max)
    sampling.map { case (h, v) =>
      val w = math.ceil(width.toDouble * h / hMax).toLong
      val t = math.ceil(height.toDouble * v / vMax).toLong
      ((w + 7) / 8) * ((t + 7) / 8) * 64 * 2
    }.sum
  }
}

object JpegHeader {
  /** The frame header of the JPEG at `file`, or None for anything that isn't a readable JPEG. */
  def read(file: Path): Option[JpegHeader] =
    Try(Using.resource(new java.io.DataInputStream(new java.io.BufferedInputStream(Files.newInputStream(file)))) { in =>
      if (in.readUnsignedShort() != 0xffd8) None
      else {
        var found = Option.empty[JpegHeader]
        var done  = false
        while (!done) {
          var marker = in.readUnsignedByte()
          while (marker != 0xff) marker = in.readUnsignedByte()
          var code = in.readUnsignedByte()
          while (code == 0xff) code = in.readUnsignedByte()
          if (code == 0xd9 || code == 0xda) done = true                     // EOI / SOS before any frame
          else if (code >= 0xd0 && code <= 0xd7 || code == 0x01) ()         // standalone markers
          else {
            val length = in.readUnsignedShort()
            val sof = code >= 0xc0 && code <= 0xcf && code != 0xc4 && code != 0xc8 && code != 0xcc
            if (sof) {
              in.readUnsignedByte()                                          // precision
              val height = in.readUnsignedShort(); val width = in.readUnsignedShort()
              val sampling = (1 to in.readUnsignedByte()).map { _ =>
                in.readUnsignedByte(); val hv = in.readUnsignedByte(); in.readUnsignedByte()
                ((hv >> 4) max 1, (hv & 0x0f) max 1)
              }
              found = Some(JpegHeader(progressive = code == 0xc2 || code == 0xc6 || code == 0xca || code == 0xce, width, height, sampling))
              done = true
            } else in.skipNBytes((length - 2).toLong)
          }
        }
        found
      }
    }).toOption.flatten
}

/** Downloads a poster to a temp file — never into the heap as a whole — or None. */
trait PosterDownload {
  /** The poster at `url` in a temp file the caller deletes, or None on any failure: a non-2xx, a
   *  timeout, or a body larger than the download cap. Never throws. */
  def fetch(url: String): Option[Path]
}

/** [[PosterDownload]] over the JDK client: the generous connect budget slow cinema origins need
 *  (see `tools.HttpPosterFetch`), a bounded request time, and a byte cap enforced while streaming,
 *  so a multi-hundred-megabyte "poster" is abandoned at the cap rather than written out. */
class HttpPosterDownload(maxBytes: Long = PosterPipeline.MaxDownloadBytes,
                         timeout: Duration = Duration.ofSeconds(20)) extends PosterDownload with Logging {
  private val client = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(15))
    .sslContext(TlsTrust.augmentedContext)
    .build()

  def fetch(url: String): Option[Path] =
    Try {
      val request = HttpRequest.newBuilder().uri(URI.create(url)).timeout(timeout)
        .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
        .GET().build()
      val response = client.send(request, HttpResponse.BodyHandlers.ofInputStream())
      Using.resource(response.body()) { body =>
        if (response.statusCode() / 100 != 2) None else PosterPipeline.copyCapped(body, maxBytes)
      }
    }.toOption.flatten
}

/** Turns a downloaded poster into the card's 420×630 poster slot. */
trait PosterShrinker {
  /** `file` decoded, cover-scaled and cropped to the slot, or None when it can't be read. */
  def coverSlot(file: Path): Option[BufferedImage]
}

/**
 * libvips (`vips thumbnail`) in a memory-capped subprocess; the bounded JDK decode only where vips
 * is absent or cannot read the format at all.
 *
 * THE CHILD SHARES THE WORKER CONTAINER'S MEMORY CGROUP, whose JVM already sits at 700-850 MB of a
 * 960 MiB limit (PL/UK; ES 896 MiB). So a poster may cost the child at most [[PosterPipeline.DecodeMemoryCapMb]]
 * of address space (`ulimit -v`, one malloc arena, no core dump): an overrun kills the child —
 * libjpeg aborts "Insufficient memory" — and never the container. Measured in the worker image
 * (Debian libvips 8.18, 2026-09-24), the cap at 192 MB: a 780-wide TMDB poster and a 2000×3000
 * progressive JPEG shrink (both need ~160 MB of address space, ~45 MB resident); a 96-megapixel
 * baseline JPEG needs 256 MB and a 96-megapixel progressive one more than 1.5 GB (360 MB resident)
 * — both refused. Vips streams a baseline JPEG, but a PROGRESSIVE one holds every DCT coefficient
 * of the whole image to its last scan, so its header ([[JpegHeader.coefficientBytes]]) says in
 * advance whether it can fit: over [[PosterPipeline.ProgressiveCoefficientBudget]] it is refused
 * without spawning anything, and the caller tries the next-smaller rendition.
 *
 * A cap-hit is NOT retried in the JVM: the JDK's decoder holds the same coefficient buffer, in
 * the worker's own native memory. The JDK decode (itself capped at [[PosterDecode.MaxPixels]])
 * runs only when vips is not installed or answers that the file is no image it knows.
 *
 * ONE SHRINK AT A TIME per process ([[VipsPosterShrinker.Gate]]): two capped children are 384 MB of
 * address space against at most ~110-260 MB of headroom on the small workers. The task framework
 * has no per-task-type concurrency limit, so the bound is this gate around the child only — a
 * render's download and composite still run four at once on the pool.
 */
class VipsPosterShrinker(
  binary:        Option[String] = VipsPosterShrinker.locate(),
  memoryCapMb:   Long           = PosterPipeline.DecodeMemoryCapMb,
  timeoutMillis: Long           = 30000L,
  gate:          PosterDecodeGate = VipsPosterShrinker.Gate
) extends PosterShrinker with Logging {

  def coverSlot(file: Path): Option[BufferedImage] =
    JpegHeader.read(file) match {
      case Some(header) if header.progressive && header.coefficientBytes > PosterPipeline.ProgressiveCoefficientBudget =>
        logger.info(s"share card: ${header.width}×${header.height} progressive poster needs ~${header.coefficientBytes >> 20} MB " +
          "to decode — over the cap, not decoded")
        None
      case _ => gate.withPermit(binary.fold(javaDecode(file))(vips(_, file)))
    }

  private def vips(bin: String, file: Path): Option[BufferedImage] = {
    val out = Files.createTempFile("poster-slot-", ".png")
    try {
      // The cap FAILS CLOSED: a Linux shell that cannot set it runs no child. macOS never honours
      // `ulimit -v`, so a developer's machine runs uncapped (time limit and gate only).
      val script = s"""ulimit -c 0; ulimit -v ${memoryCapMb * 1024} 2>/dev/null || [ "$$(uname)" = Darwin ] || exit 97; exec "$$0" thumbnail "$$1" "$$2" ${OgCardRenderer.PosterSlotWidth} --height ${OgCardRenderer.PosterSlotHeight} --crop centre"""
      val process = new ProcessBuilder("/bin/sh", "-c", script, bin, file.toString, out.toString)
        .redirectErrorStream(true).redirectOutput(ProcessBuilder.Redirect.DISCARD)
      process.environment().put("VIPS_CONCURRENCY", "1")
      process.environment().put("MALLOC_ARENA_MAX", "1")
      val child = process.start()
      if (!child.waitFor(timeoutMillis, TimeUnit.MILLISECONDS)) {
        child.destroyForcibly()
        logger.info(s"share card: vips timed out after ${timeoutMillis}ms on $file")
        None
      } else if (child.exitValue() != 0) {
        // vips reads JPEG, PNG, WebP, GIF, TIFF, HEIF: for any of those a failure is a real one (the
        // cap among them). Only a file none of those is goes to the JDK decoder.
        if (VipsPosterShrinker.knownFormat(file)) None else javaDecode(file)
      } else Option(ImageIO.read(out.toFile)).map(slot =>
        if (slot.getWidth == OgCardRenderer.PosterSlotWidth && slot.getHeight == OgCardRenderer.PosterSlotHeight) slot
        else OgCardRenderer.coverSlot(slot))
    } catch { case e: Exception => logger.info(s"share card: vips failed on $file: ${e.getMessage}"); None }
    finally Files.deleteIfExists(out)
  }

  /** The bounded JDK decode (pixel cap + subsampled read). */
  private def javaDecode(file: Path): Option[BufferedImage] =
    Try(PosterDecode.fromFile(file.toFile)).toOption.flatten.map(OgCardRenderer.coverSlot)
}

object VipsPosterShrinker {
  /** The `vips` binary on the PATH, if any. */
  def locate(): Option[String] =
    sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparator).iterator
      .map(dir => Path.of(dir, "vips")).find(Files.isExecutable).map(_.toString)

  /** One shrink at a time per process — see the class doc. */
  val Gate = new PosterDecodeGate(permits = 1)

  /** True when `file` starts like an image format vips reads. */
  private[sharecards] def knownFormat(file: Path): Boolean = Try {
    val head = Using.resource(Files.newInputStream(file))(_.readNBytes(12))
    def at(offset: Int, ascii: String) = head.length >= offset + ascii.length && new String(head.slice(offset, offset + ascii.length), "ISO-8859-1") == ascii
    (head.length >= 2 && (head(0) & 0xff) == 0xff && (head(1) & 0xff) == 0xd8) || at(1, "PNG") || at(0, "GIF8") ||
      (at(0, "RIFF") && at(8, "WEBP")) || at(0, "II*") || at(0, "MM\u0000*") || at(4, "ftyp")
  }.getOrElse(false)
}

object PosterPipeline {
  /** 50 MB: several times any real poster (TMDB originals run to ~10 MB), small enough that a
   *  runaway body is abandoned before it matters. */
  val MaxDownloadBytes: Long = 50L * 1024 * 1024

  /** The vips child's address-space cap — see [[VipsPosterShrinker]] for the measurements. */
  val DecodeMemoryCapMb: Long = tools.Env.positiveLong("KINOWO_SHARE_CARD_DECODE_MEMORY_MB", 192L)

  /** The largest progressive JPEG coefficient buffer sent to vips: the cap less the ~140 MB of
   *  address space vips holds before decoding anything (measured: a 780-wide poster fails under 128
   *  MB and shrinks under 160 MB). A 2000×3000 4:2:0 poster is 18 MB; 96 megapixels is 190-580 MB. */
  val ProgressiveCoefficientBudget: Long = (DecodeMemoryCapMb - 140L).max(16L) * 1024 * 1024

  /** JPEG at 0.95 for the poster cache, not PNG: measured on two real 420×630 TMDB slots, PNG
   *  stored 520-620 KB each and q95 116-152 KB — at ~2,000 posters a country, PNG alone would
   *  fill the whole 1 GiB budget. At 0.95 the re-encode under the card's own 0.85 is invisible. */
  val PosterQuality = 0.95f

  /** Stream `in` into a temp file, abandoning it past `maxBytes`. */
  private[sharecards] def copyCapped(in: InputStream, maxBytes: Long): Option[Path] = {
    val file = Files.createTempFile("poster-", ".img")
    val kept = Try {
      Using.resource(Files.newOutputStream(file)) { out =>
        val buffer = new Array[Byte](64 * 1024)
        var total  = 0L
        var read   = in.read(buffer)
        while (read >= 0 && total <= maxBytes) {
          total += read
          if (total <= maxBytes) out.write(buffer, 0, read)
          read = in.read(buffer)
        }
        total <= maxBytes && total > 0
      }
    }.getOrElse(false)
    if (kept) Some(file) else { Files.deleteIfExists(file); None }
  }

  /** `slot` as the poster cache stores it. */
  def encodePoster(slot: BufferedImage): Array[Byte] = {
    val bytes  = new ByteArrayOutputStream()
    val writer = ImageIO.getImageWritersByFormatName("jpg").next()
    val stream = ImageIO.createImageOutputStream(bytes)
    try {
      writer.setOutput(stream)
      val params = writer.getDefaultWriteParam
      params.setCompressionMode(ImageWriteParam.MODE_EXPLICIT)
      params.setCompressionQuality(PosterQuality)
      writer.write(null, new IIOImage(slot, null, null), params)
    } finally { writer.dispose(); stream.close() }
    bytes.toByteArray
  }
}

/**
 * A film card's poster: the film's cached one when it is still one of the film's candidates, else
 * fetched, shrunk to the slot and cached — ONE poster file per film, overwritten when the chosen
 * poster changes. The file is stamped with the key of the URL it came from (the poster as the film
 * lists it, not the smaller rendition fetched in its place), which is how a changed poster is told.
 */
class ShareCardPosters(store: ShareCardStore, download: PosterDownload, shrinker: PosterShrinker,
                       metrics: ShareCardMetrics) extends Logging {

  /** The first usable candidate — its URL, which the card's version hashes, and its slot image — or
   *  None when none can be had. */
  def load(filmId: String, candidates: Seq[String]): Option[(String, BufferedImage)] =
    cached(filmId, candidates) match {
      case hit @ Some(_) => metrics.posterCache(hit = true); hit
      case None =>
        metrics.posterCache(hit = false)
        candidates.iterator.flatMap(url => fetchAndCache(filmId, url).map(url -> _)).nextOption()
    }

  private def cached(filmId: String, candidates: Seq[String]): Option[(String, BufferedImage)] = {
    val path = store.posterPath(filmId)
    store.version(path).flatMap(key => candidates.find(ShareCardPosters.key(_) == key))
      .flatMap(url => Try(Option(ImageIO.read(path.toFile))).toOption.flatten.map(url -> _))
  }

  private def fetchAndCache(filmId: String, url: String): Option[BufferedImage] = {
    val slot = PosterRendition.candidates(url).iterator.flatMap { rendition =>
      download.fetch(rendition).flatMap { file =>
        try shrinker.coverSlot(file) finally Files.deleteIfExists(file)
      }
    }.nextOption()
    metrics.posterFetch(ok = slot.isDefined)
    slot.foreach(image => store.writeAtomically(store.posterPath(filmId), PosterPipeline.encodePoster(image), ShareCardPosters.key(url)))
    slot
  }
}

object ShareCardPosters {
  /** A poster's key: 16 hex characters of the SHA-256 of its URL. */
  def key(url: String): String = tools.Digest.sha256Hex(url).take(16)
}
