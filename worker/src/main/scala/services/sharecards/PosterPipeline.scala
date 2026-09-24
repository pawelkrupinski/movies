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

/** Smaller renditions of a poster URL to try before the original, where the source offers one.
 *  TMDB serves every poster at fixed widths under `/t/p/<size>/`; a card's slot is 420×630, so
 *  `w780` covers it at better than 1.5× while `original` can be a 6000-pixel scan. */
object PosterRendition {
  private val TmdbSized = """(https?://image\.tmdb\.org/t/p/)(original|w\d+)(/.+)""".r

  /** The URLs to fetch for `url`, cheapest first, ending with `url` itself. */
  def candidates(url: String): Seq[String] = url match {
    case TmdbSized(prefix, "original", path)                                         => Seq(s"${prefix}w780$path", url)
    case TmdbSized(prefix, size, path) if size.drop(1).toIntOption.exists(_ > 780)   => Seq(s"${prefix}w780$path", url)
    case _                                                                           => Seq(url)
  }
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
 * libvips (`vips thumbnail`) in a subprocess, with the bounded Java decode as the fallback for
 * whatever vips cannot read (or when the binary is absent).
 *
 * WHY A SUBPROCESS. vips shrinks on load — libjpeg's DCT scaling, streamed row by row — so a
 * baseline JPEG of any size decodes in memory near the OUTPUT's (measured: an 8000×12000 baseline
 * JPEG thumbnails in 76 MB RSS). The JDK decoder cannot: it refuses posters over
 * [[PosterDecode.MaxPixels]] rather than risk the native buffer. A PROGRESSIVE JPEG is the one
 * case vips cannot stream — libjpeg must hold every coefficient to the last scan (measured: 360 MB
 * RSS for 8000×12000) — so the child runs under an address-space cap and a time limit, and a
 * poster past either fails like any unreadable one. Running it out of process means that failure
 * is the child's, never the worker's heap or the JVM's native memory.
 *
 * Concurrency: every shrink holds a permit of the process-wide [[PosterDecodeGate]] (two), the
 * same gate the web's decodes share, since the render tasks run on the worker's pool of four and
 * two giant progressive decodes at once is already ~720 MB.
 */
class VipsPosterShrinker(
  binary:        Option[String] = VipsPosterShrinker.locate(),
  memoryCapMb:   Long           = PosterPipeline.DecodeMemoryCapMb,
  timeoutMillis: Long           = 30000L,
  gate:          PosterDecodeGate = PosterDecodeGate.Shared
) extends PosterShrinker with Logging {

  def coverSlot(file: Path): Option[BufferedImage] =
    gate.withPermit(binary.flatMap(vips(_, file)).orElse(javaDecode(file)))

  private def vips(bin: String, file: Path): Option[BufferedImage] = {
    val out = Files.createTempFile("poster-slot-", ".png")
    try {
      // `ulimit -v` bounds the child's address space where the kernel honours it (Linux); a
      // platform that doesn't (macOS) still gets the time limit and the concurrency cap.
      val script = s"""ulimit -v ${memoryCapMb * 1024} 2>/dev/null; exec "$$0" thumbnail "$$1" "$$2" ${OgCardRenderer.PosterSlotWidth} --height ${OgCardRenderer.PosterSlotHeight} --crop centre"""
      val process = new ProcessBuilder("/bin/sh", "-c", script, bin, file.toString, out.toString)
        .redirectErrorStream(true).redirectOutput(ProcessBuilder.Redirect.DISCARD)
      process.environment().put("VIPS_CONCURRENCY", "1")
      val child = process.start()
      if (!child.waitFor(timeoutMillis, TimeUnit.MILLISECONDS)) {
        child.destroyForcibly()
        logger.info(s"share card: vips timed out after ${timeoutMillis}ms on $file")
        None
      } else if (child.exitValue() != 0) None
      else Option(ImageIO.read(out.toFile)).map(slot =>
        if (slot.getWidth == OgCardRenderer.PosterSlotWidth && slot.getHeight == OgCardRenderer.PosterSlotHeight) slot
        else OgCardRenderer.coverSlot(slot))
    } catch { case e: Exception => logger.info(s"share card: vips failed on $file: ${e.getMessage}"); None }
    finally Files.deleteIfExists(out)
  }

  /** The bounded JDK decode (pixel cap + subsampled read), for a format vips can't read or a
   *  worker without vips. */
  private def javaDecode(file: Path): Option[BufferedImage] =
    Try(PosterDecode.fromFile(file.toFile)).toOption.flatten.map(OgCardRenderer.coverSlot)
}

object VipsPosterShrinker {
  /** The `vips` binary on the PATH, if any. */
  def locate(): Option[String] =
    sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparator).iterator
      .map(dir => Path.of(dir, "vips")).find(Files.isExecutable).map(_.toString)
}

object PosterPipeline {
  /** 50 MB: several times any real poster (TMDB originals run to ~10 MB), small enough that a
   *  runaway body is abandoned before it matters. */
  val MaxDownloadBytes: Long = 50L * 1024 * 1024

  /** The vips child's address-space cap. Virtual, not resident: vips reserves well beyond what it
   *  touches, so this sits above the ~360 MB a giant progressive JPEG really uses. */
  val DecodeMemoryCapMb: Long = tools.Env.positiveLong("KINOWO_SHARE_CARD_DECODE_MEMORY_MB", 1536L)

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
 * A film card's poster, from the store's poster cache when any candidate is cached, else fetched,
 * shrunk to the slot and cached — so only a poster URL the country has never seen costs a
 * download and a decode. Keyed by the SHA-256 of the poster URL as the film lists it (not of the
 * smaller rendition fetched in its place), so the cache key moves exactly when the film's poster does.
 */
class ShareCardPosters(store: ShareCardStore, download: PosterDownload, shrinker: PosterShrinker,
                       metrics: ShareCardMetrics) extends Logging {

  /** The first usable candidate — its URL, which the card's name hashes, and its slot image — or
   *  None when none can be had. */
  def load(candidates: Seq[String]): Option[(String, BufferedImage)] =
    candidates.iterator.flatMap(url => cached(url).map(url -> _)).nextOption() match {
      case hit @ Some(_) => metrics.posterCache(hit = true); hit
      case None =>
        metrics.posterCache(hit = false)
        candidates.iterator.flatMap(url => fetchAndCache(url).map(url -> _)).nextOption()
    }

  private def cached(url: String): Option[BufferedImage] = {
    val path = store.posterPath(ShareCardPosters.key(url))
    if (!Files.isRegularFile(path)) None else Try(Option(ImageIO.read(path.toFile))).toOption.flatten
  }

  private def fetchAndCache(url: String): Option[BufferedImage] = {
    val slot = PosterRendition.candidates(url).iterator.flatMap { rendition =>
      download.fetch(rendition).flatMap { file =>
        try shrinker.coverSlot(file) finally Files.deleteIfExists(file)
      }
    }.nextOption()
    metrics.posterFetch(ok = slot.isDefined)
    slot.foreach(image => store.writeAtomically(store.posterPath(ShareCardPosters.key(url)), PosterPipeline.encodePoster(image)))
    slot
  }
}

object ShareCardPosters {
  /** A poster's cache key: the hex SHA-256 of its URL. */
  def key(url: String): String = tools.Digest.sha256Hex(url)
}
