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

  /** True for a poster on a source that serves arbitrary sizes. */
  def resizable(url: String): Boolean = url match {
    case TmdbSized(_, _, _) | Amazon(_) => true
    case _                              => false
  }

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

/** Why a poster could not be had — the bounded `reason` label on
 *  `kinowo_worker_share_cards_poster_fetch_total{result="failed"}`. */
object PosterFailure {
  val Http4xx = "http_4xx"; val Http5xx = "http_5xx"; val HttpOther = "http_other"
  val Timeout = "timeout"; val Network = "network"; val TooLarge = "too_large"; val EmptyBody = "empty_body"
  /** Neither vips nor the JDK could read it (corrupt, or no image format either knows). */
  val DecodeError = "decode_error"
  /** The vips child hit its memory cap (or could not set it). */
  val VipsCap = "vips_cap"
  /** A progressive JPEG whose header says its decode cannot fit the cap — refused unspawned. */
  val ProgressiveEstimate = "progressive_estimate"
  /** Shrunk, but the slot could not be encoded for the cache, or something threw on the way. */
  val EncodeError = "encode_error"
  val all: Seq[String] = Seq(Http4xx, Http5xx, HttpOther, Timeout, Network, TooLarge, EmptyBody, DecodeError, VipsCap,
    ProgressiveEstimate, EncodeError)
  /** The reason label of a fetch that worked. */
  val None = "none"
}

/** Downloads a poster to a temp file — never into the heap as a whole. */
trait PosterDownload {
  /** The poster at `url` in a temp file the caller deletes, or the [[PosterFailure]] why not. Never throws. */
  def fetch(url: String): Either[String, Path]
}

object PosterDownload {
  /** `byHost`'s download for a URL on one of its hosts, `direct` for any other. */
  def routed(direct: PosterDownload, byHost: Map[String, PosterDownload]): PosterDownload = new PosterDownload {
    def fetch(url: String): Either[String, Path] =
      Try(URI.create(url).getHost).toOption.flatMap(Option(_)).map(_.toLowerCase).flatMap(byHost.get).getOrElse(direct).fetch(url)
  }
}

/** [[PosterDownload]] through a scraper egress — the residential-proxy chain a Cloudflare-blocked
 *  site's scrapes use — for that site's posters. The chain hands back the whole body, so the cap
 *  is checked once it has arrived; a poster is fetched once and then cached, so the proxy (Zyte
 *  only when the proxy fails) sees one request per film. */
class EgressPosterDownload(http: tools.HttpFetch, maxBytes: Long = PosterPipeline.MaxDownloadBytes) extends PosterDownload {
  def fetch(url: String): Either[String, Path] =
    try {
      val bytes = http.getBytes(url)
      if (bytes.isEmpty) Left(PosterFailure.EmptyBody)
      else if (bytes.length > maxBytes) Left(PosterFailure.TooLarge)
      else Right(Files.write(Files.createTempFile("poster-", ".img"), bytes))
    } catch {
      case e: tools.HttpStatusException => Left(e.code / 100 match {
        case 4 => PosterFailure.Http4xx
        case 5 => PosterFailure.Http5xx
        case _ => PosterFailure.HttpOther
      })
      case _: java.net.http.HttpTimeoutException => Left(PosterFailure.Timeout)
      case _: Exception                          => Left(PosterFailure.Network)
    }
}

/** [[PosterDownload]] for a PAID route that remembers each failed poster for a while and answers
 *  it from the memory, without the request: a Multikino poster that 403s through the residential
 *  proxy falls back to Zyte, billed per request, and the daily posterless backfill asked for the
 *  same dead poster every day. A refusal (4xx) is remembered for [[RememberedFailurePosterDownload.RefusedFor]],
 *  any other failure for [[RememberedFailurePosterDownload.FailedFor]]; a success forgets it.
 *
 *  The memory is one empty file per failed URL in `dir` (named by the URL's hash, its reason in
 *  the name, its age the file's modified time), so a restart does not forget it and every replica
 *  of the country shares it. An expired entry is removed when next read. */
class RememberedFailurePosterDownload(delegate: PosterDownload, dir: Path, clock: java.time.Clock) extends PosterDownload {
  import RememberedFailurePosterDownload.*

  // A memory that cannot be READ is not "nothing remembered": asking the paid route on the
  // strength of it is the cost this class exists to cap. The poster fails this time instead.
  def fetch(url: String): Either[String, Path] =
    Try(remembered(url)).toEither match {
      case Left(_)             => Left(PosterFailure.Network)
      case Right(Some(reason)) => Left(reason)
      case Right(None)         =>
        val result = delegate.fetch(url)
        result match {
          case Left(reason) => remember(url, reason)
          case Right(_)     => forget(url)
        }
        result
    }

  private def entries(url: String): Seq[Path] = {
    import scala.jdk.CollectionConverters.*
    val prefix = tools.Digest.sha256Hex(url).take(16) + "."
    if (!Files.isDirectory(dir)) Nil   // nothing ever failed here
    else Using.resource(Files.list(dir))(_.iterator.asScala.filter(_.getFileName.toString.startsWith(prefix)).toList)
  }

  private def remembered(url: String): Option[String] =
    entries(url).flatMap { entry =>
      val reason = entry.getFileName.toString.dropWhile(_ != '.').drop(1)
      val alive  = Try(Files.getLastModifiedTime(entry).toInstant).toOption
        .exists(at => clock.instant().isBefore(at.plusMillis(forReason(reason).toMillis)))
      if (!alive) Try(Files.deleteIfExists(entry))
      Option.when(alive)(reason)
    }.headOption

  private def remember(url: String, reason: String): Unit = Try {
    forget(url)
    Files.createDirectories(dir)
    val entry = dir.resolve(s"${tools.Digest.sha256Hex(url).take(16)}.$reason")
    Files.write(entry, Array.emptyByteArray)
    Files.setLastModifiedTime(entry, java.nio.file.attribute.FileTime.from(clock.instant()))
  }

  private def forget(url: String): Unit = entries(url).foreach(entry => Try(Files.deleteIfExists(entry)))
}

object RememberedFailurePosterDownload {
  import scala.concurrent.duration.*
  /** A poster the origin refused: it will not start working tomorrow. */
  val RefusedFor: FiniteDuration = 14.days
  /** Any other failure (a timeout, a 5xx, the proxy): long enough that the daily backfill skips it. */
  val FailedFor: FiniteDuration  = 3.days

  private def forReason(reason: String): FiniteDuration = if (reason == PosterFailure.Http4xx) RefusedFor else FailedFor
}

/** [[PosterDownload]] over the JDK client: a generous connect budget (some cinema origins take
 *  ~6-7s to a cold TLS connect, past the scrapers' tight 5s), a bounded request time, and a byte
 *  cap enforced while streaming, so a multi-hundred-megabyte "poster" is abandoned at the cap
 *  rather than written out. */
class HttpPosterDownload(maxBytes: Long = PosterPipeline.MaxDownloadBytes,
                         timeout: Duration = Duration.ofSeconds(20)) extends PosterDownload with Logging {
  private val client = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .connectTimeout(Duration.ofSeconds(15))
    .sslContext(TlsTrust.augmentedContext)
    .build()

  def fetch(url: String): Either[String, Path] =
    try {
      val uri = URI.create(url)
      // The poster's own origin as the Referer, as a browser on the cinema's site would send:
      // hotlink rules refuse a request without one (biletyna.pl: 403 with none, 200 with any).
      val request = HttpRequest.newBuilder().uri(uri).timeout(timeout)
        .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
        .header("Referer", s"${uri.getScheme}://${uri.getRawAuthority}/")
        .GET().build()
      val response = client.send(request, HttpResponse.BodyHandlers.ofInputStream())
      Using.resource(response.body()) { body =>
        response.statusCode() / 100 match {
          case 2 => PosterPipeline.copyCapped(body, maxBytes)
          case 4 => Left(PosterFailure.Http4xx)
          case 5 => Left(PosterFailure.Http5xx)
          case _ => Left(PosterFailure.HttpOther)
        }
      }
    } catch {
      case _: java.net.http.HttpTimeoutException => Left(PosterFailure.Timeout)
      case _: Exception                          => Left(PosterFailure.Network)
    }
}

/** Turns a downloaded poster into the card's 420×630 poster slot. */
trait PosterShrinker {
  /** `file` decoded, cover-scaled and cropped to the slot, or the [[PosterFailure]] why not. */
  def coverSlot(file: Path): Either[String, BufferedImage]
}

/**
 * libvips (`vips thumbnail`) in a memory-capped subprocess; the bounded JDK decode only where vips
 * is absent or cannot read the format at all.
 *
 * THE CHILD SHARES THE WORKER CONTAINER'S MEMORY CGROUP, whose JVM already sits at 700-850 MB of a
 * 960 MiB limit (PL/UK; ES 896 MiB). So a poster may cost the child at most [[PosterPipeline.DecodeMemoryCapMb]]
 * of address space (`ulimit -v`, one malloc arena, no core dump): an overrun kills the child —
 * libjpeg aborts "Insufficient memory" — and never the container. Measured in the worker image
 * (Debian libvips 8.18, 2026-09-24): a 780-wide TMDB poster needs ~160 MB of address space and ~45
 * MB resident; a 96-megapixel baseline JPEG 256 MB, a 96-megapixel progressive one more than 1.5 GB
 * (360 MB resident). Vips streams a baseline JPEG, but a PROGRESSIVE one holds every DCT
 * coefficient of the whole image to its last scan, so its header ([[JpegHeader.coefficientBytes]])
 * says in advance whether it can fit: over [[PosterPipeline.ProgressiveCoefficientBudget]] it is
 * refused without spawning anything, and the caller tries the next-smaller rendition. The cap is
 * 256 MB because ordinary posters are progressive at full chroma resolution: measured, a 2764×4096
 * 4:4:4 (68 MB of coefficients) fits 224 MB of address space at 108 MB resident, 3000×4500 4:4:4
 * (81 MB) fits 256 MB at 122 MB; 3500×5250 4:4:4 (110 MB) needs 320 MB — refused. At 192 MB the
 * first of those failed in prod.
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

  def coverSlot(file: Path): Either[String, BufferedImage] =
    JpegHeader.read(file) match {
      case Some(header) if header.progressive && header.coefficientBytes > PosterPipeline.ProgressiveCoefficientBudget =>
        Left(PosterFailure.ProgressiveEstimate)
      case _ => gate.withPermit(binary.fold(javaDecode(file))(vips(_, file)))
    }

  private def vips(bin: String, file: Path): Either[String, BufferedImage] = {
    val out = Files.createTempFile("poster-slot-", ".png")
    val log = Files.createTempFile("poster-vips-", ".log")
    try {
      // The cap FAILS CLOSED: a Linux shell that cannot set it runs no child. macOS never honours
      // `ulimit -v`, so a developer's machine runs uncapped (time limit and gate only).
      val script = s"""ulimit -c 0; ulimit -v ${memoryCapMb * 1024} 2>/dev/null || [ "$$(uname)" = Darwin ] || exit 97; exec "$$0" thumbnail "$$1" "$$2" ${OgCardRenderer.PosterSlotWidth} --height ${OgCardRenderer.PosterSlotHeight} --crop centre"""
      val process = new ProcessBuilder("/bin/sh", "-c", script, bin, file.toString, out.toString)
        .redirectErrorStream(true).redirectOutput(log.toFile)
      process.environment().put("VIPS_CONCURRENCY", "1")
      process.environment().put("MALLOC_ARENA_MAX", "1")
      val child = process.start()
      if (!child.waitFor(timeoutMillis, TimeUnit.MILLISECONDS)) {
        child.destroyForcibly()
        Left(PosterFailure.Timeout)
      } else if (child.exitValue() != 0) {
        // vips reads JPEG, PNG, WebP, GIF, TIFF, HEIF: for any of those a failure is a real one (the
        // cap among them). Only a file none of those is goes to the JDK decoder.
        if (VipsPosterShrinker.knownFormat(file)) Left(VipsPosterShrinker.failure(child.exitValue(), Try(Files.readString(log)).getOrElse("")))
        else javaDecode(file)
      } else Option(ImageIO.read(out.toFile)).map(OgCardRenderer.coverSlot).toRight(PosterFailure.DecodeError)   // opaque RGB, whatever vips wrote
    } catch { case _: Exception => Left(PosterFailure.DecodeError) }
    finally { Files.deleteIfExists(out); Files.deleteIfExists(log) }
  }

  /** The bounded JDK decode (pixel cap + subsampled read). */
  private def javaDecode(file: Path): Either[String, BufferedImage] =
    Try(PosterDecode.fromFile(file.toFile)).toOption.flatten.map(OgCardRenderer.coverSlot).toRight(PosterFailure.DecodeError)
}

object VipsPosterShrinker {
  /** The `vips` binary on the PATH, if any. */
  def locate(): Option[String] =
    sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparator).iterator
      .map(dir => Path.of(dir, "vips")).find(Files.isExecutable).map(_.toString)

  /** Why a vips child for a known image format failed: the cap (the shell could not set it — exit
   *  97 — or libjpeg / glib ran out of memory, which aborts on a signal or says so), else the file. */
  private[sharecards] def failure(exit: Int, output: String): String =
    if (exit == 97 || exit > 128 || output.toLowerCase.contains("memory")) PosterFailure.VipsCap else PosterFailure.DecodeError

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
  val DecodeMemoryCapMb: Long = tools.Env.positiveLong("KINOWO_SHARE_CARD_DECODE_MEMORY_MB", 256L)

  /** The largest progressive JPEG coefficient buffer sent to vips: the cap less the ~170 MB of
   *  address space vips needs around the buffer (measured under a `ulimit -v`: 68 MB of coefficients
   *  fit 224 MB, 81 MB fit 256 MB, 110 MB needed 320 MB). 86 MB at the 256 MB cap: an 11 MP poster
   *  at 4:4:4 is 68 MB, a 2000×3000 4:2:0 one 18 MB; 96 megapixels is 290-580 MB. */
  val ProgressiveCoefficientBudget: Long = (DecodeMemoryCapMb - 170L).max(16L) * 1024 * 1024

  /** JPEG at 0.95 for the poster cache, not PNG: measured on two real 420×630 TMDB slots, PNG
   *  stored 520-620 KB each and q95 116-152 KB — at ~2,000 posters a country, PNG alone would
   *  fill the whole 1 GiB budget. At 0.95 the re-encode under the card's own 0.85 is invisible. */
  val PosterQuality = 0.95f

  /** Stream `in` into a temp file, abandoning it past `maxBytes`. */
  private[sharecards] def copyCapped(in: InputStream, maxBytes: Long): Either[String, Path] = {
    val file = Files.createTempFile("poster-", ".img")
    val size = Try {
      Using.resource(Files.newOutputStream(file)) { out =>
        val buffer = new Array[Byte](64 * 1024)
        var total  = 0L
        var read   = in.read(buffer)
        while (read >= 0 && total <= maxBytes) {
          total += read
          if (total <= maxBytes) out.write(buffer, 0, read)
          read = in.read(buffer)
        }
        total
      }
    }
    val kept = size match {
      case scala.util.Success(total) if total > maxBytes => Left(PosterFailure.TooLarge)
      case scala.util.Success(0L)                        => Left(PosterFailure.EmptyBody)
      case scala.util.Success(_)                         => Right(file)
      case scala.util.Failure(_)                         => Left(PosterFailure.Network)
    }
    if (kept.isLeft) Files.deleteIfExists(file)
    kept
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
      writer.write(null, new IIOImage(OgCardRenderer.opaque(slot), null, null), params)
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
   *  None when none can be had. `retry` marks the re-try of a card drawn without its poster. */
  def load(filmId: String, candidates: Seq[String], retry: Boolean = false): Option[(String, BufferedImage)] =
    cached(filmId, candidates) match {
      case hit @ Some(_) => metrics.posterCache(hit = true); hit
      case None =>
        metrics.posterCache(hit = false)
        val loaded = candidates.iterator.flatMap(url => fetchAndCache(filmId, url).map(url -> _)).nextOption()
        if (candidates.nonEmpty) metrics.posterLoad(ok = loaded.isDefined, retry)
        loaded
    }

  private def cached(filmId: String, candidates: Seq[String]): Option[(String, BufferedImage)] = {
    val path = store.posterPath(filmId)
    store.version(path).flatMap(key => candidates.find(ShareCardPosters.key(_) == key))
      .flatMap(url => Try(Option(ImageIO.read(path.toFile))).toOption.flatten.map(url -> _))
  }

  /** `url`'s poster — the first of its renditions that downloads, shrinks AND encodes — cached, or
   *  None. Any failure on the way, a thrown one included, only moves on to the next rendition (and
   *  the caller to the next candidate): one bad poster must never fail the render. */
  private def fetchAndCache(filmId: String, url: String): Option[BufferedImage] = {
    val attempts = PosterRendition.candidates(url).iterator.map { rendition =>
      download.fetch(rendition).flatMap { file =>
        try shrinker.coverSlot(file).map(image => image -> PosterPipeline.encodePoster(image))
        catch { case _: Exception => Left(PosterFailure.EncodeError) }
        finally Files.deleteIfExists(file)
      }
    }
    // The first rendition that worked, else the last one's reason.
    var outcome: Either[String, (BufferedImage, Array[Byte])] = Left(PosterFailure.DecodeError)
    while (attempts.hasNext && outcome.isLeft) outcome = attempts.next()
    outcome match {
      case Right((image, bytes)) =>
        metrics.posterFetch(PosterFailure.None)
        store.writeAtomically(store.posterPath(filmId), bytes, ShareCardPosters.key(url))
        Some(image)
      case Left(reason) =>
        metrics.posterFetch(reason)
        ShareCardPosters.logFailure(logger, filmId, url, reason)
        None
    }
  }
}

object ShareCardPosters {
  private val lastLogged = new java.util.concurrent.atomic.AtomicLong(0L)

  /** One INFO line per failed poster, at most one a second (the rest at DEBUG): the film, the host
   *  and why — what `poster_fetch_total{reason}` counts but cannot name. */
  private[sharecards] def logFailure(logger: play.api.Logger, filmId: String, url: String, reason: String): Unit = {
    val host = Try(URI.create(url).getHost).toOption.flatMap(Option(_)).getOrElse("?")
    val line = s"share card: poster for $filmId from $host failed: $reason"
    val now  = System.nanoTime()
    val last = lastLogged.get()
    if (now - last > 1000000000L && lastLogged.compareAndSet(last, now)) logger.info(line) else logger.debug(line)
  }

  /** A poster's key: 16 hex characters of the SHA-256 of its URL. */
  def key(url: String): String = tools.Digest.sha256Hex(url).take(16)
}
