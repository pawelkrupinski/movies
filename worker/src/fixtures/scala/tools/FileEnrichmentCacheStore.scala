package tools

import play.api.Logging

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Try, Using}

/**
 * The enrichment cache as FILES beside the fixture tree, rather than rows in Mongo.
 *
 * It exists because of what the fixture tree cannot remember. `RecordingHttpFetch`
 * writes down what it successfully fetched, so a corpus converges on complete for
 * every URL that ANSWERS — and never for one that doesn't. Roughly half a country's
 * films never resolve to a TMDB id, and each of those still costs three or four
 * Rotten Tomatoes / Metacritic / Filmweb slug guesses that 404. None of them is a
 * success, so none is recorded, so every run re-asks all of them — over a paced,
 * rate-limited, single-threaded chain. That is the whole of a 22-minute Poland leg:
 * a profile of it finds one application thread parked in `RealHttpFetch.getBytes`
 * in every single sample, and the tree five files larger than it started.
 *
 * A remembered 404 is a remembered verdict. That was the Mongo cache's argument for
 * caching failures, and it was right; what was wrong was where it lived — behind a
 * `flyctl proxy` that CI never started, costing 5 seconds a write until three legs
 * were cancelled at the job ceiling. The answers do not need a database. They need
 * to survive from one run to the next, which is what the artifact the tree already
 * travels in does.
 *
 * So this is the same [[EnrichmentCacheStore]] contract, backed by a directory: all
 * the policy — what counts as a hit, when to write through, how a failure is revived
 * — stays above the seam in [[EnrichmentCache]] and [[CachingEnrichmentFetch]], and
 * the Mongo store and this one cannot disagree about any of it.
 */
class FileEnrichmentCacheStore(val root: Path, ttl: FiniteDuration = FileEnrichmentCacheStore.Ttl)
  extends EnrichmentCacheStore with Logging {

  Files.createDirectories(root)

  /**
   * Every unexpired entry on disk.
   *
   * Unlike the Mongo store this needs no paging and no retry: it is a local
   * directory walk, and the failure modes that made the remote read fragile (a
   * proxy that dies mid-scan, a page sized in rows rather than bytes) do not exist
   * here. A file that fails to decode is SKIPPED rather than fatal — a half-written
   * entry from a killed run should cost one live fetch, not the whole preload.
   */
  override def loadAll(): Map[String, CachedResponse] = {
    val floor   = System.currentTimeMillis() - ttl.toMillis
    var skipped = 0
    val entries = Using(Files.walk(root)) { walk =>
      walk.iterator().asScala
        .filter(path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(FileEnrichmentCacheStore.Extension))
        .flatMap { path =>
          FileEnrichmentCacheStore.decode(Try(Files.readAllBytes(path)).toOption) match {
            case Some((key, fetchedAt, response)) if fetchedAt >= floor => Some(key -> response)
            case Some(_)                                               => None            // expired
            case None                                                  => skipped += 1; None
          }
        }.toMap
    }.getOrElse {
      logger.warn(s"Enrichment cache directory $root could not be read — the run will fetch live")
      Map.empty[String, CachedResponse]
    }
    if (skipped > 0) logger.warn(s"Enrichment cache: skipped $skipped undecodable entr(ies) under $root")
    entries
  }

  /** Written via a temporary file and an atomic move, so a run killed mid-write
   *  leaves either the old entry or the new one — never a truncated file the next
   *  preload has to guess about. */
  override def put(key: String, response: CachedResponse): Unit = {
    val target = pathFor(key)
    Files.createDirectories(target.getParent)
    val temporary = Files.createTempFile(target.getParent, "entry", ".tmp")
    try {
      Files.write(temporary, FileEnrichmentCacheStore.encode(key, System.currentTimeMillis(), response))
      Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      ()
    } catch {
      case failure: Throwable => Files.deleteIfExists(temporary); throw failure
    }
  }

  /** Hashed, two levels deep. The key is a whole URL — far too long for a filename
   *  on any filesystem, and full of characters that are path separators on one of
   *  them — and a country's cache runs to tens of thousands of entries, which is
   *  more than one directory should hold. */
  private def pathFor(key: String): Path = {
    val hash = FileEnrichmentCacheStore.hash(key)
    root.resolve(hash.substring(0, 2)).resolve(hash.substring(2, 4))
      .resolve(s"${hash.substring(4)}${FileEnrichmentCacheStore.Extension}")
  }
}

object FileEnrichmentCacheStore {

  val Extension = ".entry"

  /** Where the cache sits relative to a country's fixture tree: INSIDE it, so the
   *  tarball the convergence leg already publishes carries the cache with it and
   *  neither has to be restored separately from the other. Dot-prefixed so it can't
   *  be mistaken for a recorded host directory. */
  def beside(fixtureDirectory: String): Path =
    Paths.get(clients.tools.FakeHttpFetch.rootFor(fixtureDirectory)).resolve(".enrichment-cache")

  /**
   * How long a remembered answer stands in for the live service.
   *
   * A week, where Mongo's was a day. The Mongo cache was written continuously by
   * every run against one shared cluster, so a day bounded how long a wrong answer
   * — a rate-limited 429 pinned as a verdict — could persist. This one travels in a
   * CI artifact between runs that may be a day apart on the schedule alone, and a
   * TTL that expires between runs is a TTL that never hits. A week keeps the legs
   * warm while still healing a 404 that became a 200, and the recorded successes it
   * sits beside have no expiry at all.
   */
  val Ttl: FiniteDuration = 7.days

  private def hash(key: String): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
      .digest(key.getBytes(StandardCharsets.UTF_8))
    digest.map(byte => f"${byte & 0xff}%02x").mkString
  }

  /**
   * `key \n kind \n fetchedAt \n status \n method \n payload`, gzipped.
   *
   * The KEY is stored in the entry rather than inferred from the filename, which is
   * a one-way hash — so a cache directory is self-describing and `loadAll` needs no
   * side index. Bodies are gzipped for the same reason the Mongo rows were: these
   * are HTML and JSON, a Metacritic page is 750 KB, and they compress ~10-20x.
   */
  private[tools] def encode(key: String, fetchedAt: Long, response: CachedResponse): Array[Byte] = {
    val (kind, status, method, payload) = response match {
      case CachedResponse.Body(text)                    => ("body",   "", "",     text)
      case CachedResponse.Bytes(base64)                 => ("bytes",  "", "",     base64)
      case CachedResponse.Failed(code, verb, message)   => ("failed", code.fold("")(_.toString), verb, message)
    }
    gzip(s"$key\n$kind\n$fetchedAt\n$status\n$method\n$payload")
  }

  private[tools] def decode(bytes: Option[Array[Byte]]): Option[(String, Long, CachedResponse)] =
    bytes.flatMap(raw => Try(gunzip(raw)).toOption).flatMap { text =>
      // `6` so a payload containing newlines — every HTML body does — stays whole.
      text.split("\n", 6) match {
        case Array(key, kind, fetchedAt, status, method, payload) =>
          fetchedAt.toLongOption.flatMap { at =>
            val response = kind match {
              case "body"   => Some(CachedResponse.Body(payload))
              case "bytes"  => Some(CachedResponse.Bytes(payload))
              case "failed" => Some(CachedResponse.Failed(status.toIntOption, method, payload))
              case _        => None
            }
            response.map(r => (key, at, r))
          }
        case _ => None
      }
    }

  private[tools] def gzip(text: String): Array[Byte] = {
    val bytes = new java.io.ByteArrayOutputStream()
    val out   = new java.util.zip.GZIPOutputStream(bytes)
    try out.write(text.getBytes(StandardCharsets.UTF_8)) finally out.close()
    bytes.toByteArray
  }

  private[tools] def gunzip(data: Array[Byte]): String = {
    val in = new java.util.zip.GZIPInputStream(new java.io.ByteArrayInputStream(data))
    try new String(in.readAllBytes(), StandardCharsets.UTF_8) finally in.close()
  }
}
