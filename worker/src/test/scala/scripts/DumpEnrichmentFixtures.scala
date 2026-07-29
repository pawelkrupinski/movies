package scripts

import models.Country
import tools.{CachedResponse, Env, MongoEnrichmentCacheStore}

import java.net.URI
import java.nio.file.{Files, Path, Paths}

/**
 * Convert the shared enrichment cache into on-disk fixture files, in the layout
 * `FakeHttpFetch` already replays.
 *
 * This is a pure TRANSFORMATION, which is the point: the convergence legs have
 * already paid for these answers — every live fill is written through to
 * `convergence_test` as they run — so dumping them costs one Mongo read and no
 * third-party calls at all. That is why it can run after every convergence, on any
 * conclusion, without touching TMDB, IMDb, Filmweb, RT or Metacritic.
 *
 * Reconstructing the fixture path from a cache key works because the two schemes
 * were built to agree on the parts that matter. A cache key is
 * `METHOD <credential-masked url>[ <body-hash>]`, and the fixture name is
 * `<host>/<path>.<query-fingerprint>[.<body-hash>]` where the fingerprint already
 * strips credentials — so the masked URL produces the same fingerprint the real one
 * would, and the POST body hash the cache stored is the same
 * `body.hashCode.toHexString` the recorder appends.
 *
 * `foldYear = false` throughout: the year-scoped and yearless TMDB searches are
 * different answers (0 results vs 16 on the same title) and `TmdbClient` depends on
 * the difference, so they must not collapse onto one file. The `FakeHttpFetch` that
 * reads these back has to be constructed the same way.
 *
 * Run with:
 *   KINOWO_COUNTRY=pl KINOWO_CONVERGENCE_CACHE_URI=... \
 *   KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES=enrichment-pl \
 *     sbt "worker/Test/runMain scripts.DumpEnrichmentFixtures"
 */
object DumpEnrichmentFixtures {

  def main(args: Array[String]): Unit = {
    val country = args.headOption.flatMap(code => Country.all.find(_.code == code)).getOrElse(Country.fromEnv)
    val uri = Env.get("KINOWO_CONVERGENCE_CACHE_URI").orElse(Env.get("MONGODB_URI")).getOrElse {
      System.err.println("[enrichment] set KINOWO_CONVERGENCE_CACHE_URI (or MONGODB_URI)")
      sys.exit(1)
    }
    val directory = Env.get("KINOWO_CONVERGENCE_ENRICHMENT_FIXTURES").filter(_.nonEmpty)
      .getOrElse(s"enrichment-${country.code}")
    val root = Paths.get(clients.tools.FakeHttpFetch.rootFor(directory))

    val store = MongoEnrichmentCacheStore.open(uri, country)
    try {
      val entries = store.loadAll()
      if (entries.isEmpty) {
        System.err.println(
          s"[enrichment] ${country.displayName}: cache read came back empty — refusing to write an empty " +
          "fixture set, since a leg would then treat every URL as unrecorded and sweep the live services.")
        sys.exit(1)
      }

      var written = 0
      var skipped = 0
      var bytes   = 0L
      entries.foreach { case (key, response) =>
        (pathFor(root, key), bodyOf(response)) match {
          case (Some(path), Some(content)) =>
            Files.createDirectories(path.getParent)
            Files.write(path, content)
            written += 1
            bytes += content.length
          // A cached FAILURE has no body to replay. Left out deliberately: a missing
          // fixture falls through to the live leg, which re-asks and re-decides,
          // whereas inventing a body would fabricate an answer the service never gave.
          case _ => skipped += 1
        }
      }
      println(s"[enrichment] ${country.displayName}: ${entries.size} cached entries -> $written fixture files " +
              f"($skipped skipped as failures/unwritable), ${bytes / 1048576.0}%.1f MB under $root")
    } finally store.close()
  }

  /** `METHOD <masked url>[ <body-hash>]` → the file `FakeHttpFetch` will look for. */
  private[scripts] def pathFor(root: Path, key: String): Option[Path] = {
    val parts = key.split(" ")
    if (parts.length < 2) None
    else {
      val method   = parts(0)
      val url      = parts(1)
      val bodyHash = if (parts.length > 2) Some(parts(2)) else None
      try {
        val uri  = new URI(url)
        val path = uri.getPath.stripPrefix("/")
        // An EMPTY path is legitimate, not a parse failure: IMDb's GraphQL endpoint
        // posts to the root, and `RecordingHttpFetch` writes those as a hidden file
        // inside the host directory (`caching.graphql.imdb.com/.<bodyhash>`), which
        // is exactly what `root.resolve("$host/$path…")` produces below.
        if (uri.getHost == null) None
        else {
          val query = Option(uri.getRawQuery)
            .map(q => s".${clients.tools.RecordingHttpFetch.stableQueryFingerprint(q, foldYear = false)}")
            .getOrElse("")
          val body = bodyHash.map(h => s".$h").getOrElse("")
          // GET and POST land on the same shape; the body hash is what separates the
          // IMDb GraphQL calls that all share one URL.
          val _ = method
          Some(root.resolve(s"${uri.getHost}/$path$query$body"))
        }
      } catch { case _: Throwable => None }
    }
  }

  private[scripts] def bodyOf(response: CachedResponse): Option[Array[Byte]] = response match {
    case CachedResponse.Body(text)    => Some(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
    case CachedResponse.Bytes(base64) => Some(java.util.Base64.getDecoder.decode(base64))
    case _: CachedResponse.Failed     => None
  }
}
