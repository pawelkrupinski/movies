package tools

import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream

/** Decompresses a gzipped byte payload, looping while the output still
 *  carries the gzip magic prefix `1F 8B`. TMDB (or its CDN) sometimes
 *  double-wraps responses: the body is `gzip(gzip(json))` with a single
 *  `Content-Encoding: gzip` header, so one round of `GZIPInputStream`
 *  succeeds but the JSON parser is still handed gzip bytes. Looping
 *  unwraps as many layers as we find; a cap of 4 keeps the loop
 *  bounded in the (impossible) pathological case.
 *
 *  Non-gzip input passes through unchanged. A mid-stream decompress
 *  error returns the last successfully-decoded layer so the caller
 *  still sees a String it can investigate. */
object Gunzip {

  private val MaxDepth = 4

  def decode(bytes: Array[Byte]): Array[Byte] = decode(bytes, 0)

  private def decode(bytes: Array[Byte], depth: Int): Array[Byte] = {
    if (depth >= MaxDepth || !startsWithGzipMagic(bytes)) bytes
    else
      try {
        val gz  = new GZIPInputStream(new ByteArrayInputStream(bytes))
        val out = try gz.readAllBytes() finally gz.close()
        decode(out, depth + 1)
      } catch {
        case _: Throwable => bytes
      }
  }

  def startsWithGzipMagic(bytes: Array[Byte]): Boolean =
    bytes.length >= 2 && (bytes(0) & 0xFF) == 0x1F && (bytes(1) & 0xFF) == 0x8B
}
