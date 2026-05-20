package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.Gunzip

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.zip.GZIPOutputStream

class GunzipSpec extends AnyFlatSpec with Matchers {

  private def gzip(bytes: Array[Byte]): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val gz  = new GZIPOutputStream(bos)
    try gz.write(bytes) finally gz.close()
    bos.toByteArray
  }

  private val Json = """{"id":42,"title":"hello"}"""

  "Gunzip.decode" should "pass non-gzip bytes through unchanged" in {
    val raw = Json.getBytes(StandardCharsets.UTF_8)
    Gunzip.decode(raw) shouldBe raw
  }

  it should "decode a single-gzipped payload" in {
    val original = Json.getBytes(StandardCharsets.UTF_8)
    val once     = gzip(original)
    once.head & 0xFF shouldBe 0x1F
    Gunzip.decode(once) shouldBe original
  }

  // Reproduces the prod TMDB symptom: response arrives with one
  // Content-Encoding: gzip header but the body is actually gzipped
  // twice (origin gzip + CDN gzip). A single GZIPInputStream pass
  // succeeds but the output is still gzip-magic-prefixed bytes.
  it should "unwrap a double-gzipped payload" in {
    val original = Json.getBytes(StandardCharsets.UTF_8)
    val twice    = gzip(gzip(original))
    twice.head & 0xFF shouldBe 0x1F
    Gunzip.decode(twice) shouldBe original
  }

  it should "return the input untouched if the gzip magic is present but the stream is corrupt" in {
    val bogus = Array[Byte](0x1F.toByte, 0x8B.toByte, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00)
    Gunzip.decode(bogus) shouldBe bogus
  }

  it should "handle an empty array" in {
    Gunzip.decode(Array.emptyByteArray) shouldBe Array.emptyByteArray
  }
}
