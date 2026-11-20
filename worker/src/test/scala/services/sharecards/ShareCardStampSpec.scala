package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ShareCardTestKit.*

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The stamp a card carries in a JPEG comment right after its start-of-image: what it was drawn as (`version`),
 *  when its film was first published and when its inputs were asked for — and nothing read from a file without one. */
class ShareCardStampSpec extends AnyFlatSpec with Matchers {

  "A stamped card" should "read back the version, publication and ask it was written with" in {
    val store = tempStore()
    val card  = store.cardPath("film-1")
    store.writeAtomically(card, posterJpeg, "v7", published = Some(T0), asked = Some(T0.plusMillis(1234)))
    store.version(card) shouldBe Some("v7")
    store.published(card) shouldBe Some(T0)
    store.asked(card) shouldBe Some(T0.plusMillis(1234))
    Files.readAllBytes(card).takeRight(posterJpeg.length - 2) shouldBe posterJpeg.drop(2)
  }

  it should "leave out what it was not written with" in {
    val store = tempStore()
    val card  = store.cardPath("film-2")
    store.writeAtomically(card, posterJpeg, "v1")
    (store.version(card), store.published(card), store.asked(card)) shouldBe (Some("v1"), None, None)
  }

  "A file with no stamp" should "read as none: an absent file, a plain JPEG, another program's comment, no JPEG" in {
    val store = tempStore()
    val path  = store.cardPath("film-3")
    store.version(path) shouldBe None
    Files.write(path, posterJpeg)
    store.version(path) shouldBe None
    val comment = "other:v=9".getBytes(StandardCharsets.US_ASCII)
    val length  = comment.length + 2
    Files.write(path, Array[Byte](0xff.toByte, 0xd8.toByte, 0xff.toByte, 0xfe.toByte, (length >> 8).toByte, length.toByte) ++
      comment ++ posterJpeg.drop(2))
    store.version(path) shouldBe None
    Files.write(path, "GIF89a".getBytes(StandardCharsets.US_ASCII))
    store.version(path) shouldBe None
  }
}
