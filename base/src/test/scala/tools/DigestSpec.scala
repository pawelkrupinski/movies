package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The digests are stored — a film's id, a share card's version — so their text must never move. */
class DigestSpec extends AnyFlatSpec with Matchers {
  "Digest" should "give the standard lowercase SHA-1 and SHA-256 hex" in {
    Digest.sha1Hex("abc") shouldBe "a9993e364706816aba3e25717850c26c9cd0d89d"
    Digest.sha256Hex("abc") shouldBe "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    Digest.sha1Hex("") shouldBe "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    Digest.sha1Hex("Łódź|2026") shouldBe java.security.MessageDigest.getInstance("SHA-1")
      .digest("Łódź|2026".getBytes("UTF-8")).map(b => f"${b & 0xff}%02x").mkString
  }

  "classesHex" should "fingerprint a class by its compiled bytes, not its name alone" in {
    val cls = classOf[DigestSpec]
    Digest.classesHex(Seq(cls)) shouldBe Digest.classesHex(Seq(cls))
    Digest.classesHex(Seq(cls)) should not be Digest.sha1Hex(cls.getName)
    Digest.classesHex(Seq(cls, classOf[Digest.type])) should not be Digest.classesHex(Seq(cls))
  }
}
