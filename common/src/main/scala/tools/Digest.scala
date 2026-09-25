package tools

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Content fingerprints. SHA-1 is plenty for "did these inputs change" and short
 *  enough to sit on a document; nothing here is a security boundary. */
object Digest {
  def sha1Hex(s: String): String = hex("SHA-1", s)

  /** SHA-256, for the share-card store's versions (a card's inputs, a poster's URL). */
  def sha256Hex(s: String): String = hex("SHA-256", s)

  private def hex(algorithm: String, s: String): String =
    MessageDigest.getInstance(algorithm).digest(s.getBytes(StandardCharsets.UTF_8))
      .map(b => f"${b & 0xff}%02x").mkString
}
