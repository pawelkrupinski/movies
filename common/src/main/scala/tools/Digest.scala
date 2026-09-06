package tools

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Content fingerprints. SHA-1 is plenty for "did these inputs change" and short
 *  enough to sit on a document; nothing here is a security boundary. */
object Digest {
  def sha1Hex(s: String): String =
    MessageDigest.getInstance("SHA-1").digest(s.getBytes(StandardCharsets.UTF_8))
      .map(b => f"${b & 0xff}%02x").mkString
}
