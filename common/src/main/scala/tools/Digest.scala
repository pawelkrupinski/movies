package tools

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Content fingerprints. SHA-1 is plenty for "did these inputs change" and short
 *  enough to sit on a document; nothing here is a security boundary. */
object Digest {
  def sha1Hex(s: String): String = hex("SHA-1", s)

  /** SHA-256, for the share-card store's versions (a card's inputs, a poster's URL). */
  def sha256Hex(s: String): String = hex("SHA-256", s)

  /** SHA-1 over the compiled bytes of `classes`: what changes when any of their code does. A class
   *  whose bytes cannot be read (no class file on the classpath) counts by its name alone. */
  def classesHex(classes: Seq[Class[?]]): String = {
    val digest = MessageDigest.getInstance("SHA-1")
    classes.foreach { cls =>
      digest.update(cls.getName.getBytes(StandardCharsets.UTF_8))
      Option(cls.getResourceAsStream(s"/${cls.getName.replace('.', '/')}.class")).foreach { in =>
        try digest.update(in.readAllBytes()) finally in.close()
      }
    }
    hexFormat.formatHex(digest.digest())
  }

  // `HexFormat`, not `f"$b%02x"` per byte: that was a `String.format` — a parse of the pattern and
  // a Formatter — for every byte of every digest, and `FilmId` derives each film's id through
  // here: 11% of a busy US worker's allocation (JFR, 2026-09-30). Same lowercase hex.
  private val hexFormat = java.util.HexFormat.of()

  private def hex(algorithm: String, s: String): String =
    hexFormat.formatHex(MessageDigest.getInstance(algorithm).digest(s.getBytes(StandardCharsets.UTF_8)))
}
