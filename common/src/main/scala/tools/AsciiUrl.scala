package tools

import java.nio.charset.StandardCharsets.UTF_8

/**
 * A URL every parser accepts: the RFC 3986 form, with each character the
 * grammar does not allow percent-encoded from its UTF-8 bytes.
 *
 * Scraped poster links are whatever a cinema's CMS emitted, and WordPress happily
 * serves `…/Milcząca-przyjaciółka_plakat.jpg` verbatim. Browsers take that in an
 * `<img src>`, Android's `Uri` shrugs, but Swift's `URL` on Linux — and on any
 * iPhone before iOS 17 — refuses it, and a `Codable` model with a `URL` field then
 * fails to decode the WHOLE listing over one film's poster. The API emits the
 * encoded form so the contract is "a URL", not "a string a lenient parser might
 * take".
 *
 * Idempotent: everything already ASCII, `%xx` escapes included, is left alone,
 * so a URL that was valid stays byte-for-byte what it was.
 */
object AsciiUrl {

  /** RFC 3986 unreserved + reserved + `%`: the characters allowed to appear raw anywhere in a URL. */
  private val Allowed: Set[Char] =
    (('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')).toSet ++ "-._~:/?#[]@!$&'()*+,;=%".toSet

  def encode(url: String): String =
    if (url.forall(Allowed)) url
    else {
      val out = new StringBuilder(url.length + 16)
      url.foreach { c =>
        if (Allowed(c)) out.append(c)
        else c.toString.getBytes(UTF_8).foreach(b => out.append(f"%%${b & 0xff}%02X"))
      }
      out.toString
    }
}
