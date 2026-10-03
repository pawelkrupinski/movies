package tools

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

/** `application/x-www-form-urlencoded` UTF-8 encoding — exactly what
 *  `URLEncoder.encode(text, UTF_8)` returns — appended to a builder the caller is
 *  already filling, rather than through the encoder's own builder, char buffer and
 *  byte buffer per call. The encoder escapes text in runs of chars it does not pass
 *  through; an all-ASCII run is escaped here, and any other is handed to `URLEncoder`
 *  whole — the same run it would cut — so its UTF-8 (surrogate pairs, what a lone half
 *  becomes) is the encoder's own. */
object FormEncoding {

  private val Hex = "0123456789ABCDEF"

  /** What the encoder does not escape: written as itself, or a space as `+`. */
  private def passesThrough(c: Char): Boolean =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ||
      c == '-' || c == '_' || c == '.' || c == '*' || c == ' '

  /** `text` from `from` on, encoded, onto `out`. */
  def append(text: String, from: Int, out: java.lang.StringBuilder): java.lang.StringBuilder = {
    var i = from
    while (i < text.length) {
      val c = text.charAt(i)
      if (c == ' ') { out.append('+'); i += 1 }
      else if (passesThrough(c)) { out.append(c); i += 1 }
      else {
        var end = i + 1
        var ascii = c < 0x80
        while (end < text.length && !passesThrough(text.charAt(end))) { ascii &&= text.charAt(end) < 0x80; end += 1 }
        if (ascii)
          while (i < end) {
            val escaped = text.charAt(i)
            out.append('%').append(Hex.charAt(escaped >> 4)).append(Hex.charAt(escaped & 0xF))
            i += 1
          }
        else { out.append(URLEncoder.encode(text.substring(i, end), UTF_8)); i = end }
      }
    }
    out
  }
}
