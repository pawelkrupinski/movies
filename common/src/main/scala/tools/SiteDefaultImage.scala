package tools

import java.util.Locale

/** Does an image URL have the shape of a SITE-WIDE default rather than one film's poster? Many venues set one
  * og:image on every page — Kinoteka's `kinoteka-opengraph.png`, BOK's `logo-bok_…jpg`, the bilety24 venue sites'
  * `PAN-BILET_…svg`, Kino Muranów's `kino_share.png` — and a logo taken as a film's poster is worse than none: it
  * feeds the identity's poster vote and veto, and shows as the venue's poster on a review card. Judged by the file
  * NAME only (a path segment such as bilety24's `dealer-default/` says nothing), as whole words, so
  * "plakat-bez-logotypow" stays. bilety24's stand-in for a venue that uploaded no image — `image.bilety24.pl/not-found`,
  * or a file with no name (`…/1410/.png`) — is no poster either. */
object SiteDefaultImage {
  private val Words =
    Seq("logo", "opengraph", "favicon", "placeholder", "zaslepka", "share", "og-image", "no-photo", "nophoto", "page-thumbnail", "not-found")

  def apply(url: String): Boolean = {
    val file  = url.takeWhile(c => c != '?' && c != '#').split('/').lastOption.getOrElse("").toLowerCase(Locale.ROOT)
    val words = "-" + file.replaceAll("[^a-z]+", "-") + "-"
    file.endsWith(".svg") || file.startsWith(".") || Words.exists(word => words.contains(s"-$word-"))
  }
}
